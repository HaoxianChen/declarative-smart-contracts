package synthesis

import datalog.{Program, Rule}
import imp.ImperativeTranslator
import verification.Verifier
import com.microsoft.z3._
import verification.Prove

case class BoundedModelChecker() {
  /** Input:
   *    - A datalog program
   *    - A set of rules that are query violation instance
   *  Output:
   *    - Result: Boolean
   *    - Counter example if result is false. */
  def check(program: Program, violationRules: Set[Rule], bound: Int): (Boolean, Option[Trace]) = {
    // 1) Setup verifier + transition system + properties
    val (verifier, ts, ctx, properties) = setupVerifier(program)

    // 2) Collect renamable symbols (state vars + other top-level consts)
    val (stateVars, otherConsts) = collectRenamables(ts, properties, ctx)

    // 3) Unroll and check bounds (start at k=1 to require at least one transition)
    for (k <- 0 to bound) {
      println(s"[BMC] Checking bound = $k")
      val pathConstraint = buildPathConstraint(ts, k, stateVars, otherConsts, ctx)
      // check each property at this bound
      for ((rule, prop) <- properties) {
        // val violation = ctx.mkNot(prop)
        val violation = prop
        val violationAtK = renameForStep(violation, k, stateVars, otherConsts, ctx).asInstanceOf[BoolExpr]
        checkPropertyAtBound(rule, violationAtK, pathConstraint, k, program, stateVars, otherConsts, ctx) match {
          case Some(trace) => return (false, Some(trace))
          case None => // continue
        }
      }
    }

    (true, None)
  }

  // --- helpers ---
  private def setupVerifier(program: Program): (Verifier, verification.TransitionSystem, Context, Seq[(Rule, BoolExpr)]) = {
    val materializedRelations: Set[datalog.Relation] = Set()
    val impTranslator = new ImperativeTranslator(
      program,
      materializedRelations,
      isInstrument = true,
      monitorViolations = false,
      arithmeticOptimization = true,
      enableProjection = true
    )
    val imperative = impTranslator.translate()
    val verifier = new Verifier(program, imperative)
    val ts = verifier.getTransitionSystem()

    println(s"[BMC] Transition system ready for program '${program.name}'")
    val ctx = ts.ctx
    val properties: Seq[(Rule, BoolExpr)] = program.violationRules.toSeq.map { vr =>
      // val prop = verifier.getProperty(ctx, vr)
      val prop = verifier.getViolationCheck(ctx, vr)
      (vr, prop)
    }
    properties.foreach { case (vr, prop) => println(s"[BMC] Property for violation rule '${vr.head.relation.name}': $prop") }
    (verifier, ts, ctx, properties)
  }

  private def collectRenamables(ts: verification.TransitionSystem, properties: Seq[(Rule, BoolExpr)], ctx: Context)
  : (Seq[(Expr[_], Expr[_])], Set[Expr[_]]) = {
    val initConsts = collectConstsFrom(ts.getInit(), ctx)
    val trConsts = collectConstsFrom(ts.getTr(), ctx)
    val propConsts = properties.flatMap { case (_, p) => collectConstsFrom(p, ctx) }.toSet
    val stateVars: Seq[(Expr[_], Expr[_])] = ts.getVariables().toSeq
    val stateConsts = stateVars.flatMap { case (a, b) => Seq(a, b) }.toSet
    val otherConsts: Set[Expr[_]] = (initConsts ++ trConsts ++ propConsts).filterNot(stateConsts.contains)
    (stateVars, otherConsts)
  }

  private def collectConstsFrom(root: Expr[_], ctx: Context): Set[Expr[_]] = {
    import scala.collection.mutable
    val acc = mutable.HashSet.empty[Expr[_]]
    val visited = mutable.HashSet.empty[Expr[_]]
    val stack = mutable.Stack[(Expr[_], Int)]((root, 0))
    val MAX_DEPTH = 200
    val MAX_ARGS = 200
    while (stack.nonEmpty) {
      val (x, depth) = stack.pop()
      if (!visited.contains(x)) {
        visited += x
        try {
          if (x.isConst) {
            val name = x.getSExpr
            if (name != "true" && name != "false") acc += x
          } else if (x.isQuantifier) {
            if (depth + 1 <= MAX_DEPTH) stack.push((x.asInstanceOf[Quantifier].getBody, depth + 1))
          } else {
            if (depth < MAX_DEPTH) {
              val args: Array[Expr[_]] = try { x.getArgs } catch { case _: Throwable => Array.empty[Expr[_]] }
              if (args != null && args.length <= MAX_ARGS) {
                var i = args.length - 1
                while (i >= 0) { stack.push((args(i), depth + 1)); i -= 1 }
              }
            }
          }
        } catch { case _: Throwable => /* ignore malformed nodes */ }
      }
    }
    acc.toSet
  }

  private def versionedName(base: String, v: Int): String = if (v == 0) base else s"${base}_v${v}"
  private def otherConstName(orig: String, step: Int): String = s"${orig}_s${step}"

  private def renameForStep(e: Expr[_], step: Int, stateVars: Seq[(Expr[_], Expr[_])], otherConsts: Set[Expr[_]], ctx: Context): Expr[_] = {
    var from = List.empty[Expr[_]]
    var to = List.empty[Expr[_]]
    for ((v_in, v_out) <- stateVars) {
      val base = v_in.getSExpr
      // Use the same per-step naming convention as otherConstName (e.g., base_s0, base_s1)
      val inName = otherConstName(base, step)
      val outName = otherConstName(base, step + 1)
      val inVar = ctx.mkConst(inName, v_in.getSort.asInstanceOf[com.microsoft.z3.Sort])
      val outVar = ctx.mkConst(outName, v_out.getSort.asInstanceOf[com.microsoft.z3.Sort])
      from ::= v_in; to ::= inVar; from ::= v_out; to ::= outVar
    }
    for (c <- otherConsts) {
      val orig = c.getSExpr
      if (orig != "true" && orig != "false") {
        try { val newConst = ctx.mkConst(otherConstName(orig, step), c.getSort.asInstanceOf[com.microsoft.z3.Sort]); from ::= c; to ::= newConst } catch { case _: Throwable => () }
      }
    }
    val fromArr = from.reverse.toArray; val toArr = to.reverse.toArray
    e.substitute(fromArr, toArr)
  }

  private def buildPathConstraint(ts: verification.TransitionSystem, k: Int,
                                  stateVars: Seq[(Expr[_], Expr[_])], otherConsts: Set[Expr[_]], ctx: Context): BoolExpr = {
    val init0 = renameForStep(ts.getInit(), 0, stateVars, otherConsts, ctx).asInstanceOf[BoolExpr]

    val trConjs: Seq[BoolExpr] = (0 until k).map { step =>
      renameForStep(ts.getTr(), step, stateVars, otherConsts, ctx).asInstanceOf[BoolExpr]
    }

    // compose into a single path constraint and return it
    val pathConstraintLocal: BoolExpr = if (trConjs.isEmpty) init0 else ctx.mkAnd((init0 +: trConjs): _*)
    pathConstraintLocal
  }

  private def checkPropertyAtBound(rule: Rule, violationAtK: BoolExpr, pathConstraint: BoolExpr, k: Int,
                                   program: Program, stateVars: Seq[(Expr[_], Expr[_])], otherConsts: Set[Expr[_]], ctx: Context): Option[Trace] = {
    // Dump control: check system property or environment variable
    val dumpEnabled: Boolean = {
      val prop = sys.props.get("bmc.dump.smt")
      val env = sys.env.get("BMC_DUMP_SMT")
      prop.orElse(env).exists(s => s.toLowerCase == "true")
    }

    def dumpSmt2(expr: Expr[_], filename: String): Unit = {
      try {
        val solver = ctx.mkSolver()
        solver.add(expr.asInstanceOf[BoolExpr])
        val smt = solver.toString
        import java.nio.file.{Paths, Files}
        import java.nio.charset.StandardCharsets
        Files.write(Paths.get(filename), smt.getBytes(StandardCharsets.UTF_8))
        println(s"[BMC] Dumped SMT2 to $filename")
      } catch {
        case e: Throwable => println(s"[BMC] Failed to dump SMT2: ${e}")
      }
    }

    val timeouts = Array(20000, 60000, 120000)
    var res: Status = Status.UNKNOWN
    var model: Model = null
    var attempt = 0
    while (attempt < timeouts.length && res != Status.SATISFIABLE && res != Status.UNSATISFIABLE) {
      // Optionally dump the combined constraint to SMT2 before solving
      if (dumpEnabled && attempt == 0) {
        val combined = ctx.mkAnd(pathConstraint, violationAtK)
        val ts = java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss_SSS")
        val stamp = java.time.LocalDateTime.now().format(ts)
        val fname = s"bmc_k${k}_${rule.head.relation.name}_$stamp.smt2"
        dumpSmt2(combined, fname)
      }

      val solver = ctx.mkSolver()
      val p = ctx.mkParams(); p.add("timeout", timeouts(attempt))
      p.add("smt.mbqi", true)
      // if (attempt >= 1) try { p.add("smt.mbqi", true) } catch { case _: Throwable => () }
      solver.setParameters(p)
      solver.add(pathConstraint)
      solver.add(violationAtK)
      res = solver.check()
      if (res == Status.SATISFIABLE) try { model = solver.getModel } catch { case _: Throwable => model = null }
      attempt += 1
    }
    println(s"[BMC] Solver result for rule ${rule.head.relation.name} at bound $k: $res")
    if (res == Status.SATISFIABLE) {
      println(s"[BMC] Counterexample found at bound $k for rule ${rule.head.relation.name}")
      println(model)
      extractTraceFromModel(model, k, ctx, program)
    } else None
  }

  // extract trace helper (keeps previous heuristic behavior)
  private def extractTraceFromModel(model: Model, k: Int, ctx: Context, program: Program): Option[Trace] = {
    try {
      import scala.collection.mutable.ArrayBuffer
      val steps = ArrayBuffer.empty[synthesis.Transaction]
      for (stepIdx <- 0 until k) {
        // Try per-step transaction naming first (transaction_s{step}), fall back to versionedName if not found
        val trNameConstStep = ctx.mkConst(otherConstName("transaction", stepIdx + 1), ctx.mkStringSort())
        val trValExprStep = try { model.eval(trNameConstStep, true) } catch { case _: Throwable => null }
        val trNameStep: String = if (trValExprStep != null) trValExprStep.toString.replaceAll("\"", "") else ""
        val trName = if (trNameStep.nonEmpty) trNameStep else {
          val trNameConstV = ctx.mkConst(versionedName("transaction", stepIdx + 1), ctx.mkStringSort())
          val trValExprV = try { model.eval(trNameConstV, true) } catch { case _: Throwable => null }
          if (trValExprV == null) "" else trValExprV.toString.replaceAll("\"", "")
        }
        val relOpt = program.relations.find(_.name == trName)
        def evalIntConst(name: String): Int = {
          try {
            val c = ctx.mkConst(s"${name}_s${stepIdx}", ctx.getIntSort)
            val v = model.eval(c, true)
            if (v == null) 0 else {
              val s = v.toString
              try { s.toInt } catch { case _: Throwable => 0 }
            }
          } catch { case _: Throwable => 0 }
        }
        val msgSenderVal = evalIntConst("msgSender")
        val msgValueVal = evalIntConst("msgValue")
        val implicitParams = synthesis.ImplicitParameters(msgSenderVal, msgValueVal)
        val txRel = relOpt.getOrElse(program.interfaces.headOption.map(_.relation).getOrElse(program.relations.head))
        val tx = synthesis.Transaction(txRel, List(), implicitParams)
        steps += tx
      }
      Some(synthesis.Trace(steps.toSeq))
    } catch { case _: Throwable => None }
  }

}

object BoundedModelChecker {
  /** Prepare some unit tests here.
   *  - Read the program and violation rules from a file.
   *  - Return counter example when it violates the property.
   * */
  def unitTest1(): Unit = {
    println("BoundedModelChecker.unitTest1: placeholder (no test implemented)")
  }
  
}
