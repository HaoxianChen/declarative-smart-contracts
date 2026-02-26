package synthesis

import datalog.{Constant, Literal, Parameter, Program, Relation, Rule}
import imp.ImperativeTranslator
import verification.{TransitionSystem, Verifier, Z3Helper}
import com.microsoft.z3._
import Verifier.indicatorConstForTransactionTriggerRelation
import imp.SolidityTranslator.transactionRelationPrefix

case class BoundedModelChecker() {
  // cache for per-step substitution arrays and name->Expr map
  private val stepSubstCache = scala.collection.mutable.Map.empty[Int, (Array[Expr[_]], Array[Expr[_]], Map[String, Expr[_]])]

  // get or compute the per-step subst (fromArr, toArr, map) and cache it
  private def getStepSubst(step: Int, stateVars: Seq[(Expr[_], Expr[_])], otherConsts: Set[Expr[_]], ctx: Context): (Array[Expr[_]], Array[Expr[_]], Map[String, Expr[_]]) = {
    stepSubstCache.getOrElseUpdate(step, buildStepSubst(step, stateVars, otherConsts, ctx))
  }

  /**
   * Unified helper for evaluating Z3 expressions in the model.
   * Returns Option[String] for general use, or Option[Int] for integer conversion.
   */
  private def evalModelExpr(model: Model, expr: Expr[_], modelCompletion: Boolean = true): Option[String] = {
    try Option(model.eval(expr, modelCompletion)).map(_.toString) catch { case _: Throwable => None }
  }
  private def evalModelInt(model: Model, expr: Expr[_], modelCompletion: Boolean = true): Option[Int] = {
    evalModelExpr(model, expr, modelCompletion).flatMap(s => try Some(s.toInt) catch { case _: Throwable => None })
  }

  private def triggerIndicator(ctx: Context, program: Program): Map[Relation, Set[(IntExpr, Literal)]] = {
    import verification.Verifier.indicatorConstForTransactionTriggerRelation
    val txInterfaces = program.interfaces.filter(i => i.relation.name.startsWith(transactionRelationPrefix))
    txInterfaces.map { t =>
      val triggeredRules = program.rules.diff(program.violationRules)
        .filter(r => r.body.exists(lit => lit.relation == t.relation))
      val indicators = triggeredRules.zipWithIndex.map { case (triggeredRule, i) =>
        val const: IntExpr = indicatorConstForTransactionTriggerRelation(ctx, t.relation, i)
        val trigLit: Literal = triggeredRule.body.filter(_.relation.name.startsWith(transactionRelationPrefix)).head
        (const, trigLit)
      }.toSet
      t.relation -> indicators
    }.toMap
  }

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
     val (_, _, encMap0) = getStepSubst(0, stateVars, otherConsts, ctx)

    // 3.1) Check state transaction first.
    val txProperties = getTxProperties(ts, violationRules, verifier,
        program, ctx, stateVars, otherConsts)

    // Clear any previously cached per-step substitutions (important if same BMC instance is reused)
    stepSubstCache.clear()
    // NOTE: per-step substitutions are computed lazily by `getStepSubst` when needed.

     // 3) Unroll and check bounds (start at k=1 to require at least one transition)
     for (k <- 1 to bound) {
       println(s"[BMC] Checking bound = $k")
       val pathConstraint = buildPathConstraint(ts, k, stateVars, otherConsts, ctx)
       // println(s"Path constraint:\n $pathConstraint")
       // check each property at this bound
       for ((rule, prop) <- txProperties) {
         // val violation = ctx.mkNot(prop)
         val violation = prop
         val violationAtK = renameForStep(violation, k-1, stateVars, otherConsts, ctx).asInstanceOf[BoolExpr]
         checkPropertyAtBound(rule, violationAtK, pathConstraint, k, program, stateVars, otherConsts, ctx, ts, encMap0) match {
           case Some(trace) => return (false, Some(trace))
           case None => // continue
         }
       }
       for ((rule, prop) <- properties) {
         // val violation = ctx.mkNot(prop)
         val violation = prop
         val violationAtK = renameForStep(violation, k, stateVars, otherConsts, ctx).asInstanceOf[BoolExpr]
         checkPropertyAtBound(rule, violationAtK, pathConstraint, k, program, stateVars, otherConsts, ctx, ts, encMap0) match {
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

    val txViolationRules = program.violationRules.filter(_.body.exists(_.relation.name.startsWith(transactionRelationPrefix)))
    val stateViolationRules = program.violationRules.diff(txViolationRules)

    // val properties: Seq[(Rule, BoolExpr)] = program.violationRules.toSeq.map { vr =>
    val stateProperties: Seq[(Rule, BoolExpr)] = stateViolationRules.toSeq.map { vr =>
      // val prop = verifier.getProperty(ctx, vr)
      val prop = verifier.getViolationCheck(ctx, vr)
      (vr, prop)
    }
    // val txProperties: Seq[(Rule, BoolExpr)] = ???

    val properties = stateProperties // ++ txProperties
    properties.foreach { case (vr, prop) => println(s"[BMC] Property for violation rule '${vr.head.relation.name}': $prop") }
    (verifier, ts, ctx, properties)
  }

  /** Check if transaction parameter violates txViolation rules but still able to
   *  go through the transaction.
   *  */
  private def getTxProperties(ts: TransitionSystem, violationRules : Set[Rule],
                                verifier: Verifier,
                                program: Program,
                                ctx: Context,
                                stateVars: Seq[(Expr[_], Expr[_])],
                                otherConsts: Set[Expr[_]]): // (Boolean, Option[Trace]) = {
                                Seq[(Rule, BoolExpr)] = {
    // Find rules that mention transaction interfaces (recv_/transactionRelationPrefix)
    val txRules = violationRules.filter(_.body.exists(_.relation.name.startsWith(transactionRelationPrefix)))

    var properties: Seq[(Rule,BoolExpr)] = Seq()
    // if (txRules.isEmpty) return (true, None)

    val init0: BoolExpr = renameForStep(ts.getInit(), 0, stateVars, otherConsts, ctx).asInstanceOf[BoolExpr]
    val (_, _, encMap0) = getStepSubst(0, stateVars, otherConsts, ctx)

    for (r <- txRules) {
      val recvLitOpt = r.body.find(_.relation.name.startsWith(transactionRelationPrefix))
      if (recvLitOpt.isEmpty) throw new Exception(s"No transaction interface found: $r.")
      val recvLit = recvLitOpt.get
      val recvName = recvLit.relation.name

      val violationExpr: BoolExpr = {
        val prefix = "kv" // this is the top-level transaction relation parameter prefix.
        val expr = verifier.getTxViolationCheck(ctx, r, prefix)

        // recvLit is available in the outer scope (the transaction literal for this rule)
        val params = recvLit.fields

        // Build from/to arrays for substitution: kv<name> -> i0_<name>
        val fromArr = params.map { p =>
          ctx.mkConst(s"${prefix}_${p.name}", Z3Helper.typeToSort(ctx, p._type) )
        }.toArray[Expr[_]]

        val toArr = params.map { p =>
          ctx.mkConst(s"i0_${p.name}", Z3Helper.typeToSort(ctx, p._type))
        }.toArray[Expr[_]]

        val subsExpr = expr.substitute(fromArr, toArr).asInstanceOf[BoolExpr]

        // Create the indicator constant and implication: indicator = 1 => (violation with params)
        val anyIndicatorEq1: BoolExpr = {
          val triggeredRulesForInterface = program.transactionRules().filter(_.body.exists(_.relation == recvLit.relation)).toSeq
          val indicatorLits: Seq[BoolExpr] = triggeredRulesForInterface.zipWithIndex.map { case (_, i) =>
            val indicatorConst: IntExpr = indicatorConstForTransactionTriggerRelation(ctx, recvLit.relation, i)
            ctx.mkEq(indicatorConst, ctx.mkInt(1))
          }
          if (indicatorLits.isEmpty) ctx.mkFalse() else ctx.mkOr(indicatorLits: _*)
        }

        ctx.mkAnd(anyIndicatorEq1, subsExpr)
      }

      // Find candidate transition expressions that mention the transaction relation or indicator
      val candidates = ts.getTrs().filter { tr =>
        try {
          val s = tr.toString
          s.contains(recvName) || s.contains(recvName.stripPrefix(transactionRelationPrefix))
        } catch { case _: Throwable => false }
      }
      if (candidates.isEmpty)
        throw new Exception(s"[BMC] Warning: no transition expression found for transaction '$recvName' (rule ${r.head.relation.name})")

      properties +:= (r, violationExpr)
    } // end for txRules
    properties
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

  private def otherConstName(orig: String, step: Int): String = s"${orig}_s${step}"

  private def renameForStep(e: Expr[_], step: Int, stateVars: Seq[(Expr[_], Expr[_])], otherConsts: Set[Expr[_]], ctx: Context): Expr[_] = {
    val (fromArr, toArr, _) = getStepSubst(step, stateVars, otherConsts, ctx)
    e.substitute(fromArr, toArr)
  }

  // Build the substitution arrays and a name->Expr map for a given step.
  // Returns (fromArr, toArr, map) where fromArr/toArr are Arrays used for substitute,
  // and map contains mappings from original symbol name to per-step Expr
  private def buildStepSubst(step: Int, stateVars: Seq[(Expr[_], Expr[_])], otherConsts: Set[Expr[_]], ctx: Context): (Array[Expr[_]], Array[Expr[_]], Map[String, Expr[_]]) = {
    var from = List.empty[Expr[_]]
    var to = List.empty[Expr[_]]
    val m = scala.collection.mutable.Map.empty[String, Expr[_]]
    for ((v_in, v_out) <- stateVars) {
      try {
        val base = v_in.getSExpr
        val inName = otherConstName(base, step)
        val outName = otherConstName(base, step + 1)
        val inVar = ctx.mkConst(inName, v_in.getSort.asInstanceOf[Sort])
        val outVar = ctx.mkConst(outName, v_out.getSort.asInstanceOf[Sort])
        from ::= v_in; to ::= inVar; from ::= v_out; to ::= outVar
        m += (v_in.getSExpr -> inVar)
        m += (v_out.getSExpr -> outVar)
      } catch { case _: Throwable => () }
    }
    for (c <- otherConsts) {
      try {
        val orig = c.getSExpr
        if (orig != "true" && orig != "false") {
          val newConst = ctx.mkConst(otherConstName(orig, step), c.getSort.asInstanceOf[Sort])
          from ::= c; to ::= newConst
          m += (orig -> newConst)
        }
      } catch { case _: Throwable => () }
    }
    val fromArr = from.reverse.toArray
    val toArr = to.reverse.toArray
    (fromArr, toArr, m.toMap)
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
                                   program: Program, stateVars: Seq[(Expr[_], Expr[_])], otherConsts: Set[Expr[_]], ctx: Context,
                                   ts: TransitionSystem,
                                   encMap0: Map[String, Expr[_]]): Option[Trace] = {
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
      // println(s"Model:$model")
      val constructorTx = extractConstructorFromModel(model, ts.getInit(), program, encMap0, ctx)
      val trace = extractTraceFromModel(model, k, ctx, program, stateVars, otherConsts)
      // println(trace)
      // val evalutedTrace = extractEvaluatedTraceFromModel(model, k, ctx, program, stateVars, otherConsts)
      // println(evalutedTrace)
      trace match {
        case Some(t) => Some(Trace( constructorTx +: t.steps))
        case None => throw new Exception("No counter example found")
      }
    } else None
  }

  /**
   * Extract Trace Helper
   */
  // Prefer the per-step encoding map (encMap) when extracting the transaction relation name
  private def extractTransactionRelation(model: Model,
                                         ctx: Context,
                                         stepIdx: Int,
                                         encMap: Map[String, Expr[_]],
                                         program: Program
                                        ): (Relation, Literal) = {
    val indicators: Map[Relation, Set[(IntExpr, Literal)]] = triggerIndicator(ctx, program)
    // Find the relation whose indicator constant evaluates to 1 in the model
    val activeRelations: Seq[(Relation, Literal)] = indicators.toSeq.flatMap { case (rel, indicatorSet) =>
      indicatorSet.collect {
        case (indicatorConst, lit) =>
          val constExpr = encMap.getOrElse(indicatorConst.getSExpr, indicatorConst)
          if (evalModelExpr(model, constExpr).contains("1")) (rel, lit) else null
      }.filter(_ != null)
    }

    if (activeRelations.size != 1)
      throw new Exception(s"Expected exactly one active transaction relation at step $stepIdx, found: ${activeRelations}")

    // Return the recv_* interface relation directly (it already exists in program.relations
    // and its signature matches what InductiveSynthesis expects in the predicates map).
    // The semantic "stripped" relation (e.g. `withdraw`) may have a different signature.
    val (rel, lit) = activeRelations.head
    (rel, lit)
  }

  /**
   * Unified helper to get the BMC model variable for a given name, sort, and step.
   * Optionally uses an encoding map for lookup, otherwise constructs the per-step variable.
   */
  private def stepVar(name: String, sort: Sort, step: Int, ctx: Context, encMap: Map[String, Expr[_]] = Map.empty): Expr[_] = {
    encMap.getOrElse(name, ctx.mkConst(otherConstName(name, step), sort))
  }

  private def extractConstructorFromModel(model: Model, initConstraint: BoolExpr, program: Program,
                                          encMap0: Map[String, Expr[_]], ctx: Context): Transaction = {
    val constructorRel = program.relations.find(_.name == "constructor")
      .getOrElse(throw new Exception("Constructor relation not found in program.relations"))

    val constructorParams = constructorRel.paramList.map (
      p => {
        val sort = Z3Helper.typeToSort(ctx, p._type)
        val expr = ctx.mkConst(otherConstName(s"_${p.name}", 0), sort)
        val value: String = evalModelExpr(model, expr).getOrElse("").replaceAll("\"", "")
        datalog.Constant(p._type, value)
      }
    )

    val msgSenderVal: Int = evalModelInt(model, stepVar("msgSender", ctx.getIntSort, 0, ctx, encMap0)).getOrElse(0)
    val msgValueVal: Int = evalModelInt(model, stepVar("msgValue", ctx.getIntSort, 0, ctx, encMap0)).getOrElse(0)
    Transaction(constructorRel, constructorParams, ImplicitParameters(msgSenderVal,msgValueVal))
  }

  private def extractTraceFromModel(model: Model, k: Int, ctx: Context, program: Program,
                                    stateVars: Seq[(Expr[_], Expr[_])], otherConsts: Set[Expr[_]]): Option[Trace] = {
     import synthesis.Context.{msgValue, msgSender}
     import scala.collection.mutable.ArrayBuffer
     val steps = ArrayBuffer.empty[synthesis.Transaction]
     for (stepIdx <- 0 until k) {
       // get the per-step substitution + map produced by the same helper used in renameForStep
       val (_, _, encMap) = getStepSubst(stepIdx, stateVars, otherConsts, ctx)
       // extract transaction relation using the encoding map (with ctx as fallback)
       val (txRel,triggerLiteral) = extractTransactionRelation(model, ctx, stepIdx, encMap, program)

       def evalFieldConst(p: Parameter): Constant = {
         val field = p.name
         val tpe = p._type
         val sort = Z3Helper.typeToSort(ctx, tpe)
         val prefix = "i0_"
         val fieldName = s"$prefix$field"
         // val cExpr: Expr[_] = encMap.getOrElse(field, ctx.mkConst(otherConstName(s"$prefix$field", stepIdx), sort))
         val cExpr = stepVar(fieldName, sort, stepIdx, ctx, encMap)
         val value: String = evalModelExpr(model, cExpr).getOrElse("").replaceAll("\"", "")
         datalog.Constant(tpe, value)
       }

       val parameters: List[datalog.Constant] = triggerLiteral.fields.map(evalFieldConst)

       def evalIntConst(name: String): Int = {
         val cExpr: Expr[_] = stepVar(name, ctx.getIntSort, stepIdx, ctx, encMap)
         evalModelInt(model, cExpr).getOrElse(0)
       }

       val (msgSenderVal,msgValueVal) = (evalIntConst("msgSender"), evalIntConst("msgValue"))
       // val (msgSenderVal,msgValueVal) = if (stepIdx== 0) {
       //   (evalIntConst("msgSender"), evalIntConst("msgValue"))
       // }
       // else {
       //   (evalFieldConst(msgSender.fields.head).name.toInt,
       //     evalFieldConst(msgValue.fields.head).name.toInt)
       // }
       val implicitParams = synthesis.ImplicitParameters(msgSenderVal, msgValueVal)
       val tx = synthesis.Transaction(txRel, parameters, implicitParams)
       steps += tx
     }
     Some(synthesis.Trace(steps.toSeq))
   }

  /**
   * Helper to extract a State object from the model at a given step.
   * Uses stateVars, ctx, and encMap for lookup.
   */
  private def extractStateFromModel(model: Model, stepIdx: Int, stateVars: Seq[(Expr[_], Expr[_])], ctx: Context, encMap: Map[String, Expr[_]]): State = {
    import synthesis.State
    // Create an empty State
    val state = State()

    // Helpers to evaluate model expressions (use Z3 Expr fully-qualified to avoid clash)
    def evalStrOpt(e: com.microsoft.z3.Expr[_]): Option[String] = evalModelExpr(model, e, true).map(_.replaceAll("\"", ""))
    def evalIntOpt(e: com.microsoft.z3.Expr[_]): Option[Int] = evalModelInt(model, e, true)

    // Populate scalar variables from stateVars (use the v_in name as canonical)
    for ((v_in, _) <- stateVars) {
        val name = v_in.getSExpr
        val sort: Sort = v_in.getSort.asInstanceOf[Sort]
        val expr: com.microsoft.z3.Expr[_] = encMap.getOrElse(name, ctx.mkConst(otherConstName(name, stepIdx), sort)).asInstanceOf[com.microsoft.z3.Expr[_]]

        // Match common sorts: Int, Bool, and fallback (symbol/string)
        if (sort == ctx.getIntSort) {
          evalIntOpt(expr).foreach { v =>
            state.update(datalog.Variable(datalog.Type.integerType, name),
              datalog.Constant(datalog.Type.integerType, v.toString))
          }
        } else if (sort == ctx.getBoolSort) {
          evalStrOpt(expr).foreach { s =>
            val b = if (s == "true" || s == "1") "1" else "0"
            state.update(datalog.Variable(datalog.BooleanType(), name),
              datalog.Constant(datalog.BooleanType(), b))
          }
        } else if (sort == ctx.getBoolSort) {
          evalStrOpt(expr).foreach { s =>
            val _type = datalog.SymbolType(name)
            state.update(datalog.Variable(_type, name), datalog.Constant(_type, s))
          }
        }
        else {
          ???
        }
    }

    // Attempt to populate relation maps (materialized relations) from model constants.
    // We use heuristics: keys in encMap refer to per-step renamed constants; maps are harder to reconstruct precisely here.
    // We'll look for encMap entries that correspond to map accesses by scanning encMap keys for relation-like patterns
    try {
      // No-op: reconstruction of maps requires encoder details available elsewhere. Keep as placeholder for now.
    } catch { case _: Throwable => () }

    state
  }

  /**
   * Extracts the evaluated trace: for each step, returns the transaction and the state variable values before execution.
   */
  def extractEvaluatedTraceFromModel(model: Model, k: Int, ctx: Context, program: Program,
                                     stateVars: Seq[(Expr[_], Expr[_])], otherConsts: Set[Expr[_]]): Option[synthesis.EvaluatedTrace] = {
    import scala.collection.mutable.ArrayBuffer
    import synthesis.State
    val steps = ArrayBuffer.empty[(Transaction, State)]

    // Extract initial state (before any transaction)
    val (_, _, encMap0) = getStepSubst(0, stateVars, otherConsts, ctx)
    val initialState = extractStateFromModel(model, 0, stateVars, ctx, encMap0)

    for (stepIdx <- 0 until k) {
      val (_, _, encMap) = getStepSubst(stepIdx, stateVars, otherConsts, ctx)
      val (txRel, triggerLiteral) = extractTransactionRelation(model, ctx, stepIdx, encMap, program)

      def evalFieldConst(p: Parameter): Constant = {
        val field = p.name
        val tpe = p._type
        val sort = Z3Helper.typeToSort(ctx, tpe)
        val prefix = "i0_"
        val fieldName = s"$prefix$field"
        val cExpr = stepVar(fieldName, sort, stepIdx, ctx, encMap)
        val value: String = evalModelExpr(model, cExpr).getOrElse("").replaceAll("\"", "")
        datalog.Constant(tpe, value)
      }
      val parameters: List[datalog.Constant] = triggerLiteral.fields.map(evalFieldConst)

      def evalIntConst(name: String): Int = {
        val cExpr: Expr[_] = stepVar(name, ctx.getIntSort, stepIdx, ctx, encMap)
        evalModelInt(model, cExpr).getOrElse(0)
      }
      val msgSenderVal = evalIntConst("msgSender")
      val msgValueVal = evalIntConst("msgValue")
      val implicitParams = synthesis.ImplicitParameters(msgSenderVal, msgValueVal)
      val tx = synthesis.Transaction(txRel, parameters, implicitParams)

      // Extract state after transaction execution (at stepIdx+1)
      val (_, _, encMapNext) = getStepSubst(stepIdx + 1, stateVars, otherConsts, ctx)
      val stateAfter = extractStateFromModel(model, stepIdx + 1, stateVars, ctx, encMapNext)

      steps += ((tx, stateAfter))
    }
    Some(synthesis.EvaluatedTrace(initialState, steps.toSeq))
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
