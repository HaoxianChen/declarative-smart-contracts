package synthesis

import com.microsoft.z3.{BoolExpr, Context, Expr, Model, Sort, Status, Z3Exception}
import datalog.{Constant, Program, Relation, Rule}
import imp.ImperativeTranslator
import util.Misc
import verification.{Prove, TransitionSystem, Verifier}

import scala.collection.mutable

case class BoundedModelChecker() {
  /** Input:
   *    - A datalog program
   *    - A set of rules that are query violation instance
   *  Output:
   *    - Result: Boolean
   *    - Counter example if result is false. */
  def check(program: Program, violationRules: Set[Rule], bound: Int): (Boolean, Option[Trace]) = {
    require(bound >= 0, s"Bound must be non-negative, got $bound")
    if (violationRules.isEmpty || bound == 0) {
      return (true, None)
    }

    val materializedRelations: Set[Relation] = Set()
    val translator = new ImperativeTranslator(
      program,
      materializedRelations,
      isInstrument = true,
      monitorViolations = false,
      arithmeticOptimization = true,
      enableProjection = true
    )
    val imperative = translator.translate()

    val verifier = new Verifier(program, imperative)
    val ctx = BoundedModelChecker.ReflectionUtils.getField[Context](verifier, "ctx")
    val ts = BoundedModelChecker.ReflectionUtils.invokeNoArg[TransitionSystem](verifier, "getTransitionSystem")

    val combinedTrans = ts.getTr()
    val init = ts.getInit()

    // Collect state variables (current, next) from transition system
    val variables: Seq[(Expr[_], Expr[_])] = BoundedModelChecker.ReflectionUtils.getField[Set[(Expr[_], Expr[_])]](ts, "variables").toSeq
    val (xs, xns) = BoundedModelChecker.BmcUtilities.splitStateVariables(variables)

    val stateExprs: Set[Expr[_]] = variables.flatMap { case (cur, nxt) => Seq(cur, nxt) }.toSet
    val relationByName: Map[String, Relation] = program.relations.map(r => r.name -> r).toMap

    val propertyMethod = classOf[Verifier].getDeclaredMethod("getProperty", classOf[Context], classOf[Rule])
    propertyMethod.setAccessible(true)

    val allViolationRules = if (violationRules.nonEmpty) violationRules else program.violationRules

    for (rule <- allViolationRules) {
      val property = propertyMethod.invoke(verifier, ctx, rule).asInstanceOf[BoolExpr]
      val violationGoal = ctx.mkNot(property)

      val fvsSet = (Prove.get_vars(init) ++ Prove.get_vars(combinedTrans) ++ Prove.get_vars(violationGoal))
        .diff(stateExprs)
      val fvs = BoundedModelChecker.BmcUtilities.orderExprArray(fvsSet)

      BoundedModelChecker.SimpleBMC.bmc(ctx, init, combinedTrans, violationGoal, fvs, xs, xns, bound) match {
        case Some(model) if model.nonEmpty =>
          val traceOpt = BoundedModelChecker.BmcUtilities.modelToTrace(model, relationByName)
          return (false, traceOpt)
        case _ =>
      }
    }

    (true, None)
  }
}

object BoundedModelChecker {
  /** Prepare some unit tests here.
   *  - Read the program and violation rules from a file.
   *  - Return counter example when it violates the property.
   * */
  def unitTest1(): Unit = {
    println("[BMC][unitTest1] ===== testbmc =====")
    val ctx1 = new Context()
    val bvSize = 4
    val bvSort = ctx1.mkBitVecSort(bvSize)
    val x = ctx1.mkBVConst("x", bvSize)
    val xNext = ctx1.mkBVConst("x_next", bvSize)
    val init1 = ctx1.mkEq(x, ctx1.mkBV(0, bvSize))
    val trans1 = ctx1.mkEq(xNext, ctx1.mkBVAdd(x, ctx1.mkBV(3, bvSize)))
    val goal1 = ctx1.mkEq(x, ctx1.mkBV(15, bvSize))

    SimpleBMC
      .bmc(ctx1, init1, trans1, goal1, Array.empty, Array(x), Array(xNext), bound = 8)
      .foreach { model =>
        var id = 0
        model.foreach { elem =>
          print(s"$id: ")
          println(elem.mkString(", "))
          id += 1
        }
      }
    ctx1.close()

    println("[BMC][unitTest1] ===== testbmc2 =====")
    val ctx2 = new Context()
    val ts = TransitionSystem("Crowdsale", ctx2)

    val GOAL = 10000
    val CLOSETIME = 10000

    val addrSort = ctx2.mkBitVecSort(256)

    val (state, stateOut) = ts.newVar("state", ctx2.mkBitVecSort(2))
    val OPEN = ctx2.mkBV(0, 2)
    val SUCCESS = ctx2.mkBV(1, 2)
    val REFUND = ctx2.mkBV(2, 2)

    val (deposits, depositsOut) = ts.newVar("deposits", ctx2.mkArraySort(addrSort, ctx2.mkBitVecSort(256)))
    val (totalDeposits, totalDepositsOut) = ts.newVar("totalDeposits", ctx2.mkBitVecSort(256))
    val (raised, raisedOut) = ts.newVar("raised", ctx2.mkBitVecSort(256))
    val (auxWithdraw, auxWithdrawOut) = ts.newVar("aux_withdraw", ctx2.mkBoolSort())
    val (auxRefund, auxRefundOut) = ts.newVar("aux_refund", ctx2.mkBoolSort())
    val (func, funcOut) = ts.newVar("func", ctx2.mkStringSort())
    val (timestamp, timestampOut) = ts.newVar("now", ctx2.mkBitVecSort(256))

    val p = ctx2.mkConst("p", addrSort)
    val amount = ctx2.mkBVConst("amount", 256)

    val init2 = ctx2.mkAnd(
      ctx2.mkForall(Array(p), ctx2.mkEq(ctx2.mkSelect(deposits, p), ctx2.mkBV(0, 256)), 1, null, null, ctx2.mkSymbol("Q1"), ctx2.mkSymbol("skid1")),
      ctx2.mkEq(raised, ctx2.mkBV(0, 256)),
      ctx2.mkEq(totalDeposits, ctx2.mkBV(0, 256)),
      ctx2.mkEq(auxWithdraw, ctx2.mkFalse()),
      ctx2.mkEq(auxRefund, ctx2.mkFalse()),
      ctx2.mkEq(state, OPEN),
      ctx2.mkEq(func, ctx2.mkString("init")),
      ctx2.mkEq(timestamp, ctx2.mkBV(0, 256))
    )

    val trInvest = ctx2.mkAnd(
      ctx2.mkBVULT(raised, ctx2.mkBV(GOAL, 256)),
      ctx2.mkEq(stateOut, state),
      ctx2.mkEq(raisedOut, ctx2.mkBVAdd(raised, amount)),
      ctx2.mkEq(depositsOut, ctx2.mkStore(deposits, p, ctx2.mkBVAdd(ctx2.mkSelect(deposits, p), amount))),
      ctx2.mkEq(totalDepositsOut, ctx2.mkBVAdd(totalDeposits, amount)),
      ctx2.mkEq(auxRefundOut, auxRefund),
      ctx2.mkEq(auxWithdrawOut, auxWithdraw),
      ctx2.mkEq(funcOut, ctx2.mkString("invest")),
      ctx2.mkBVSGT(timestampOut, timestamp)
    )

    val trCloseSuccess = ctx2.mkAnd(
      ctx2.mkBVUGE(raised, ctx2.mkBV(GOAL, 256)),
      ctx2.mkEq(stateOut, SUCCESS),
      ctx2.mkEq(depositsOut, deposits),
      ctx2.mkEq(raisedOut, raised),
      ctx2.mkEq(totalDepositsOut, totalDeposits),
      ctx2.mkEq(auxRefundOut, auxRefund),
      ctx2.mkEq(auxWithdrawOut, auxWithdraw),
      ctx2.mkEq(funcOut, ctx2.mkString("close_success")),
      ctx2.mkBVSGT(timestampOut, timestamp)
    )

    val trCloseRefund = ctx2.mkAnd(
      ctx2.mkBVSGT(timestamp, ctx2.mkBV(CLOSETIME, 256)),
      ctx2.mkBVULT(raised, ctx2.mkBV(GOAL, 256)),
      ctx2.mkEq(stateOut, REFUND),
      ctx2.mkEq(depositsOut, deposits),
      ctx2.mkEq(raisedOut, raised),
      ctx2.mkEq(totalDepositsOut, totalDeposits),
      ctx2.mkEq(auxRefundOut, auxRefund),
      ctx2.mkEq(auxWithdrawOut, auxWithdraw),
      ctx2.mkEq(funcOut, ctx2.mkString("close_refund")),
      ctx2.mkBVSGT(timestampOut, timestamp)
    )

    val trClaimRefund = ctx2.mkAnd(
      ctx2.mkEq(state, REFUND),
      ctx2.mkBVULT(raised, ctx2.mkBV(GOAL, 256)),
      ctx2.mkEq(depositsOut, ctx2.mkStore(deposits, p, ctx2.mkBV(0, 256))),
      ctx2.mkEq(totalDepositsOut, ctx2.mkBVSub(totalDeposits, ctx2.mkSelect(deposits, p))),
      ctx2.mkEq(raisedOut, raised),
      ctx2.mkEq(stateOut, state),
      ctx2.mkEq(auxRefundOut, ctx2.mkTrue()),
      ctx2.mkEq(auxWithdrawOut, auxWithdraw),
      ctx2.mkEq(funcOut, ctx2.mkString("claimrefund")),
      ctx2.mkBVSGT(timestampOut, timestamp)
    )

    val trWithdraw = ctx2.mkAnd(
      ctx2.mkEq(state, SUCCESS),
      ctx2.mkEq(totalDepositsOut, ctx2.mkBV(0, 256)),
      ctx2.mkEq(depositsOut, deposits),
      ctx2.mkEq(raisedOut, raised),
      ctx2.mkEq(stateOut, state),
      ctx2.mkEq(auxWithdrawOut, ctx2.mkTrue()),
      ctx2.mkEq(auxRefundOut, auxRefund),
      ctx2.mkEq(funcOut, ctx2.mkString("withdraw")),
      ctx2.mkBVSGT(timestampOut, timestamp)
    )

    println(funcOut.getClass)
    ts.setInit(init2)

    val combinedTransition = ctx2.mkOr(trInvest, trCloseRefund, trCloseSuccess, trClaimRefund, trWithdraw)

    val r2 = ctx2.mkNot(ctx2.mkAnd(auxWithdraw, auxRefund))
    val goal2 = ctx2.mkNot(r2)
    val fvs2: Array[Expr[_]] = Array(p, amount)
    val xs2: Array[Expr[_]] = Array(deposits, totalDeposits, raised, state, auxWithdraw, auxRefund, func, timestamp)
    val xns2: Array[Expr[_]] = Array(depositsOut, totalDepositsOut, raisedOut, stateOut, auxWithdrawOut, auxRefundOut, funcOut, timestampOut)

    SimpleBMC
      .bmc(ctx2, ts.getInit(), combinedTransition, goal2, fvs2, xs2, xns2, bound = 8)
      .foreach { model =>
        var id = 0
        model.foreach { elem =>
          print(s"$id: ")
          println(elem.mkString(", "))
          id += 1
        }
      }
    ctx2.close()
  }

  private object SimpleBMC {
    private var freshIndex: Int = 0

    private def pureName(name: String): String = {
      if (name.contains("|")) {
        val first = name.indexOf("|")
        val second = name.indexOf("|", first + 1)
        if (first >= 0 && second > first) name.substring(first + 1, second)
        else name
      } else name
    }

    private def fresh(round: Int, ctx: Context, name: String, sort: Sort): Expr[_] = {
      freshIndex += 1
      ctx.mkConst(s"${name}:r${round}:i${freshIndex}", sort)
    }

    def bmc(
      ctx: Context,
      init: BoolExpr,
      trans: BoolExpr,
      goal: BoolExpr,
      fvs: Array[Expr[_]],
      xs: Array[Expr[_]],
      xns: Array[Expr[_]],
      bound: Int
    ): Option[Array[mutable.Map[String, Expr[_]]]] = {
      val solver = ctx.mkSolver()
      solver.add(init)

      var currentTrans = trans
      var currentGoal = goal
      var currentXs = xs
      var currentXns = xns
      var currentFvs = fvs

      freshIndex = 0

      for (round <- 0 until bound) {
        val guard = fresh(round, ctx, "P", ctx.getBoolSort).asInstanceOf[BoolExpr]
        solver.push()
        solver.add(guard)
        solver.add(ctx.mkImplies(guard, currentGoal))
        val res = solver.check(guard)
        solver.pop()

        if (res == Status.SATISFIABLE) {
          return Some(extractModel(solver.getModel))
        }

        solver.add(currentTrans)

        val newXs = currentXs.map { x => fresh(round, ctx, pureName(x.toString), x.getSort.asInstanceOf[Sort]) }
        val newFvs = currentFvs.map { f => fresh(round, ctx, pureName(f.toString), f.getSort.asInstanceOf[Sort]) }

        val substitutions = (BmcUtilities.toArray(currentXns ++ currentXs ++ currentFvs), BmcUtilities.toArray(newXs ++ currentXns ++ newFvs))
        currentTrans = substituteSafe(currentTrans, substitutions)
        currentGoal = substituteSafe(currentGoal, (currentXs, currentXns))

        currentXs = currentXns
        currentXns = BmcUtilities.toArray(newXs)
        currentFvs = BmcUtilities.toArray(newFvs)
      }

      None
    }

    private def substituteSafe(expr: BoolExpr, substitution: (Array[Expr[_]], Array[Expr[_]])): BoolExpr = {
      val (from, to) = substitution
      val compatible = from.zip(to).filter { case (f, t) =>
        try {
          f.getSort == t.getSort
        } catch {
          case _: Throwable => false
        }
      }
      expr.substitute(compatible.map(_._1), compatible.map(_._2)).asInstanceOf[BoolExpr]
    }

    private def extractModel(model: Model): Array[mutable.Map[String, Expr[_]]] = {
      val entries = model.getDecls.map { decl =>
        val name = decl.getName.toString
        val value = try {
          model.getConstInterp(decl)
        } catch {
          case _: Z3Exception => decl.getRange
        }
        name -> value.asInstanceOf[Expr[_]]
      }.toMap
      categorize(entries)
    }

    private def categorize(raw: Map[String, Expr[_]]): Array[mutable.Map[String, Expr[_]]] = {
      var maxRound = 0
      raw.foreach { case (key, _) =>
        val round =
          if (key.contains(":r")) {
            key.substring(key.indexOf(":r") + 2, key.indexOf(":i")).toInt
          } else if (key.contains("_next")) 1 else 0
        maxRound = math.max(maxRound, round)
      }

      val buckets = Array.fill(math.max(1, maxRound + 2))(mutable.Map[String, Expr[_]]())
      raw.foreach { case (key, value) =>
        val pname = pureName(key)
        if (pname != "P") {
          val round =
            if (key.contains(":r")) {
              key.substring(key.indexOf(":r") + 2, key.indexOf(":i")).toInt
            } else if (key.contains("_next")) 1 else 0
          val normalized = if (pname.endsWith("_next")) pname.substring(0, pname.length - 5) else pname
          if (round >= 0 && round < buckets.length) {
            buckets(round).update(normalized, value)
          }
        }
      }
      buckets
    }
  }

  private object BmcUtilities {
    def splitStateVariables(vars: Seq[(Expr[_], Expr[_])]): (Array[Expr[_]], Array[Expr[_]]) = {
      val ordered = vars.sortBy { case (cur, _) => cur.toString }
      val current = ordered.map(_._1).toArray
      val next = ordered.map(_._2).toArray
      (current, next)
    }

    def orderExprArray(exprs: Iterable[Expr[_]]): Array[Expr[_]] = {
      toArray(exprs.toSeq.distinct.sortBy(_.toString))
    }

    def toArray(seq: Iterable[Expr[_]]): Array[Expr[_]] = seq.toArray[Expr[_]]

    def modelToTrace(
      model: Array[mutable.Map[String, Expr[_]]],
      relations: Map[String, Relation]
    ): Option[Trace] = {
      val steps = model.zipWithIndex.flatMap { case (entry, stepIdx) =>
        val txValueOpt = entry.get("transaction").orElse(entry.get("func"))
        txValueOpt.flatMap { expr =>
          val raw = expr.toString.replace("\"", "")
          val name = raw.trim
          if (name.isEmpty || name.equalsIgnoreCase("init") || name.equalsIgnoreCase("constructor")) {
            None
          } else {
            relations.get(name).map { rel =>
              val params = rel.sig.map(t => Constant(t, "_"))
              Transaction(rel, params.toList)
            }
          }
        }
      }

      if (steps.isEmpty) None else Some(Trace(steps.toSeq))
    }
  }

  private object ReflectionUtils {
    def getField[T](instance: AnyRef, name: String): T = {
      val field = instance.getClass.getDeclaredField(name)
      field.setAccessible(true)
      field.get(instance).asInstanceOf[T]
    }

    def invokeNoArg[T](instance: AnyRef, name: String): T = {
      val method = instance.getClass.getDeclaredMethod(name)
      method.setAccessible(true)
      method.invoke(instance).asInstanceOf[T]
    }
  }
}
