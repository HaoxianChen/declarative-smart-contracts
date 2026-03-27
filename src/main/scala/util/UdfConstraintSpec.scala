package util

import com.microsoft.z3.{ArithSort, BoolExpr, Context, Expr, Sort}

// ---- Expression tree ----

sealed trait UdfExpr

/** Reference to the i-th input parameter (0-indexed). */
case class UdfParam(index: Int) extends UdfExpr

/** Integer literal. */
case class UdfLiteral(v: BigInt) extends UdfExpr

/** Boolean literal (true / false). */
case class UdfBool(b: Boolean) extends UdfExpr

/**
 * Reference to a contract state variable.
 * initVal is its declared initial value; used to generate a lower-bound constraint in Z3.
 */
case class UdfStateVar(name: String, initVal: BigInt) extends UdfExpr

/** Binary operation: arithmetic (+, -, *, /) or comparison (==, !=, <, <=, >, >=). */
case class UdfBinOp(op: String, lhs: UdfExpr, rhs: UdfExpr) extends UdfExpr

// ---- Constraint types ----

sealed trait UdfConstraint

/** The function always returns a specific expression: `out = expr`. */
case class ReturnEquals(expr: UdfExpr) extends UdfConstraint

/** A conditional if-return branch encoded as an implication: `cond -> out = result`. */
case class ConditionalReturn(cond: UdfExpr, result: UdfExpr) extends UdfConstraint

/** A require() precondition that must hold for inputs. */
case class RequireConstraint(cond: UdfExpr) extends UdfConstraint

/** Explicit arithmetic bounds on the output: `lo <= out <= hi`. */
case class OutputBound(lo: Option[UdfExpr], hi: Option[UdfExpr]) extends UdfConstraint

// ---- Spec ----

/**
 * All extracted constraints for one UDF function.
 *
 * toZ3 converts them into a single BoolExpr for use in the Verifier:
 *   - inputConsts(i) maps to the i-th Datalog input parameter.
 *   - outConst maps to the return value (last column of the Datalog relation).
 *
 * When ConditionalReturn constraints are present, bare ReturnEquals are suppressed to
 * avoid asserting the unconditional branch applies under all conditions.
 */
case class UdfConstraintSpec(fnName: String, constraints: List[UdfConstraint]) {

  def toZ3(ctx: Context, inputConsts: Array[Expr[_]], outConst: Expr[_]): BoolExpr = {

    def evalArith(e: UdfExpr): Expr[ArithSort] = e match {
      case UdfParam(i)          => inputConsts(i).asInstanceOf[Expr[ArithSort]]
      case UdfLiteral(v)        => ctx.mkInt(v.toLong).asInstanceOf[Expr[ArithSort]]
      case UdfStateVar(name, _) => ctx.mkConst(s"udf_sv_$name", ctx.mkIntSort()).asInstanceOf[Expr[ArithSort]]
      case UdfBinOp("*", l, r)  => ctx.mkMul(evalArith(l), evalArith(r))
      case UdfBinOp("+", l, r)  => ctx.mkAdd(evalArith(l), evalArith(r))
      case UdfBinOp("-", l, r)  => ctx.mkSub(evalArith(l), evalArith(r))
      case UdfBinOp("/", l, r)  => ctx.mkDiv(evalArith(l), evalArith(r))
      case UdfBool(_)           => throw new IllegalArgumentException(s"UdfBool cannot appear in arithmetic context")
      case UdfBinOp(op, _, _)   => throw new IllegalArgumentException(s"Operator '$op' is not arithmetic")
    }

    // Returns true for operators that produce a Bool result.
    def isBoolOp(op: String): Boolean =
      Set("==", "!=", "<", "<=", ">", ">=", "||", "&&").contains(op)

    def evalBool(e: UdfExpr): BoolExpr = e match {
      case UdfBool(b)           => if (b) ctx.mkTrue() else ctx.mkFalse()
      case UdfBinOp("||", l, r) => ctx.mkOr(evalBool(l), evalBool(r))
      case UdfBinOp("&&", l, r) => ctx.mkAnd(evalBool(l), evalBool(r))
      case UdfBinOp("==", l, r) =>
        ctx.mkEq(evalArith(l).asInstanceOf[Expr[Sort]], evalArith(r).asInstanceOf[Expr[Sort]])
      case UdfBinOp("!=", l, r) =>
        ctx.mkNot(ctx.mkEq(evalArith(l).asInstanceOf[Expr[Sort]], evalArith(r).asInstanceOf[Expr[Sort]]))
      case UdfBinOp(">=", l, r) => ctx.mkGe(evalArith(l), evalArith(r))
      case UdfBinOp(">",  l, r) => ctx.mkGt(evalArith(l), evalArith(r))
      case UdfBinOp("<=", l, r) => ctx.mkLe(evalArith(l), evalArith(r))
      case UdfBinOp("<",  l, r) => ctx.mkLt(evalArith(l), evalArith(r))
      case _                    => throw new IllegalArgumentException(s"Cannot convert $e to BoolExpr")
    }

    // out == expr, dispatching on bool vs arithmetic output sort.
    // Bool binary ops (||, &&, ==, etc.) must go through evalBool, not evalArith.
    def eqOut(expr: UdfExpr): BoolExpr = expr match {
      case UdfBool(b) =>
        val boolVal = (if (b) ctx.mkTrue() else ctx.mkFalse()).asInstanceOf[Expr[Sort]]
        ctx.mkEq(outConst.asInstanceOf[Expr[Sort]], boolVal)
      case UdfBinOp(op, _, _) if isBoolOp(op) =>
        ctx.mkEq(outConst.asInstanceOf[Expr[Sort]], evalBool(expr).asInstanceOf[Expr[Sort]])
      case e =>
        ctx.mkEq(outConst.asInstanceOf[Expr[Sort]], evalArith(e).asInstanceOf[Expr[Sort]])
    }

    // If there are ConditionalReturn branches, skip unconditional ReturnEquals to avoid contradiction.
    val hasConditionals = constraints.exists(_.isInstanceOf[ConditionalReturn])
    val activeConstraints =
      if (hasConditionals) constraints.filterNot(_.isInstanceOf[ReturnEquals])
      else constraints

    // Collect state-variable lower-bound constraints, deduplicated by name.
    def collectSvs(e: UdfExpr): List[(String, BigInt)] = e match {
      case UdfStateVar(name, initVal) => List((name, initVal))
      case UdfBinOp(_, l, r)         => collectSvs(l) ++ collectSvs(r)
      case _                          => Nil
    }
    val svMap: Map[String, BigInt] = constraints.flatMap {
      case ReturnEquals(e)         => collectSvs(e)
      case ConditionalReturn(c, r) => collectSvs(c) ++ collectSvs(r)
      case OutputBound(lo, hi)     => lo.toList.flatMap(collectSvs) ++ hi.toList.flatMap(collectSvs)
      case _                       => Nil
    }.toMap

    val svConstraints: List[BoolExpr] = svMap.map { case (name, initVal) =>
      val sv = ctx.mkConst(s"udf_sv_$name", ctx.mkIntSort()).asInstanceOf[Expr[ArithSort]]
      ctx.mkGe(sv, ctx.mkInt(initVal.toLong).asInstanceOf[Expr[ArithSort]])
    }.toList

    val semanticParts: List[BoolExpr] = activeConstraints.flatMap {
      case ReturnEquals(expr) =>
        List(eqOut(expr))
      case ConditionalReturn(cond, result) =>
        List(ctx.mkImplies(evalBool(cond), eqOut(result)))
      case RequireConstraint(cond) =>
        List(evalBool(cond))
      case OutputBound(lo, hi) =>
        val loC = lo.map(l => ctx.mkGe(outConst.asInstanceOf[Expr[ArithSort]], evalArith(l)))
        val hiC = hi.map(h => ctx.mkLe(outConst.asInstanceOf[Expr[ArithSort]], evalArith(h)))
        loC.toList ++ hiC.toList
    }

    val parts = semanticParts ++ svConstraints
    if (parts.isEmpty) ctx.mkTrue()
    else ctx.mkAnd(parts.toArray: _*)
  }
}
