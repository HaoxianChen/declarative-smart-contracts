package verification

import com.microsoft.z3.{BoolExpr, Context, Expr, Sort}
import datalog.Relation
import imp.Trigger
import verification.RuleZ3Constraints.getVersionedVariableName
import verification.TransitionSystem.makeStateVar
import verification.Z3Helper.getSort

case class RuleZ3Constraints(ruleConstraints: BoolExpr,
                             updateConstraint: BoolExpr,
                             updateExprs: Array[(Expr[Sort], Expr[Sort], Expr[_<:Sort])],
                             nextTriggers: Set[Trigger]) {
  def allConstraints(ctx: Context): BoolExpr = ctx.mkAnd(ruleConstraints, updateConstraint)

  def getVersionedUpdateConstraint(ctx: Context, relation: Relation, indices: List[Int], version: Int): BoolExpr = {
    if (updateExprs.nonEmpty) {
      val allUpdates = {
        val _exprs: Array[BoolExpr] = updateExprs.map(t => ctx.mkEq(t._2, t._3))
        ctx.mkAnd(_exprs:_*)
      }
      val versionedUpdates: BoolExpr = versionUpdateExpr(ctx, allUpdates, relation, indices, version).asInstanceOf[BoolExpr]
      ctx.mkAnd(updateConstraint, versionedUpdates)
    }
    else {
      ctx.mkAnd(updateConstraint).simplify().asInstanceOf[BoolExpr]
    }
  }

  private def versionUpdateExpr(ctx: Context, expr: Expr[_], relation: Relation, indices: List[Int], version: Int): Expr[_] = {
    val sort = getSort(ctx, relation, indices)
    val (v_in, v_out) = makeStateVar(ctx, relation.name, sort)
    val v_in_versioned = ctx.mkConst(getVersionedVariableName(relation, version), sort)
    val v_out_versioned = ctx.mkConst(getVersionedVariableName(relation, version+1), sort)
    val from: Array[Expr[_]] = Array(v_in, v_out)
    val to: Array[Expr[_]] = Array(v_in_versioned, v_out_versioned)
    expr.substitute(from, to)
  }
}
object RuleZ3Constraints {
  def apply(ctx:Context): RuleZ3Constraints = RuleZ3Constraints(ctx.mkTrue(), ctx.mkTrue(), Array(), Set())

  def getVersionedVariableName(relation: Relation, version: Int): String = {
    if (version > 0) s"${relation.name}_v${version}"
    else s"${relation.name}"
  }
}

