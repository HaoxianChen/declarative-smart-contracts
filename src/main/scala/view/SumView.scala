package view

import com.microsoft.z3.{ArithExpr, ArithSort, ArrayExpr, ArraySort, BoolExpr, Context, Expr, Sort}
import datalog.{Add, Arithmetic, Literal, Param, Relation, Rule, Sum}
import imp.{DeleteTuple, Increment, IncrementValue, Insert, InsertTuple, OnIncrement, OnInsert, OnStatement, ReplacedByKey, Statement, Trigger}
import verification.RuleZ3Constraints
import verification.TransitionSystem.makeStateVar
import verification.Z3Helper.{fieldsToConst, functorExprToZ3}

case class SumView(rule: Rule, primaryKeyIndices: List[Int], ruleId: Int) extends View {
  require(rule.aggregators.size==1)
  require(rule.aggregators.head.isInstanceOf[Sum])
  val sum: Sum = rule.aggregators.head.asInstanceOf[Sum]

  /** Interfaces */
  def insertRow(insertTuple: InsertTuple): OnStatement = {
    val insertedLiteral = getInsertedLiteral(insertTuple.relation)
    val (delta, resultIndex) = getDelta(insertTuple)
    // val resultIndex = rule.head.fields.indexOf(sum.aggResult)
    val keyIndices = rule.head.fields.indices.toList.filterNot(_ == resultIndex)
    // val delta: Arithmetic = {
    //   val d = Param(sum.aggParam)
    //   Arithmetic.updateArithmeticType(d, View.getDeltaType(d._type))
    // }
    val increment = Increment(rule.head.relation, rule.head, keyIndices, resultIndex, delta = delta)
    OnInsert(insertedLiteral, rule.head.relation, increment, ruleId)
  }

  def deleteRow(deleteTuple: DeleteTuple): OnStatement = {
    ???
  }

  def updateRow(incrementValue: IncrementValue): OnStatement = {
    val insertedLiteral = getInsertedLiteral(incrementValue.relation)
    val resultIndex = rule.head.fields.indexOf(sum.aggResult)
    val keyIndices = {
      val keys = primaryKeyIndices.map(i=>rule.head.fields(i))
      keys.map(k=>insertedLiteral.fields.indexOf(k))
    }
    // val delta: Arithmetic = incrementValue.delta
    val delta: Arithmetic = {
      val d = Param(insertedLiteral.fields(incrementValue.valueIndex))
      d
    }
    val increment = Increment(rule.head.relation, rule.head, keyIndices, resultIndex, delta = delta)
    OnIncrement(insertedLiteral, keyIndices, updateIndex = incrementValue.valueIndex, updateTarget = rule.head.relation,
      increment, ruleId)
  }

  def getInsertedLiteral(relation: Relation): Literal = {
    require(relation==sum.relation)
    sum.literal
  }

  private def getDelta(incrementValue: IncrementValue): (Arithmetic, Int) = {
    val insertedLiteral = getInsertedLiteral(incrementValue.relation)
    val resultIndex = rule.head.fields.indexOf(sum.aggResult)
    val delta: Arithmetic = {
      val d = Param(insertedLiteral.fields(incrementValue.valueIndex))
      d
    }
    (delta, resultIndex)
  }

  private def getDelta(insertTuple: InsertTuple): (Arithmetic, Int) = {
    val resultIndex = rule.head.fields.indexOf(sum.aggResult)
    val delta: Arithmetic = {
      val d = Param(sum.aggParam)
      d
    }
    (delta, resultIndex)
  }

  /** Interfaces to generate Z3 constraints */
  def insertRowZ3(ctx: Context, insertTuple: InsertTuple, isMaterialized: Boolean, z3Prefix: String) = {
    val (delta, resultIndex) = getDelta(insertTuple)
    val insertedLiteral = getInsertedLiteral(insertTuple.relation)
    val (updateConstraint, updateExpr) = updateTargetRelationZ3(ctx, insertedLiteral, delta, resultIndex, isMaterialized, z3Prefix)

    /** todo: support more general cases, where join exists.
     * Now this function only propagates the update.
     * */
    val bodyConstraint = ctx.mkTrue()
    makeRuleZ3Constraints(ctx, bodyConstraint, updateConstraint, updateExpr,
      IncrementValue(this.relation, this.primaryKeyIndices, resultIndex, delta))
  }

  def updateRowZ3(ctx: Context, incrementValue: IncrementValue, isMaterialized: Boolean, z3Prefix: String) = {
    val (delta, resultIndex) = getDelta(incrementValue)
    val insertedLiteral = getInsertedLiteral(incrementValue.relation)
    val (updateConstraint, updateExpr) = updateTargetRelationZ3(ctx, insertedLiteral, delta, resultIndex, isMaterialized, z3Prefix)
    /** todo: support more general cases, where join exists.
     * Now this function only propagates the update.
     * */
    val bodyConstraint = ctx.mkTrue()
    makeRuleZ3Constraints(ctx, bodyConstraint, updateConstraint, updateExpr,
      IncrementValue(this.relation, this.primaryKeyIndices, resultIndex, delta))
  }

  def deleteRowZ3(ctx: Context, deleteTuple: DeleteTuple, isMaterialized: Boolean, z3Prefix: String) = ???

  def getQueryStatement(): Statement = ???

  def getZ3QueryConstraint(ctx: Context, z3Prefix: String): BoolExpr = ???
}

