package view

import com.microsoft.z3.{ArithExpr, ArithSort, ArrayExpr, ArraySort, BoolExpr, Context, Expr, Sort}
import datalog.{Add, Arithmetic, Literal, Param, Relation, Rule, Sum}
import imp.{DeleteTuple, Increment, IncrementValue, Insert, InsertTuple, OnIncrement, OnInsert, OnStatement, ReplacedByKey, Statement, Trigger}
import verification.TransitionSystem.makeStateVar
import verification.Verifier.{arithmeticToZ3, fieldsToConst}

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
    val keyIndices = rule.head.fields.indices.toList.filterNot(_ == resultIndex)
    // val delta: Arithmetic = incrementValue.delta
    val delta: Arithmetic = {
      val d = Param(insertedLiteral.fields(incrementValue.valueIndex))
      Arithmetic.updateArithmeticType(d, View.getDeltaType(d._type))
    }
    val increment = Increment(rule.head.relation, rule.head, keyIndices, resultIndex, delta = delta)
    OnIncrement(insertedLiteral, keyIndices, updateIndex = incrementValue.valueIndex, updateTarget = rule.head.relation,
      increment, ruleId)
  }

  protected def getInsertedLiteral(relation: Relation): Literal = {
    require(relation==sum.relation)
    sum.literal
  }

  private def getDelta(incrementValue: IncrementValue): (Arithmetic, Int) = {
    val insertedLiteral = getInsertedLiteral(incrementValue.relation)
    val resultIndex = rule.head.fields.indexOf(sum.aggResult)
    val delta: Arithmetic = {
      val d = Param(insertedLiteral.fields(incrementValue.valueIndex))
      Arithmetic.updateArithmeticType(d, View.getDeltaType(d._type))
    }
    (delta, resultIndex)
  }

  private def getDelta(insertTuple: InsertTuple): (Arithmetic, Int) = {
    val resultIndex = rule.head.fields.indexOf(sum.aggResult)
    val delta: Arithmetic = {
      val d = Param(sum.aggParam)
      Arithmetic.updateArithmeticType(d, View.getDeltaType(d._type))
    }
    (delta, resultIndex)
  }

  /** Interfaces to generate Z3 constraints */
  def insertRowZ3(ctx: Context, insertTuple: InsertTuple, isMaterialized: Boolean, z3Prefix: String): BoolExpr = {
    val (delta, resultIndex) = getDelta(insertTuple)
    val insertedLiteral = getInsertedLiteral(insertTuple.relation)
    updateTargetRelationZ3(ctx, insertedLiteral, delta, resultIndex, isMaterialized, z3Prefix)
  }

  def updateRowZ3(ctx: Context, incrementValue: IncrementValue, isMaterialized: Boolean, z3Prefix: String): BoolExpr = {
    val (delta, resultIndex) = getDelta(incrementValue)
    val insertedLiteral = getInsertedLiteral(incrementValue.relation)
    updateTargetRelationZ3(ctx, insertedLiteral, delta, resultIndex, isMaterialized, z3Prefix)
  }

  def getNextTriggers(trigger: Trigger): Set[Trigger] = {
    assert(this.rule.body.exists(_.relation==trigger.relation) || this.sum.relation == trigger.relation, s"${trigger}\n${this.rule}")
    trigger match {
      case insertTuple:InsertTuple => {
        val (delta, resultIndex) = getDelta(insertTuple)
        Set(IncrementValue(relation, primaryKeyIndices, resultIndex, delta))
      }
      case DeleteTuple(relation, keyIndices) => ???
      case ReplacedByKey(relation, keyIndices, targetRelation) => ???
      case incremeantValue:IncrementValue => {
        val (delta, resultIndex) = getDelta(incremeantValue)
        Set(IncrementValue(relation, primaryKeyIndices, resultIndex, delta))
      }
    }
  }

}

