package view

import datalog.{Arithmetic, Literal, Param, Relation, Rule, Sum}
import imp.{DeleteTuple, Increment, IncrementValue, Insert, InsertTuple, OnIncrement, OnInsert, OnStatement, Statement}

case class SumView(rule: Rule, primaryKeyIndices: List[Int], ruleId: Int) extends View {
  require(rule.aggregators.size==1)
  require(rule.aggregators.head.isInstanceOf[Sum])
  val sum: Sum = rule.aggregators.head.asInstanceOf[Sum]

  /** Interfaces */
  def insertRow(insertTuple: InsertTuple): OnStatement = {
    val insertedLiteral = getInsertedLiteral(insertTuple.relation)
    val resultIndex = rule.head.fields.indexOf(sum.aggResult)
    val keyIndices = rule.head.fields.indices.toList.filterNot(_ == resultIndex)
    val delta: Arithmetic = Param(sum.aggParam)
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
      Param(insertedLiteral.fields(incrementValue.valueIndex))
    }
    val increment = Increment(rule.head.relation, rule.head, keyIndices, resultIndex, delta = delta)
    OnIncrement(insertedLiteral, keyIndices, updateIndex = incrementValue.valueIndex, updateTarget = rule.head.relation,
      increment, ruleId)
  }

  protected def getInsertedLiteral(relation: Relation): Literal = {
    require(relation==sum.relation)
    sum.literal
  }
}

