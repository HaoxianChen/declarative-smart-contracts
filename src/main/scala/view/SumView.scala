package view

import datalog.{Arithmetic, Literal, Param, Relation, Rule, Sum}
import imp.{Increment, IncrementValue, OnInsert, OnStatement, Statement}

case class SumView(rule: Rule, primaryKeyIndices: List[Int]) extends View {
  require(rule.aggregators.size==1)
  require(rule.aggregators.head.isInstanceOf[Sum])
  val sum: Sum = rule.aggregators.head.asInstanceOf[Sum]

  /** Interfaces */
  def insertRow(relation: Relation): OnStatement = {
    require(rule.aggregators.size == 1)
    require(rule.aggregators.contains(sum))
    require(rule.body.size <= 1 && rule.body.forall(_.relation == sum.relation))
    require(rule.head.fields.contains(sum.aggResult))

    val insertedLiteral = getInsertedLiteral(relation)
    val resultIndex = rule.head.fields.indexOf(sum.aggResult)
    val keyIndices = rule.head.fields.indices.toList.filterNot(_ == resultIndex)
    val delta: Arithmetic = Param(sum.aggParam)
    val increment = Increment(rule.head.relation, rule.head, keyIndices, resultIndex, delta = delta)
    OnInsert(insertedLiteral, rule.head.relation, increment)
  }

  def deleteRow(relation: Relation): OnStatement = ???

  def updateRow(incrementValue: IncrementValue): OnStatement = ???

  protected def getInsertedLiteral(relation: Relation): Literal = {
    require(relation==sum.relation)
    sum.literal
  }
}

