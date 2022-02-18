package view

import datalog.{Arithmetic, Param, Relation, Rule, Sum}
import imp.{Increment, IncrementValue, OnInsert, Statement}

case class SumView(rule: Rule) extends View {
  require(rule.aggregators.size==1)
  require(rule.aggregators.head.isInstanceOf[Sum])
  val sum: Sum = rule.aggregators.head.asInstanceOf[Sum]

  /** Interfaces */
  def insertRow(relation: Relation): Statement = {
    require(rule.aggregators.size == 1)
    require(rule.aggregators.contains(sum))
    require(rule.body.size <= 1 && rule.body.forall(_.relation == sum.relation))
    require(rule.head.fields.contains(sum.aggResult))

    val insertedLiteral = sum.literal
    val resultIndex = rule.head.fields.indexOf(sum.aggResult)
    val keyIndices = rule.head.fields.indices.toList.filterNot(_ == resultIndex)
    val delta: Arithmetic = Param(sum.aggParam)
    val increment = Increment(rule.head.relation, rule.head, keyIndices, resultIndex, delta = delta)
    OnInsert(insertedLiteral, rule.head.relation, increment)
  }

  def deleteRow(relation: Relation): Statement = ???

  def updateRow(incrementValue: IncrementValue): Statement = ???
}

