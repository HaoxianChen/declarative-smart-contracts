package view

import datalog.{Arithmetic, Count, Literal, Negative, One, Param, Relation, Rule}
import imp.{Increment, IncrementValue, OnDelete, OnInsert, OnStatement}

case class CountView(rule: Rule, primaryKeyIndices: List[Int]) extends View {
  require(rule.aggregators.size==1)
  require(rule.aggregators.head.isInstanceOf[Count])
  val count: Count = rule.aggregators.head.asInstanceOf[Count]

  /** Interfaces */
  def insertRow(relation: Relation): OnStatement = {
    val insertedLiteral = getInsertedLiteral(relation)
    val resultIndex = rule.head.fields.indexOf(count.aggResult)
    val delta: Arithmetic = One(count.aggResult._type)
    val increment = Increment(rule.head.relation, rule.head, primaryKeyIndices, resultIndex, delta = delta)
    OnInsert(insertedLiteral, rule.head.relation, statement = increment)
  }

  def deleteRow(relation: Relation): OnStatement = {
    val deletedLiteral = getInsertedLiteral(relation)
    val resultIndex = rule.head.fields.indexOf(count.aggResult)
    val delta: Arithmetic = Negative(One(count.aggResult._type))
    val decrement = Increment(rule.head.relation, rule.head, primaryKeyIndices, resultIndex, delta = delta)
    OnDelete(deletedLiteral, rule.head.relation, statement = decrement)
  }

  def updateRow(incrementValue: IncrementValue): OnStatement = ???

  protected def getInsertedLiteral(relation: Relation): Literal = {
    require(relation==count.relation)
    count.literal
  }
}
