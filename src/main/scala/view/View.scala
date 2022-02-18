package view

import datalog._
import imp._

abstract class View {
  def rule: Rule
  val relation: Relation = rule.head.relation

  /** Interfaces */
  def insertRow(relation: Relation): Statement
  def deleteRow(relation: Relation): Statement
  def updateRow(incrementValue: IncrementValue): Statement

  def getUpdateStatement(trigger: Trigger): Statement = trigger match {
    case InsertTuple(relation) => insertRow(relation)
    case iv: IncrementValue => updateRow(iv)
  }
}
object View {
  def apply(rule: Rule): View = {
    require(rule.aggregators.size <= 1)
    if (rule.aggregators.isEmpty) {
      JoinView(rule)
    }
    else {
      rule.aggregators.head match {
        case _:Sum => SumView(rule)
        case _:Max => MaxView(rule)
      }
    }
  }
}

