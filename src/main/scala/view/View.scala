package view

import datalog._
import imp._

abstract class View {
  def rule: Rule
  def primaryKeyIndices: List[Int]
  val relation: Relation = rule.head.relation

  /** Interfaces */
  def insertRow(relation: Relation): OnStatement
  def deleteRow(relation: Relation): OnStatement
  def updateRow(incrementValue: IncrementValue): OnStatement

  def getUpdateStatement(trigger: Trigger): OnStatement = trigger match {
    case InsertTuple(relation, keyIndices) => insertRow(relation)
    case DeleteTuple(relation, keyIndices) => deleteRow(relation)
    case iv: IncrementValue => updateRow(iv)
  }

  val isDeleteBeforeInsert: Boolean = {
    // todo: skip deletion when the inserted literal share the same key with the head.
    primaryKeyIndices.nonEmpty
  }

  protected def getInsertTupleStatement(): Statement = {
    val insert = Insert(rule.head)
    if (isDeleteBeforeInsert) {
      val keys = primaryKeyIndices.map(i=>insert.literal.fields(i))
      val deleteByKeys = DeleteByKeys(insert.relation, keys)
      Statement.makeSeq(deleteByKeys, insert)
    }
    else {
      insert
    }
  }

}
object View {
  def apply(rule: Rule, primaryKeyIndices: List[Int]): View = {
    require(rule.aggregators.size <= 1)
    if (rule.aggregators.isEmpty) {
      JoinView(rule, primaryKeyIndices)
    }
    else {
      rule.aggregators.head match {
        case _:Sum => SumView(rule, primaryKeyIndices)
        case _:Max => MaxView(rule, primaryKeyIndices)
        case _:Count => CountView(rule, primaryKeyIndices)
      }
    }
  }
}

