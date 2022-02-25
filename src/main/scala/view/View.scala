package view

import datalog._
import imp._

abstract class View {
  def rule: Rule
  def primaryKeyIndices: List[Int]
  val relation: Relation = rule.head.relation

  /** Interfaces */
  def insertRow(insertTuple: InsertTuple): OnStatement
  def deleteRow(deleteTuple: DeleteTuple): OnStatement
  def updateRow(incrementValue: IncrementValue): OnStatement

  def getUpdateStatement(trigger: Trigger): OnStatement = trigger match {
    case it: InsertTuple => insertRow(it)
    case dt:DeleteTuple => deleteRow(dt)
    case iv: IncrementValue => updateRow(iv)
  }

  def isDeleteBeforeInsert(relation: Relation, keyIndices: List[Int]): Boolean = {
    // todo: skip deletion when the inserted literal share the same key with the head.
    keyIndices.nonEmpty || relation.isInstanceOf[SingletonRelation]
  }

  protected def deleteByKeysStatement(literal: Literal, keyIndices: List[Int]): Statement = {
      val keys = keyIndices.map(i=>literal.fields(i))
      DeleteByKeys(literal.relation, keys)
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

