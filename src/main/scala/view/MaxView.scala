package view

import datalog.{Literal, Max, Param, Parameter, Relation, Rule, Variable}
import imp.{GroundVar, If, IncrementValue, Insert, OnInsert, ReadTuple, Statement}

case class MaxView(rule: Rule) extends View {
  require(rule.aggregators.size==1)
  require(rule.aggregators.head.isInstanceOf[Max])
  val max: Max = rule.aggregators.head.asInstanceOf[Max]

  /** Interfaces */
  def insertRow(relation: Relation): Statement = {
    val insertedLiteral: Literal = rule.body.head
    val newValue: Param = Param(insertedLiteral.fields(max.valueIndex))
    val groupKeys: List[Parameter] = {
      val allKeys = max.literal.fields.filterNot(_==max.aggParam).filterNot(_.name=="_")
      rule.head.fields.intersect(allKeys)
    }
    val readTuple: ReadTuple = ReadTuple(rule.head.relation, groupKeys)
    val oldValue: Param = Param(Variable(max.aggResult._type,"_max"))
    val groundVar: GroundVar = {
      val valueIndexInHead: Int = rule.head.fields.indexOf(max.aggResult)
      GroundVar(oldValue.p,rule.head.relation,valueIndexInHead)
    }
    val condition = imp.Greater(newValue,oldValue)
    val insert: Insert = Insert(rule.head)
    val stmt = Statement.makeSeq(readTuple, groundVar, If(condition, insert))
    OnInsert(literal = insertedLiteral, updateTarget = rule.head.relation, statement = stmt)
  }

  def deleteRow(relation: Relation): Statement = ???

  def updateRow(incrementValue: IncrementValue): Statement = ???
}
