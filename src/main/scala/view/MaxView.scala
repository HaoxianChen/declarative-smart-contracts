package view

import com.microsoft.z3.{BoolExpr, Context}
import datalog.{Literal, Max, Param, Parameter, Relation, Rule, Variable}
import imp.{DeleteTuple, GroundVar, If, IncrementValue, Insert, InsertTuple, OnInsert, OnStatement, ReadTuple, Statement, Trigger}

case class MaxView(rule: Rule, primaryKeyIndices: List[Int], ruleId: Int) extends View {
  require(rule.aggregators.size==1)
  require(rule.aggregators.head.isInstanceOf[Max])
  val max: Max = rule.aggregators.head.asInstanceOf[Max]

  /** Interfaces */
  def insertRow(insertTuple: InsertTuple): OnStatement = {
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
    OnInsert(literal = insertedLiteral, updateTarget = rule.head.relation, statement = stmt, ruleId)
  }

  def deleteRow(deleteTuple: DeleteTuple): OnStatement = ???

  def updateRow(incrementValue: IncrementValue): OnStatement = ???

  protected def getInsertedLiteral(relation: Relation): Literal = {
    val lit = rule.body.head
    require(lit.relation==relation)
    lit
  }

  /** Interfaces to genreate Z3 constraints */
  def getNextTriggers(trigger: Trigger): Set[Trigger] = ???

  def insertRowZ3(ctx: Context, insertTuple: InsertTuple, isMaterialized:Boolean, z3Prefix: String): BoolExpr = ???

  def updateRowZ3(ctx: Context, incrementValue: IncrementValue, isMaterialized: Boolean, z3Prefix: String): BoolExpr = ???
}
