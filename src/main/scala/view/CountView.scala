package view

import com.microsoft.z3.{BoolExpr, Context, Expr, Sort}
import datalog.{Arithmetic, Count, Literal, Negative, One, Param, Relation, Rule, Variable}
import imp.{DeleteTuple, Empty, Increment, IncrementValue, Insert, InsertTuple, OnDelete, OnInsert, OnStatement, Statement, Trigger}

case class CountView(rule: Rule, primaryKeyIndices: List[Int], ruleId: Int) extends View {
  require(rule.aggregators.size==1)
  require(rule.aggregators.head.isInstanceOf[Count])
  val count: Count = rule.aggregators.head.asInstanceOf[Count]

  /** Interfaces */
  def insertRow(insertTuple: InsertTuple): OnStatement = {
    val insertedLiteral = getInsertedLiteral(insertTuple.relation)
    val resultIndex = rule.head.fields.indexOf(count.aggResult)
    val delta: Arithmetic = {
      val d = One(count.aggResult._type)
      Arithmetic.updateArithmeticType(d, View.getDeltaType(d._type))
    }
    val statement = {
      val delete = if (isDeleteBeforeInsert(insertTuple.relation, insertTuple.keyIndices)) {
        deleteByKeysStatement(insertedLiteral, insertTuple.keyIndices)
      }
      else {
        Empty()
      }
      val increment = Increment(rule.head.relation, rule.head, primaryKeyIndices, resultIndex, delta = delta)
      Statement.makeSeq(delete, increment)
    }
    OnInsert(insertedLiteral, rule.head.relation, statement, ruleId)
  }

  def deleteRow(deleteTuple: DeleteTuple): OnStatement = {
    val deletedLiteral = getInsertedLiteral(deleteTuple.relation)
    val resultIndex = rule.head.fields.indexOf(count.aggResult)
    val delta: Arithmetic = {
      val d = Negative(One(count.aggResult._type))
      Arithmetic.updateArithmeticType(d, View.getDeltaType(d._type))
    }
    val decrement = Increment(rule.head.relation, rule.head, primaryKeyIndices, resultIndex, delta = delta)
    OnDelete(deletedLiteral, rule.head.relation, statement = decrement, ruleId)
  }

  def updateRow(incrementValue: IncrementValue): OnStatement = ???

  protected def getInsertedLiteral(relation: Relation): Literal = {
    require(relation==count.relation)
    val memberNames = relation.memberNames
    val fields = count.literal.fields.zipWithIndex.map{
      case (p,i) => {
        val name = if (p.name == "_") s"_${memberNames(i)}$i" else p.name
        Variable(p._type,name)
      }
    }
    Literal(relation, fields)
  }

  /** Interfaces to generate Z3 constraints */
  def getNextTriggers(trigger: Trigger): Set[Trigger] = ???

  /** Interfaces to generate Z3 constraints */
  def insertRowZ3(ctx: Context, insertTuple: InsertTuple, isMaterialized: Boolean, z3Prefix: String): (BoolExpr, BoolExpr, Array[(Expr[Sort], Expr[Sort], Expr[_ <: Sort])]) = ???

  def updateRowZ3(ctx: Context, incrementValue: IncrementValue, isMaterialized: Boolean, z3Prefix: String): (BoolExpr, BoolExpr, Array[(Expr[Sort], Expr[Sort], Expr[_ <: Sort])]) = ???
}
