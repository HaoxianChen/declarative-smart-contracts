package imp
import datalog._
import util.Indenter._

sealed abstract class Statement

case class Empty() extends Statement
case class GroundVar(p: Parameter, relation: Relation, index: Int) extends Statement {
  override def toString: String = s"$p := ${relation.name}[$index]"
}
case class Seq(a: Statement, b: Statement) extends Statement {
  override def toString: String = s"$a\n$b"
}
case class If(condition: Condition, statement: Statement) extends Statement {
  override def toString: String =
    e"""if($condition) {
  $statement
}""".stripMargin
}
case class On(trigger: Trigger, statement: Statement) extends Statement {
  override def toString: String =
    e"""on $trigger {
  $statement
}""".stripMargin
}
// Update
sealed abstract class UpdateStatement extends Statement
case class Insert(literal: Literal) extends UpdateStatement {
  override def toString: String = s"insert $literal"
}
case class Increment(relation: Relation, keys: List[Parameter], valueIndex: Int, delta: Int) extends UpdateStatement {
  override def toString: String = ???
}
// Join
case class Search(relation: Relation, condition: Condition, statement: Statement) extends Statement {
  override def toString: String =
    e"""search ${relation.name} where $condition {
  $statement
}""".stripMargin
}

object Statement {
  def makeSeq(a: Statement, b: Statement): Statement = a match {
    case _: Empty => b
    case _ => b match {
      case _: Empty => a
      case _ => Seq(a,b)
    }
  }
}

// On insert / increment, do statement
sealed abstract class Trigger {
  def relation: Relation
}
case class InsertTuple(relation: Relation) extends Trigger {
  override def toString: String = s"insert ${relation.name}"
}
case class IncrementValue(relation: Relation, keyIndices: List[Int], valueIndex: Int, delta: Int) extends Trigger {
  override def toString: String = ???
}

sealed abstract class Condition
case class True() extends Condition
case class False() extends Condition
case class Match(relation: Relation, index: Int, p: Parameter) extends Condition {
  override def toString: String = s"$p==${relation.name}[$index]"
}
case class Greater(a: Parameter, b: Parameter) extends Condition {
  override def toString: String = s"$a>$b"
}
case class Lesser(a: Parameter, b: Parameter) extends Condition {
  override def toString: String = s"$a<$b"
}
case class Geq(a: Parameter, b: Parameter) extends Condition {
  override def toString: String = s"$a>=$b"
}
case class Leq(a: Parameter, b: Parameter) extends Condition {
  override def toString: String = s"$a<=$b"
}
case class And(a: Condition, b: Condition) extends Condition {
  override def toString: String = s"$a && $b"
}
case class Or(a: Condition, b: Condition) extends Condition {
  override def toString: String = s"$a || $b"
}
object Condition {
  def conjunction(a: Condition, b: Condition): Condition = a match {
    case _: False => False()
    case _: True => b
    case _ => b match {
      case _: True => a
      case _: False => False()
      case _ => And(a,b)
    }
  }
  def disjunction(a: Condition, b: Condition): Condition = a match {
    case _:True => True()
    case _:False => b
    case _ => b match {
      case _: True => True()
      case _: False => a
      case _ => Or(a,b)
    }
  }
}
