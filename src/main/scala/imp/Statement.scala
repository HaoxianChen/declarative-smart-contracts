package imp
import datalog._
import util.Indenter._

sealed abstract class Statement

case class Empty() extends Statement
case class GroundVar(p: Parameter, relation: Relation, index: Int) extends Statement {
  // override def toString: String = s"${p._type} $p = ${relation.name}[$index];"
  override def toString: String = {
    s"${p._type} $p = ${relation.name}Tuple.${relation.memberNames(index)};"
  }
}
case class Assign(p: Param, arithmetic: Arithmetic) extends Statement {
  override def toString: String = s"$p := $arithmetic"
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
sealed abstract class OnStatement extends Statement {
  def updateTarget: Relation
  def statement: Statement
}

case class OnInsert(literal: Literal, updateTarget: Relation, statement: Statement) extends OnStatement {
  override def toString: String =
    e"""on insert $literal {
  $statement
}""".stripMargin
}
case class OnIncrement(relation: Relation, keys: List[Parameter], updateValue: Param, updateTarget: Relation,
                       statement: Statement) extends OnStatement {
  override def toString: String = {
    val keyStr = keys.mkString(",")
    e"""on increment ${relation.name} at $keyStr on $updateValue  {
  $statement
}""".stripMargin
  }
}
// Update
sealed abstract class UpdateStatement extends Statement {
  def relation: Relation
  def literal: Literal
}
case class Insert(literal: Literal) extends UpdateStatement {
  val relation = literal.relation
  override def toString: String = s"insert $literal"
}
case class Increment(relation: Relation, literal: Literal, keyIndices: List[Int], valueIndex: Int, delta: Arithmetic)
  extends UpdateStatement {
  override def toString: String = {
    val keyStr = {
      val keys = keyIndices.map(i => literal.fields(i))
      keys.mkString(",")
    }
    val fieldName = relation.memberNames(valueIndex)
    s"${relation.name}[$keyStr].$fieldName += $delta;"
  }
}
// Join
case class Search(relation: Relation, conditions: Set[Match], statement: Statement) extends Statement {
  override def toString: String = {
    val conditionStr = conditions.mkString("&&")
    e"""search ${relation.name} where $conditionStr {
  $statement
}""".stripMargin
  }
}

/** Statements used in Solidity */
object Publicity extends Enumeration {
  type Publicity = Value
  val Public, Private = Value
}
sealed abstract class SolidityStatement extends Statement
case class ReadTuple(relation: SimpleRelation, key: Parameter) extends SolidityStatement{
  override def toString: String = {
    val tupleName: String = s"${relation.name}Tuple"
    s"${tupleName.capitalize} $tupleName = ${relation.name}[$key];"
  }
}
case class DeclFunction(name: String, params: List[Parameter], returnType: Type, stmt: Statement,
                        publicity: Publicity.Publicity)
    extends SolidityStatement{
  override def toString: String = {
    val paramStr = params.map(p => s"${p._type} ${p.name}").mkString(",")
    val returnStr: String = returnType match {
      case _ @ (_:UnitType| _:AnyType) => ""
      case t @ (_: SymbolType | _: NumberType|_:CompoundType) => s"returns (${t.name})"
    }
    val publicityStr = publicity match {
      case Publicity.Public => "public"
      case Publicity.Private => "private"
    }
    e"""function $name($paramStr) $publicityStr $returnStr {
  $stmt
}""".stripMargin
  }
}
case class Call(functionName: String, params: List[Parameter]) extends SolidityStatement {
  override def toString: String = {
    val paramStr = params.mkString(",")
    s"$functionName($paramStr);"
  }
}
case class DefineStruct(name: String, _type: StructType) extends SolidityStatement {
  override def toString: String = {
    val defineMembers = _type.members.map(p => s"${p._type} ${p.name};").mkString("\n")
    e"""struct ${_type.name} {
  $defineMembers
}"""
  }
}
case class DeclRelation(relation: Relation, mapType: MapType) extends SolidityStatement {
  override def toString: String = s"$mapType ${relation.name};"
}
case class DeclContract(name: String, statement: Statement) extends SolidityStatement {
  override def toString: String =
    e"""contract $name {
  $statement
}""".stripMargin
}

object Statement {
  private def _makeSeq(a: Statement, b: Statement): Statement = a match {
    case _: Empty => b
    case _ => b match {
      case _: Empty => a
      case _ => Seq(a,b)
    }
  }
  def makeSeq(statementSeq: Statement*): Statement = statementSeq.foldLeft[Statement](Empty())(_makeSeq)
}

// On insert / increment, do statement
sealed abstract class Trigger {
  def relation: Relation
}
case class InsertTuple(relation: Relation) extends Trigger {
  override def toString: String = s"insert ${relation.name}"
}
case class IncrementValue(relation: Relation, keyIndices: List[Int], valueIndex: Int, delta: Arithmetic)
  extends Trigger {
  override def toString: String = {
    val keyStr = keyIndices.mkString(",")
    s"${relation.name}[$keyStr] += $delta"
  }
}

sealed abstract class Condition
case class True() extends Condition {
  override def toString: String = "true"
}
case class False() extends Condition {
  override def toString: String = "false"
}
case class Match(relation: Relation, index: Int, p: Parameter) extends Condition {
  override def toString: String = s"$p==${relation.name}[$index]"
}
case class Greater(a: Arithmetic, b: Arithmetic) extends Condition {
  override def toString: String = s"$a>$b"
}
case class Lesser(a: Arithmetic, b: Arithmetic) extends Condition {
  override def toString: String = s"$a<$b"
}
case class Geq(a: Arithmetic, b: Arithmetic) extends Condition {
  override def toString: String = s"$a>=$b"
}
case class Leq(a: Arithmetic, b: Arithmetic) extends Condition {
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

case class ImperativeAbstractProgram(name: String, relations: Set[Relation], indices: Map[SimpleRelation, Int],
                                     statement: Statement,
                                     dependencies: Map[Relation, Set[Relation]]) {
  override def toString: String = s"$statement"
}
