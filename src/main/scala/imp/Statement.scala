package imp
import datalog._
import util.Indenter._

sealed abstract class Statement

case class Empty() extends Statement {
  override def toString: String = s"// Empty()"
}
case class GroundVar(p: Parameter, relation: Relation, index: Int) extends Statement {
  // override def toString: String = s"${p._type} $p = ${relation.name}[$index];"
  override def toString: String = {
    relation match {
      case _:SingletonRelation => s"${p._type} $p = ${relation.name}.${relation.memberNames(index)};"
      case _:MsgSender => s"${p._type} $p = msg.sender;"
      case _:MsgValue => s"${p._type} $p = msg.value;"
      case _:SimpleRelation => s"${p._type} $p = ${relation.name}Tuple.${relation.memberNames(index)};"
    }
  }
}
case class Assign(p: Param, arithmetic: Arithmetic) extends Statement {
  // override def toString: String = s"$p := $arithmetic"
  override def toString: String = s"${p.p._type} $p = $arithmetic;"
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
  def literal: Literal
  def relation: Relation
  def updateTarget: Relation
  def statement: Statement
}

case class OnInsert(literal: Literal, updateTarget: Relation, statement: Statement) extends OnStatement {
  val relation: Relation = literal.relation
  override def toString: String =
    e"""on insert $literal {
  $statement
}""".stripMargin
}
case class OnIncrement(literal: Literal, keyIndices: List[Int], updateIndex: Int, updateTarget: Relation,
                       statement: Statement) extends OnStatement {
  val relation: Relation = literal.relation
  val keys: List[Parameter] = keyIndices.map(i => literal.fields(i))
  val updateValue = literal.fields(updateIndex)
  override def toString: String = {
    val keyStr = keys.mkString(",")
    e"""on increment ${relation.name} at $keyStr with $updateValue  {
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
    val keyStr = relation match {
      case SimpleRelation(name, sig, memberNames) => {
        val keys = keyIndices.map(i => literal.fields(i))
        "[" + keys.mkString(",") + "]"
      }
      case _:SingletonRelation => ""
      case r: ReservedRelation => throw new Exception(s"Cannot update reserved relation $r")
    }
    val fieldName = relation.memberNames(valueIndex)
    delta match {
      case Negative(e) => s"${relation.name}$keyStr.$fieldName -= $e;"
      case _ => s"${relation.name}$keyStr.$fieldName += $delta;"
    }
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
case class FunctionMetaData(publicity: Publicity.Publicity, isView: Boolean, isTransaction: Boolean) {
  override def toString: String = {
    val publicityStr = publicity match {
      case Publicity.Public => "public"
      case Publicity.Private => "private"
    }
    if (isView) {
      List(publicityStr, "view").mkString(" ")
    }
    else publicityStr
  }
}
sealed abstract class SolidityStatement extends Statement
case class Constructor(params: List[Parameter], statement: Statement) extends SolidityStatement {
  override def toString: String = {
    val paramStr = params.map(p => s"${p._type} ${p.name}").mkString(",")
    e"""constructor($paramStr) public {
  $statement
}""".stripMargin
  }
}
case class ReadTuple(relation: Relation, keyList: List[Parameter]) extends SolidityStatement{
  require(relation.isInstanceOf[SingletonRelation] || keyList.nonEmpty)
  override def toString: String = {
    val tupleName: String = s"${relation.name}Tuple"
    val keyStr: String = keyList.map(k=>s"[$k]").mkString("")
    s"${tupleName.capitalize} memory $tupleName = ${relation.name}$keyStr;"
  }
}
case class SetTuple(relation: SingletonRelation, params: List[Parameter]) extends SolidityStatement {
  override def toString: String = {
    val structType = s"${relation.name.capitalize}Tuple"
    val paramStr = params.mkString(",")
    s"${relation.name} = $structType($paramStr);"
  }
}
case class DeclFunction(name: String, params: List[Parameter], returnType: Type, stmt: Statement,
                        metaData: FunctionMetaData)
    extends SolidityStatement{
  override def toString: String = {
    val paramStr = params.map(p => s"${p._type} ${p.name}").mkString(",")
    val returnStr: String = returnType match {
      case _ @ (_:UnitType| _:AnyType) => ""
      case t @ (_: SymbolType |_: NumberType|_:BooleanType|_:CompoundType) => s"returns (${t.name})"
    }
    e"""function $name($paramStr) $metaData $returnStr {
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
case class DeclRelation(relation: Relation, _type: Type) extends SolidityStatement {
  override def toString: String = s"${_type} ${relation.name};"
}
case class DeclContract(name: String, statement: Statement) extends SolidityStatement {
  override def toString: String =
    e"""contract $name {
  $statement
}""".stripMargin
}
case class Return(p: Parameter) extends SolidityStatement {
  override def toString: String = s"return $p;"
}
case class Require(condition: Condition, msg: String) extends SolidityStatement {
  override def toString: String = s"require($condition,\"$msg\");"
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
  override def toString: String = {
    relation match {
      case _: MsgSender => s"$p==msg.sender"
      case _: MsgValue => s"$p==msg.value"
      case _:SingletonRelation|_:SimpleRelation => {
        val key = relation.memberNames(index)
        s"$p==${relation.name}.$key"
      }
    }
  }
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
