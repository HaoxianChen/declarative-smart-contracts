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
      case _:Now => s"${p._type} $p = block.timestamp;"
      case _:Send => throw new Exception("Send should not appear in body.")
      case _:SimpleRelation => s"${p._type} $p = ${relation.name}Tuple.${relation.memberNames(index)};"
    }
  }
}
case class Assign(p: Param, expr: Expr) extends Statement {
  // override def toString: String = s"$p := $arithmetic"
  override def toString: String = s"${p.p._type} $p = $expr;"
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
  def ruleId: Int
}

case class OnInsert(literal: Literal, updateTarget: Relation, statement: Statement, ruleId: Int) extends OnStatement {
  val relation: Relation = literal.relation
  override def toString: String =
    e"""on insert $literal {
  $statement
}""".stripMargin
}
case class OnDelete(literal: Literal, updateTarget: Relation, statement: Statement, ruleId: Int) extends OnStatement {
  val relation: Relation = literal.relation
  override def toString: String =
    e"""on delete $literal {
  $statement
}""".stripMargin
}
case class OnIncrement(literal: Literal, keyIndices: List[Int], updateIndex: Int, updateTarget: Relation,
                       statement: Statement, ruleId: Int) extends OnStatement {
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
}
case class Insert(literal: Literal) extends UpdateStatement {
  val relation = literal.relation
  override def toString: String = s"insert $literal"
}
case class Delete(literal: Literal) extends UpdateStatement {
  val relation = literal.relation
  override def toString: String = s"delete $literal"
}
case class DeleteByKeys(relation: Relation, keys: List[Parameter], updateTarget: Relation) extends UpdateStatement {
  override def toString: String = {
    val keyStr = keys.map(k=>s"[$k]").mkString("")
    s"update $updateTarget on delete ${relation.name}$keyStr"
  }
}
case class IncrementAndInsert(increment: Increment) extends UpdateStatement {
  val relation = increment.relation
}
case class UpdateDependentRelations(update: UpdateStatement) extends Statement {
  override def toString: String = s"update dependent relations on $update"
}
case class Increment(relation: Relation, literal: Literal, keyIndices: List[Int], valueIndex: Int, delta: Arithmetic)
  extends UpdateStatement {
  val valueType = relation.sig(valueIndex)
  val keyParams = keyIndices.map(i => literal.fields(i))
  override def toString: String = {
    val keyStr = relation match {
      case SimpleRelation(name, sig, memberNames) => {
        val keys = keyIndices.map(i => literal.fields(i))
        keys.map(k=>s"[$k]").mkString("")
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
case class Search(relation: Relation, conditions: Set[MatchRelationField], statement: Statement) extends Statement {
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
case class FunctionMetaData(publicity: Publicity.Publicity, isView: Boolean, isTransaction: Boolean,
                            modifiers: Set[String]) {
  override def toString: String = {
    val publicityStr = publicity match {
      case Publicity.Public => "public"
      case Publicity.Private => "private"
    }
    val viewStr = if (isView) "view" else ""
    val modifierStr = modifiers.mkString(" ")
    List(publicityStr, viewStr, modifierStr).mkString(" ")
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
case class ReadTuple(relation: Relation, keyList: List[Parameter],
                     outputVar: String) extends SolidityStatement{
  require(relation.isInstanceOf[SingletonRelation] || keyList.nonEmpty)
  override def toString: String = {
    val tupleName: String = s"${relation.name}Tuple"
    val keyStr: String = keyList.map(k=>s"[$k]").mkString("")
    s"${tupleName.capitalize} memory $outputVar = ${relation.name}$keyStr;"
  }
}
object ReadTuple {
  def apply(relation: Relation, keyList: List[Parameter]): ReadTuple = {
    // val tupleName: String = s"${relation.name}Tuple"
    val tupleName: String = DataStructureHelper.relationalTupleName(relation)
    ReadTuple(relation, keyList, tupleName)
  }
}

case class ReadArray(arrayName: String, iterator: Parameter, outputVar: Variable) extends SolidityStatement {
  override def toString: String = s"${outputVar._type} memory ${outputVar.name} = $arrayName[$iterator];"
}
case class ReadValueFromMap(relation: Relation, keyList: List[Parameter],
                            output: Parameter) extends SolidityStatement {
  override def toString: String = {
    val keyStr: String = keyList.map(k=>s"[$k]").mkString("")
    s"${output._type} ${output.name} = ${relation.name}$keyStr;"
  }
}
case class UpdateMap(name: String, keys: List[Parameter], tupleTypeName: String, params: List[Parameter])
  extends SolidityStatement {
  override def toString: String = {
    val keyStr: String = keys.map(k=>s"[$k]").mkString("")
    val paramStr: String = params.mkString(",")
    s"$name$keyStr = $tupleTypeName($paramStr);"
  }
}
case class UpdateMapValue(name: String, keys: List[Parameter], fieldName: String, p: Parameter)
  extends SolidityStatement {
  override def toString: String = {
    val keyStr: String = keys.map(k=>s"[$k]").mkString("")
    s"$name$keyStr.$fieldName = $p;"
  }
}
case class SetTuple(relation: SingletonRelation, params: List[Parameter]) extends SolidityStatement {
  override def toString: String = {
    val structType = s"${relation.name.capitalize}Tuple"
    val paramStr = params.mkString(",")
    s"${relation.name} = $structType($paramStr);"
  }
}
case class ConvertType(from: Arithmetic, to: Variable) extends SolidityStatement {
  private val t = to._type
  override def toString: String = s"$t $to = $t($from);"
}
object ConvertType {
  def apply(from: Variable, to: Variable): ConvertType = ConvertType(Param(from),to)
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
case class DeclEvent(name: String, params: List[Parameter]) extends SolidityStatement {
  override def toString: String = {
    val paramStr = params.map(p=>s"${p._type} ${p.name}").mkString(",")
    s"event $name($paramStr);"
  }
}
case class DeclModifier(name: String, params: List[Parameter], beforeStatement: Statement,
                        afterStatement: Statement) extends SolidityStatement {
  override def toString: String = {
    val paramStr = params.map(p => s"${p._type} ${p.name}").mkString(",")
    e"""modifier $name($paramStr) {
    $beforeStatement
    _;
    $afterStatement
}"""
  }
}
case class Call(functionName: String, params: List[Parameter], optReturnVar: Option[Variable] = None) extends SolidityStatement {
  override def toString: String = {
    val paramStr = params.map {
      case v: Variable => s"$v"
      case c: Constant => s"${c._type}(${c.name})"
    }.mkString(",")
    val returnStr = optReturnVar match {
      case Some(v) => s"${v._type} ${v.name} = "
      case None => ""
    }
    s"$returnStr$functionName($paramStr);"
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
case class DeclVariable(name: String, _type: Type) extends SolidityStatement {
  override def toString: String = s"${_type} $name;"
}
case class DeclContract(name: String, statement: Statement) extends SolidityStatement {
  override def toString: String =
    e"""contract $name {
  $statement
}""".stripMargin
}
case class ForLoop(iterator: Variable, initValue: Arithmetic, loopCondition: Condition,
               nextValue: Arithmetic,
               statement: Statement) extends SolidityStatement {
  override def toString: String = {
    e"""for(${iterator._type} $iterator = $initValue; $loopCondition; $iterator = $nextValue) {
    $statement
}""".stripMargin
  }
}
case class GetObjectAttribute(objectName: String, attributeName: String, ret: Parameter) extends SolidityStatement {
  override def toString: String = s"${ret._type} ${ret.name} = $objectName.$attributeName;"
}
case class CallObjectMethod(objectName: String, methodName: String, params: List[String],
                            optRet: Option[Variable] = None) extends SolidityStatement {
  override def toString: String = {
    val paramStr = params.mkString(",")
    optRet match {
      case Some(ret) => s"${ret._type} ${ret.name} = $objectName.$methodName($paramStr);"
      case None => s"$objectName.$methodName($paramStr);"
    }
  }
}
case class Return(p: Parameter) extends SolidityStatement {
  override def toString: String = s"return $p;"
}
case class Require(condition: Condition, msg: String) extends SolidityStatement {
  override def toString: String = s"require($condition,\"$msg\");"
}
case class Revert(msg: String) extends SolidityStatement {
  override def toString: String = s"revert(\"$msg\");"
}
case class SendEther(p: Parameter, amount: Parameter) extends SolidityStatement {
  require(p._type == Type.addressType)
  override def toString: String = s"payable($p).send($amount);"
}
case class Emit(event: String, parameters: List[Parameter]) extends SolidityStatement {
  override def toString: String = {
    val paramStr = parameters.mkString(",")
    s"emit $event($paramStr);"
  }
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
case class InsertTuple(relation: Relation, keyIndices: List[Int]) extends Trigger {
  override def toString: String = s"insert ${relation.name}"
}
case class DeleteTuple(relation: Relation, keyIndices: List[Int]) extends Trigger {
  override def toString: String = s"delete ${relation.name}"
}
case class ReplacedByKey(relation: Relation, keyIndices: List[Int], targetRelation: Relation) extends Trigger {
  override def toString: String = s"update $targetRelation on delete ${relation.name}"
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
case class MatchRelationField(relation: Relation, index: Int, p: Parameter) extends Condition {
  require(0 <= index && index < relation.sig.size, s"index out of bound $relation, $index, $p")
  override def toString: String = {
    relation match {
      case _: MsgSender => s"$p==msg.sender"
      case _: MsgValue => s"$p==msg.value"
      case _: Now => s"$p==block.timestamp"
      case _: Send => throw new Exception(s"Send relation should not be matched.")
      case _:SingletonRelation => {
        val key = relation.memberNames(index)
        s"$p==${relation.name}.$key"
      }
      case _:SimpleRelation => {
        val key = relation.memberNames(index)
        s"$p==${relation.name}Tuple.$key"
      }
    }
  }
}
case class Match(a: Expr, b: Expr) extends Condition {
  override def toString: String = s"$a==$b"
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
case class Unequal(a: Expr, b: Expr) extends Condition {
  override def toString: String = s"$a!=$b"
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
  def makeConjunction(conditions: Condition*): Condition = conditions.foldLeft[Condition](True())(conjunction)
}

case class ImperativeAbstractProgram(name: String, relations: Set[Relation], indices: Map[SimpleRelation, List[Int]],
                                     onStatements: Set[OnStatement],
                                     dependencies: Map[Relation, Set[Relation]],
                                     rules: Set[Rule]) {
  override def toString: String = onStatements.mkString("\n")
}
