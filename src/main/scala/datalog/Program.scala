package datalog

import imp.SolidityTranslator.transactionRelationPrefix

sealed abstract class Parameter {
  def _type: Type
  def name: String
  override def toString: String = name
  def setType(newType: Type): Parameter
}

case class Constant(_type: Type, name: String) extends Parameter {
  def setType(newType: Type): Constant = this.copy(_type=newType)

  override def toString: String = _type match {
    case SymbolType(t) => s"${t}($name)"
    case _ => s"$name"
  }
}
object Constant {
  val CTrue = Constant(BooleanType(), "true")
  val CFalse = Constant(BooleanType(), "false")
}
case class Variable(_type: Type, name: String) extends Parameter {
  def setType(newType: Type): Variable = this.copy(_type=newType)
}

sealed abstract class Relation {
  def name: String
  def sig: List[Type]
  def memberNames: List[String]
  require(sig.size == memberNames.size)

  def paramList: List[Parameter] = sig.zip(memberNames).map {
    case (t,n) => Variable(t,n)
  }
}
object Relation {
  val reservedRelations: Set[Relation] = Set(
    MsgSender(), MsgValue(), Now(), Send(), Balance(), Receive(), This(), Transaction()
  )
}

case class SimpleRelation(name: String, sig: List[Type], memberNames: List[String]) extends Relation
case class SingletonRelation(name: String, sig: List[Type], memberNames: List[String]) extends Relation
sealed abstract class ReservedRelation extends Relation
case class Balance() extends ReservedRelation {
  def name: String = "thisBalance"
  def sig: List[Type] = List(Type.uintType)
  def memberNames: List[String] = List("n")
}
case class MsgSender() extends ReservedRelation {
  def name: String = "msgSender"
  def sig: List[Type] = List(Type.addressType)
  def memberNames: List[String] = List("p")
}
case class MsgValue() extends ReservedRelation {
  def name: String = "msgValue"
  def sig: List[Type] = List(Type.uintType)
  def memberNames: List[String] = List("p")
}
case class Now() extends ReservedRelation {
  def name: String = "now"
  def sig: List[Type] = List(Type.uintType)
  def memberNames: List[String] = List("time")
}
case class Send() extends ReservedRelation {
  def name: String = "send"
  def sig: List[Type] = List(Type.addressType, Type.uintType)
  def memberNames: List[String] = List("p", "amount")
}
case class Receive() extends ReservedRelation {
  def name: String = "receive"
  def sig: List[Type] = List(Type.addressType, Type.uintType)
  def memberNames: List[String] = List("p", "amount")
}
case class This() extends ReservedRelation {
  def name: String = "this"
  def sig: List[Type] = List(Type.addressType)
  def memberNames: List[String] = List("p")
}
case class Transaction() extends ReservedRelation {
  def name: String = "transaction"
  def sig: List[Type] = List(SymbolType("String"))
  def memberNames: List[String] = List("name")
}

case class Literal(relation: Relation, fields: List[Parameter]) {
  override def toString: String = {
    val fieldStr = fields.mkString(",")
    s"${relation.name}($fieldStr)"
  }

  def rename(mapping: Map[Parameter,Parameter]): Literal = {
    val newFields = fields.map(x => mapping.getOrElse(x,x))
    this.copy(fields=newFields)
  }
}

case class Rule(head: Literal, body: Set[Literal], functors: Set[Functor], aggregators: Set[Aggregator]) {
  override def toString: String = {
    val litStr = body.map(_.toString)
    val functorStr = functors.map(_.toString)
    val aggStr = aggregators.map(_.toString)
    val bodyStr = (litStr++functorStr++aggStr).mkString(",")
    s"$head :- $bodyStr."
  }

  /** todo: mask ungrounded variable with underscore. */

  def groundedParams: Set[Parameter] = {
    val allParams = (head.fields ++ body.flatMap(_.fields)).toSet.filterNot(_.name=="_")
    allParams
  }
}

case class Interface(relation: Relation, inputIndices: List[Int], optReturnIndex: Option[Int]) {
  def inputTypes: List[Type] = inputIndices.map(i => relation.sig(i))
  def returnType: Type = optReturnIndex match {
    case Some(i) => relation.sig(i)
    case None => UnitType()
  }
  override def toString: String = {
    val inputStr = inputTypes.mkString(",")
    val retStr = returnType match {
      case _: AnyType => throw new Exception(s"Interface ${relation} does not return Any type.")
      case _: UnitType => s""
      case _:SymbolType|_:NumberType|_:BooleanType|_:CompoundType => s": $returnType"
    }
    s"${relation.name}($inputStr)" + retStr
  }
}
case class Program(rules: Set[Rule], interfaces: Set[Interface], relationIndices: Map[SimpleRelation, List[Int]],
                   functions: Set[Relation],
                   violations: Set[Relation],
                   name: String = "Contract0") {
  val relations = rules.flatMap(r => r.body.map(_.relation) + r.head.relation)
  override def toString: String = {
    var ret: String = s""
    ret += "Interfaces:\n" + interfaces.mkString("\n") + "\n"
    ret += "Indices:\n" + relationIndices.mkString("\n") + "\n"
    ret += "Rules:\n" + rules.mkString("\n")
    ret
  }
  def setName(newName: String): Program = this.copy(name=newName)

  def transactionRules(): Set[Rule] = rules.filter(_.body.exists(
                                         _.relation.name.startsWith(transactionRelationPrefix)))

  def addRules(newRules: Set[Rule]): Program = this.copy(rules = this.rules++newRules)
}
