package datalog

sealed abstract class Type {
  def name: String
  override def toString: String = name
}
case class UnitType() extends Type {
  val name:String = "Unit"
}
case class AnyType() extends Type {
  val name: String = "Any"
}
case class SymbolType(name: String) extends Type
case class NumberType(name: String) extends Type
object Type {
  def apply(name: String): Type = name match {
    case "int" => integerType
    case "uint" => uintType
    case _ => SymbolType(name)
  }
  val addressType: Type = SymbolType("address")
  val integerType: Type = NumberType("int")
  val uintType: Type = NumberType("uint")
}

sealed abstract class Parameter {
  def _type: Type
  def name: String
  override def toString: String = name
}

case class Constant(_type: Type, name: String) extends Parameter
case class Variable(_type: Type, name: String) extends Parameter

sealed abstract class Relation {
  def name: String
  def sig: List[Type]
}
object Relation {
  val reservedRelations: Set[Relation] = Set(
    SingletonRelation("msgSender", List(Type.addressType))
  )
}

case class SimpleRelation(name: String, sig: List[Type]) extends Relation
case class SingletonRelation(name: String, sig: List[Type]) extends Relation

case class Literal(relation: Relation, fields: List[Parameter]) {
  override def toString: String = {
    val fieldStr = fields.mkString(",")
    s"${relation.name}($fieldStr)"
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
}

case class Interface(relation: Relation, inputTypes: List[Type], returnType: Option[Type]) {
  override def toString: String = {
    val inputStr = inputTypes.mkString(",")
    val retStr = returnType match {
      case Some(rt) => s": $rt"
      case None => s""
    }
    s"${relation.name}($inputStr)" + retStr
  }
}
case class Program(rules: Set[Rule], interfaces: Set[Interface], relationIndices: Map[SimpleRelation, Int]) {
  override def toString: String = {
    var ret: String = s""
    ret += "Interfaces:\n" + interfaces.mkString("\n") + "\n"
    ret += "Indices:\n" + relationIndices.mkString("\n") + "\n"
    ret += "Rules:\n" + rules.mkString("\n")
    ret
  }
}
