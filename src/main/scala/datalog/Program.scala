package datalog

case class Type(name: String) {
  override def toString: String = name
}
object Type {
  val integerType = Type("Int")
  val any = Type("Any")
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
  val reservedNames: Set[String] = Set("datalog.Greater", "datalog.Lesser", "msgSender")
}

case class SimpleRelation(name: String, sig: List[Type]) extends Relation
case class SingletonRelation(name: String, sig: List[Type]) extends Relation

case class Literal(relation: Relation, fields: List[Parameter]) {
  override def toString: String = {
    val fieldStr = fields.mkString(",")
    s"${relation.name}($fieldStr)"
  }
}

sealed abstract class Functor
case class Greater(a: Parameter, b: Parameter) extends Functor {
  override def toString: String = s"$a>$b"
}
case class Lesser(a: Parameter, b: Parameter) extends Functor {
  override def toString: String = s"$a<$b"
}

case class Rule(head: Literal, body: Set[Literal], functors: Set[Functor]) {
  override def toString: String = {
    val bodyStr = body.mkString(",")
    val functorStr = functors.mkString(",")
    s"$head :- $bodyStr,$functorStr."
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
case class Program(rules: Set[Rule], interfaces: Set[Interface], relationIndices: Map[Relation, Int]) {
  override def toString: String = {
    var ret: String = s""
    ret += "Interfaces:\n" + interfaces.mkString("\n") + "\n"
    ret += "Indices:\n" + relationIndices.mkString("\n") + "\n"
    ret += "Rules:\n" + rules.mkString("\n")
    ret
  }
}
