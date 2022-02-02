case class Type(name: String) {
  override def toString: String = name
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

case class SimpleRelation(name: String, sig: List[Type]) extends Relation
case class SingletonRelation(name: String, sig: List[Type]) extends Relation

case class Literal(relation: Relation, fields: List[Parameter]) {
  override def toString: String = {
    val fieldStr = fields.mkString(",")
    s"${relation.name}($fieldStr)"
  }
}
case class Rule(head: Literal, body: Set[Literal]) {
  override def toString: String = {
    val bodyStr = body.mkString(s",")
    s"$head :- $bodyStr."
  }
}

case class Interface(name: String, inputTypes: List[Type], returnType: Option[Type]) {
  override def toString: String = {
    val inputStr = inputTypes.mkString(",")
    val retStr = returnType match {
      case Some(rt) => s": $rt"
      case None => s""
    }
    s"$name($inputStr)" + retStr
  }
}
case class Program(rules: Set[Rule], interfaces: Set[Interface]) {
  override def toString: String = interfaces.mkString("\n") + "\n" + rules.mkString("\n")
}
