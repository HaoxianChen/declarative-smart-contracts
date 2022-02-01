case class Type(name: String)

sealed abstract class Parameter {
  def _type: Type
  def name: String
  override def toString: String = name
}

case class Constant(_type: Type, name: String) extends Parameter
case class Variable(_type: Type, name: String) extends Parameter

case class Relation(name: String, sig: List[Type])
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
case class Program(rules: Set[Rule]) {
  override def toString: String = rules.mkString("\n")
}
