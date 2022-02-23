package datalog

sealed abstract class Aggregator {
  def literal: Literal
  def aggResult: Parameter
  def relation: Relation = literal.relation
  require(!literal.fields.contains(aggResult))
}

case class Sum(literal: Literal, aggParam: Variable, aggResult: Variable) extends Aggregator {
  require(literal.fields.contains(aggParam))
  val valueIndex: Int = literal.fields.indexOf(aggParam)
  override def toString: String = s"$aggResult = sum $aggParam: $literal"
}
case class Max(literal: Literal, aggParam: Variable, aggResult: Variable) extends Aggregator {
  require(literal.fields.contains(aggParam))
  val valueIndex: Int = literal.fields.indexOf(aggParam)
  override def toString: String = s"$aggResult = max $aggParam: $literal"
}
case class Count(literal: Literal, aggResult: Variable) extends Aggregator {
  override def toString: String = s"$aggResult = count: $literal"
}
