package temporal

/**
 * Temporal property AST for Past-time LTL formulas.
 * 
 * Supported operators:
 * - ONCE φ: φ held at some point in the past (including now)
 * - ALWAYS φ: φ has always held (including now)
 * - NOT φ: negation
 * - φ AND ψ: conjunction
 * - φ OR ψ: disjunction
 * - φ IMPLY ψ: implication
 */
sealed trait TemporalExpr {
  override def toString: String = this match {
    case TemporalExpr.Identifier(name) => name
    case TemporalExpr.NumericLiteral(value) => value.toString
    case TemporalExpr.BoolLiteral(value) => value.toString
    case TemporalExpr.FunctionCall(name, args) => 
      s"$name(${args.mkString(", ")})"
    case TemporalExpr.Not(expr) => s"NOT ($expr)"
    case TemporalExpr.And(left, right) => s"($left AND $right)"
    case TemporalExpr.Or(left, right) => s"($left OR $right)"
    case TemporalExpr.Imply(left, right) => s"($left IMPLY $right)"
    case TemporalExpr.Eq(left, right) => s"($left == $right)"
    case TemporalExpr.Neq(left, right) => s"($left != $right)"
    case TemporalExpr.Lt(left, right) => s"($left < $right)"
    case TemporalExpr.Le(left, right) => s"($left <= $right)"
    case TemporalExpr.Gt(left, right) => s"($left > $right)"
    case TemporalExpr.Ge(left, right) => s"($left >= $right)"
    case TemporalExpr.Once(expr) => s"ONCE ($expr)"
    case TemporalExpr.Always(expr) => s"ALWAYS ($expr)"
  }
}

object TemporalExpr {
  case class Identifier(name: String) extends TemporalExpr
  case class NumericLiteral(value: BigInt) extends TemporalExpr
  case class BoolLiteral(value: Boolean) extends TemporalExpr
  case class FunctionCall(name: String, args: List[TemporalExpr]) extends TemporalExpr
  
  // Logical operators
  case class Not(expr: TemporalExpr) extends TemporalExpr
  case class And(left: TemporalExpr, right: TemporalExpr) extends TemporalExpr
  case class Or(left: TemporalExpr, right: TemporalExpr) extends TemporalExpr
  case class Imply(left: TemporalExpr, right: TemporalExpr) extends TemporalExpr
  
  // Comparison operators
  case class Eq(left: TemporalExpr, right: TemporalExpr) extends TemporalExpr
  case class Neq(left: TemporalExpr, right: TemporalExpr) extends TemporalExpr
  case class Lt(left: TemporalExpr, right: TemporalExpr) extends TemporalExpr
  case class Le(left: TemporalExpr, right: TemporalExpr) extends TemporalExpr
  case class Gt(left: TemporalExpr, right: TemporalExpr) extends TemporalExpr
  case class Ge(left: TemporalExpr, right: TemporalExpr) extends TemporalExpr
  
  // Temporal operators
  case class Once(expr: TemporalExpr) extends TemporalExpr
  case class Always(expr: TemporalExpr) extends TemporalExpr
}

/**
 * A single temporal property with metadata.
 */
case class TemporalProperty(
  line: Int,
  expr: TemporalExpr,
  rawText: String,
  comment: Option[String] = None
) {
  override def toString: String = {
    val commentStr = comment.map(c => s"// $c\n").getOrElse("")
    s"$commentStr$expr"
  }
}





