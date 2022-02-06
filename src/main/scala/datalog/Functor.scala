package datalog

sealed abstract class Expression
case class Param(p: Parameter) extends Expression {
  override def toString: String = s"$p"
}
case class Negative(e: Expression) extends Expression {
  override def toString: String = s"-$e"
}
case class Plus(a: Expression, b: Expression) extends Expression {
  override def toString: String = s"$a+$b"
}
case class Minus(a: Expression, b:Expression) extends Expression {
  override def toString: String = s"$a-$b"
}

sealed abstract class Functor
case class Greater(a: Parameter, b: Parameter) extends Functor {
  override def toString: String = s"$a>$b"
}
case class Lesser(a: Parameter, b: Parameter) extends Functor {
  override def toString: String = s"$a<$b"
}
case class Geq(a: Parameter, b: Parameter) extends Functor {
  override def toString: String = s"$a>=$b"
}
case class Leq(a: Parameter, b: Parameter) extends Functor {
  override def toString: String = s"$a<=$b"
}

