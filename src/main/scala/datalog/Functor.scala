package datalog

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

