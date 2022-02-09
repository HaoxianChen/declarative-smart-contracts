package datalog

sealed abstract class Arithmetic {
  def _paren(e: Arithmetic): String = e match {
    case _: Param => s"$e"
    case _ => s"($e)"
  }
}
case class Zero() extends Arithmetic {
  override def toString: String = "0"
}
case class One() extends Arithmetic {
  override def toString: String = "1"
}
case class Param(p: Parameter) extends Arithmetic {
  override def toString: String = s"$p"
}
case class Negative(e: Arithmetic) extends Arithmetic {
  override def toString: String = s"${_paren(e)}"
}
case class Add(a: Arithmetic, b: Arithmetic) extends Arithmetic {
  override def toString: String = s"${_paren(a)}+${_paren(b)}"
}
case class Sub(a: Arithmetic, b:Arithmetic) extends Arithmetic {
  override def toString: String = s"${_paren(a)}-${_paren(b)}"
}
case class Mul(a: Arithmetic, b: Arithmetic) extends Arithmetic {
  override def toString: String = s"${_paren(a)}*${_paren(b)}"
}
object Arithmetic {
  def derivativeOf(e: Arithmetic, x: Param): Arithmetic = e match {
    case Zero() => Zero()
    case One() => Zero()
    case Param(p) => if (p.name==x.p.name) One() else Zero()
    case Negative(e2) => Negative(derivativeOf(e2,x))
    case Add(a,b) => Add(derivativeOf(a,x), derivativeOf(b,x))
    case Sub(a,b) => Sub(derivativeOf(a,x), derivativeOf(b,x))
    case Mul(a,b) => Mul(derivativeOf(a,x), derivativeOf(b,x))
  }
}

sealed abstract class Functor
case class Greater(a: Arithmetic, b: Arithmetic) extends Functor {
  override def toString: String = s"$a>$b"
}
case class Lesser(a: Arithmetic, b: Arithmetic) extends Functor {
  override def toString: String = s"$a<$b"
}
case class Geq(a: Arithmetic, b: Arithmetic) extends Functor {
  override def toString: String = s"$a>=$b"
}
case class Leq(a: Arithmetic, b: Arithmetic) extends Functor {
  override def toString: String = s"$a<=$b"
}
case class Assign(a: Param, b: Arithmetic) extends Functor {
  override def toString: String = s"$a := $b"
  def updateOutputType(outputType: Type): Assign = {
    val newP = a.p.setType(outputType)
    val newA = Param(newP)
    this.copy(a=newA)
  }
}

