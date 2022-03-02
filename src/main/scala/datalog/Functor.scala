package datalog

sealed abstract class Arithmetic {
  def _type: Type
  def _paren(e: Arithmetic): String = e match {
    case _:Param|_:One|_:Zero => s"$e"
    case _ => s"($e)"
  }
}
case class Zero(_type: Type) extends Arithmetic {
  override def toString: String = _type match {
    case _:UnitType|_:AnyType|_:NumberType => s"0"
    case SymbolType(name) => s"$name(0)"
    case BooleanType() => s"false"
    case compoundType: CompoundType => throw new Exception(s"illegal type $compoundType for Zero.")
  }
}
case class One(_type: Type) extends Arithmetic {
  override def toString: String = _type match {
    case _:UnitType|_:AnyType|_:NumberType => s"1"
    case SymbolType(name) => s"$name(1)"
    case BooleanType() => s"true"
    case compoundType: CompoundType => throw new Exception(s"illegal type $compoundType for Zero.")
  }
}
case class Param(p: Parameter) extends Arithmetic {
  val _type = p._type
  override def toString: String = p match {
    case Constant(_type, name) => _type match {
      case SymbolType(t) => s"${t}($name)"
      case _ => s"$name"
    }
    case Variable(_type, name) => name
  }
}
case class Negative(e: Arithmetic) extends Arithmetic {
  val _type = e._type
  override def toString: String = s"-${_paren(e)}"
}
sealed abstract class BinaryOperator extends Arithmetic {
  def a: Arithmetic
  def b: Arithmetic
}
case class Add(a: Arithmetic, b: Arithmetic) extends BinaryOperator {
  require(a._type == b._type)
  val _type = a._type
  override def toString: String = s"${_paren(a)}+${_paren(b)}"
}
case class Sub(a: Arithmetic, b:Arithmetic) extends BinaryOperator {
  require(a._type == b._type)
  val _type = a._type
  override def toString: String = s"${_paren(a)}-${_paren(b)}"
}
case class Mul(a: Arithmetic, b: Arithmetic) extends BinaryOperator {
  require(a._type == b._type, s"$a,$b")
  val _type = a._type
  override def toString: String = s"${_paren(a)}*${_paren(b)}"
}
object Arithmetic {
  def derivativeOf(e: Arithmetic, x: Param): Arithmetic = e match {
    case Zero(t) => Zero(t)
    case One(t) => Zero(t)
    case Param(p) => if (p.name==x.p.name) One(p._type) else Zero(p._type)
    case Negative(e2) => Negative(derivativeOf(e2,x))
    case Add(a,b) => Add(derivativeOf(a,x), derivativeOf(b,x))
    case Sub(a,b) => Sub(derivativeOf(a,x), derivativeOf(b,x))
    case Mul(a,b) => Mul(derivativeOf(a,x), derivativeOf(b,x))
  }
  def simplify(expr: Arithmetic): Arithmetic = {
    def _simplify(expr: Arithmetic): Arithmetic = expr match {
      case a @ (_:Zero|_:One|_:Param) => a
      case Negative(Negative(a)) => _simplify(a)
      case Negative(a) => Negative(_simplify(a))
      case Add(Zero(_),b) => _simplify(b)
      case Add(a,Zero(_)) => _simplify(a)
      case Add(a,b) => Add(_simplify(a),_simplify(b))
      case Sub(Zero(_),b) => Negative(_simplify(b))
      case Sub(a, Zero(_)) => _simplify(a)
      case Sub(a,b) => Sub(_simplify(a),_simplify(b))
      case Mul(One(_),b) => _simplify(b)
      case Mul(Negative(One(_)),b) => Negative(_simplify(b))
      case Mul(a,One(_)) => _simplify(a)
      case Mul(a, Negative(One(_))) => Negative(_simplify(a))
      case Mul(a,b) => Mul(_simplify(a),_simplify(b))
    }

    var e1 = expr
    var e2 = expr
    do {
      e1 = e2
      e2 = _simplify(e1)
    } while (e2!=e1)
    e2
  }
}

sealed abstract class Functor {
  def a: Arithmetic
  def b: Arithmetic
}
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
case class Unequal(a: Arithmetic, b: Arithmetic) extends Functor {
  override def toString: String = s"$a!=$b"
}
case class Assign(a: Param, b: Arithmetic) extends Functor {
  override def toString: String = s"$a := $b"
  def updateOutputType(outputType: Type): Assign = {
    val newP = a.p.setType(outputType)
    val newA = Param(newP)
    this.copy(a=newA)
  }
}

