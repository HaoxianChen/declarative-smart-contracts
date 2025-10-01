package synthesis

import datalog.{Add, AnyType, ArithOperator, Arithmetic, Assign, BinaryOperator, BooleanType, CompoundType, Constant, Div, Equal, Expr, Functor, Geq, Greater, Leq, Lesser, Min, Mul, Negative, NumberType, One, Param, Parameter, Rule, Sub, SymbolType, Type, Unequal, UnitType, Variable, Zero}
import synthesis.PredicateEnumerator.extractTxLiteral

import scala.collection.mutable

case class State() {
  val state: mutable.Map[Variable, Int] = mutable.Map()

  def lookup(variable: Variable): Int = {
    state(variable)
  }

  def update(variable: Variable, value: Constant): Unit = variable._type match {
    case _: NumberType => state(variable) = value.name.toInt
    case _: BooleanType => state(variable) = if (value.name == "1") 1 else 0
    case _: SymbolType => state(variable) = value.name.toInt
    case _ => throw new IllegalArgumentException(s"Unsupported variable type: ${variable._type}")
  }

  /** Apply bindings for the duration of `thunk`, then restore previous state. */
  def withTemporaryBindings[R](bindings: Seq[(Variable, Constant)])(thunk: => R): R = {
    val saved: Map[Variable, Option[Int]] = bindings.map { case (v, _) => v -> state.get(v) }.toMap
    try {
      for ((v, c) <- bindings) update(v, c)
      thunk
    } finally {
      for ((v, opt) <- saved) {
        opt match {
          case Some(value) => state(v) = value
          case None => state.remove(v)
        }
      }
    }
  }
}
object State {
  // make new object from a state
  def apply(s: State): State = {
    val newState = State()
    for ((k,v) <- s.state) {
      newState.state(k) = v
    }
    newState
  }
}

case class Interpreter() {
  def evaluate(state: State, transaction: Transaction, predicate: Predicate): Boolean = {
    require(transaction.relation == predicate.context.tx.relation)
    require(transaction.parameters.length == predicate.context.tx.fields.length)

    val bindings = predicate.context.tx.fields.zip(transaction.parameters).collect {
      case (v: Variable, c) => (v, c)
    }

    state.withTemporaryBindings(bindings) {
      evaluate(state, predicate.functor)
    }
  }

  def evaluate(state: State, functor: Functor): Boolean = functor match {
    case operator: ArithOperator => operator match {
      case Greater(a: Arithmetic, b: Arithmetic) => evalTerm(state, a) > evalTerm(state, b)
      case Lesser(a: Arithmetic, b: Arithmetic) => evalTerm(state, a) < evalTerm(state, b)
      case Geq(a: Arithmetic, b: Arithmetic) => evalTerm(state, a) >= evalTerm(state, b)
      case Leq(a: Arithmetic, b: Arithmetic) => evalTerm(state, a) <= evalTerm(state, b)
    }
    case Unequal(a: Expr, b: Expr) => evalTerm(state, a) != evalTerm(state, b)
    case Equal(a: Expr, b: Expr) => evalTerm(state, a) == evalTerm(state, b)
    case Assign(a: Expr, b: Expr) => throw new UnsupportedOperationException("Assign is not supported in evaluate")
  }

  private def evalTerm(state: State, term: Expr): Int = term match {
    case arith: Arithmetic => arith match {
      case Zero(_type) => 0
      case One(_type) => 1
      case Param(p) => evalParam(state, p)
      case Negative(e) => - evalTerm(state, e)
      case operator: BinaryOperator => operator match {
        case Add(a, b) => evalTerm(state, a) + evalTerm(state, b)
        case Sub(a, b) => evalTerm(state, a) - evalTerm(state, b)
        case Mul(a, b) => evalTerm(state, a) * evalTerm(state, b)
        case Div(a, b) => evalTerm(state, a) / evalTerm(state, b)
        case Min(a, b) => math.min(evalTerm(state, a), evalTerm(state, b))
      }
    }
    case _ => throw new IllegalArgumentException("Unsupported term type")
  }

  private def evalParam(state: State, p: Parameter): Int = p match {
    case Constant(_type, name) => name.toInt
    case Variable(_type, name) => state.lookup(Variable(_type, name))
  }


}


object Interpreter {
  def test1(rule: Rule, predicates: Set[Predicate]): Unit = {
    val txLiteral = extractTxLiteral(rule)
    val params: List[Constant] = txLiteral.relation.sig.map(randomConstant)
    val tx = Transaction(txLiteral.relation, params)

    println(s"Tx: ${tx}")
    val interpreter = Interpreter()
    val state = State()
    for (p <- predicates) {
      val result = interpreter.evaluate(state, tx ,p)
      println(s"$p : $result")
    }
  }

  def randomConstant(_type: Type): Constant = _type match {
    case _: NumberType => Constant(_type, scala.util.Random.nextInt(100).toString)
    case _: BooleanType => Constant(_type, if (scala.util.Random.nextBoolean()) "1" else "0")
    case _: SymbolType => Constant(_type, scala.util.Random.nextInt(100).toString)
    case _ => throw new IllegalArgumentException(s"Unsupported type: ${_type}")
  }


}