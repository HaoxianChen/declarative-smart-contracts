package synthesis

import datalog.{Add, AnyType, ArithOperator, Arithmetic, Assign, BinaryOperator, BooleanType, CompoundType, Constant, Div, Equal, Expr, Functor, Geq, Greater, Leq, Lesser, Min, Mul, Negative, NumberType, One, Param, Parameter, ReservedRelation, Rule, SimpleRelation, SingletonRelation, Sub, SymbolType, Type, Unequal, UnitType, Variable, Zero}
import synthesis.PredicateEnumerator.extractTxLiteral

import scala.collection.mutable

case class State() {
  val state: mutable.Map[String, Int] = mutable.Map()
  // Refactored: mapping key is now SimpleRelation
  val maps: mutable.Map[String, mutable.Map[Vector[Int], Int]] = mutable.Map()


  def lookup(variableName: String): Int = {
    state.get(variableName).getOrElse(0)
  }

  // Backward-compatible single-key lookup
  def lookup(relation: SimpleRelation, key: Int): Int =
    lookup(relation, Seq(key))

  // Multi-key lookup: defaults to 0 if missing
  def lookup(relation: SimpleRelation, keys: Seq[Int]): Int =
    lookup(relation.name, keys)

  // Multi-key lookup: defaults to 0 if missing
  def lookup(relationName: String, keys: Seq[Int]): Int =
    maps.get(relationName).flatMap(_.get(keys.toVector)).getOrElse(0)

  def update(variable: Variable, value: Constant): Unit = {
    val variableName = variable.name
    variable._type match {
      case _: NumberType => state(variableName) = value.name.toInt
      case _: BooleanType => state(variableName) = if (value.name == "1") 1 else 0
      case _: SymbolType => state(variableName) = value.name.toInt
      case _ => throw new IllegalArgumentException(s"Unsupported variable type: ${variable._type}")
    }
  }

  def updateInt(varName: String, value: Int): Unit = state(varName) = value
  def updateBoolean(varName: String, value: Boolean): Unit = {
    state(varName) = if (value) 1 else 0
  }
  def updateSymbol(varName: String, value: String): Unit = state(varName) = value.toInt

  // Backward-compatible single-key update
  def update(relation: SimpleRelation, key: Int, value: Constant): Unit =
    update(relation, Seq(key), value)

  // Multi-key update
  def update(relation: SimpleRelation, keys: Seq[Int], value: Constant): Unit = {
    val m = maps.getOrElseUpdate(relation.name, mutable.Map())
    m(keys.toVector) = constantToInt(value)
  }

  // Multi-key update
  def update(relationName: String, keys: Seq[Int], value: Int): Unit = {
    val m = maps.getOrElseUpdate(relationName, mutable.Map())
    m(keys.toVector) = value
  }

  /** Apply bindings for the duration of `thunk`, then restore previous state. */
  def withTemporaryBindings[R](bindings: Seq[State.Binding])(thunk: => R): R = {
    // Save old scalar values
    val savedScalars: Seq[(Variable, Option[Int])] =
      bindings.collect { case State.Binding.Scalar(v, _) => v }
        .distinct
        .map(v => (v, state.get(v.name)))

    // Save map elements (composite keys)
    val mapElemSaves: Seq[(SimpleRelation, Vector[Int], Option[Int])] = bindings.collect {
      case State.Binding.MapElem(rel, keys, _) =>
        val keyVec = resolveParamsToInts(keys)
        val old = maps.get(rel.name).flatMap(_.get(keyVec))
        (rel, keyVec, old)
    }

    try {
      // Apply scalar bindings
      bindings.collect { case State.Binding.Scalar(v, c) => (v, c) }
        .foreach { case (v, c) => update(v, c) }

      // Apply map elements
      bindings.collect { case State.Binding.MapElem(rel, keys, c) => (rel, keys, c) }
        .foreach { case (rel, keys, c) =>
          val keyVec = resolveParamsToInts(keys)
          update(rel, keyVec, c)
        }

      thunk
    } finally {
      // Restore scalars
      savedScalars.foreach {
        case (v, Some(value)) => state(v.name) = value
        case (v, None)        => state.remove(v.name)
      }
      // Restore map elements
      mapElemSaves.foreach {
        case (rel, keyVec, Some(value)) =>
          val m = maps.getOrElseUpdate(rel.name, mutable.Map())
          m(keyVec) = value
        case (rel, keyVec, None) =>
          maps.get(rel.name).foreach { m =>
            m.remove(keyVec)
            if (m.isEmpty) maps.remove(rel.name)
          }
      }
    }
  }

  // Helpers
  private def constantToInt(value: Constant): Int = value.name match {
    case "1" => 1
    case "0" => 0
    case s   => s.toInt
  }

  private def resolveParamToInt(p: Parameter): Int = p match {
    case Constant(_, name)   => name.toInt
    case v: Variable         => lookup(v.name)
  }

  private def resolveParamsToInts(ps: Seq[Parameter]): Vector[Int] =
    ps.map(resolveParamToInt).toVector
}
object State {
  // Bindings supported by withTemporaryBindings
  sealed trait Binding
  object Binding {
    final case class Scalar(variable: Variable, value: Constant) extends Binding
    // MapElem now uses SimpleRelation as key
    final case class MapElem(relation: SimpleRelation, keys: Seq[Parameter], value: Constant) extends Binding
  }
}

case class Interpreter(interpreterContext: InterpreterContext) {
  /** keep only materialized relation in the map */
  private val relationIndices = interpreterContext.relationIndices.filter {
    case (rel, _) => interpreterContext.materializedRelations.contains(rel)
  }

  def evaluate(state: State, transaction: Transaction, predicate: Predicate): Boolean = {
    require(transaction.relation == predicate.context.tx.relation)
    require(transaction.parameters.length == predicate.context.tx.fields.length)

    val bindings: Seq[State.Binding] = makeBindings(state, predicate.context, transaction)

    state.withTemporaryBindings(bindings) {
      evaluate(state, predicate.functor)
    }
  }

  private def makeBindings(state: State, context: Context, transaction: Transaction): Seq[State.Binding] = {
    // Step 1: Scalar bindings for tx fields
    val scalarBindings: Seq[State.Binding] =
      context.tx.fields.zip(transaction.parameters).collect {
        case (v: Variable, c: Constant) => State.Binding.Scalar(v, c)
      }
    // Step 1b: Bind implicit parameters (msgSender and msgValue)
    val msgSenderVar = Context.msgSender.fields.head.asInstanceOf[Variable]
    val msgValueVar = Context.msgValue.fields.head.asInstanceOf[Variable]
    val msgSenderConst = Constant(datalog.Type.uintType, transaction.implicitParameters.msgSender.toString)
    val msgValueConst = Constant(datalog.Type.uintType, transaction.implicitParameters.value.toString)
    val implicitBindings: Seq[State.Binding] = Seq(
      State.Binding.Scalar(msgSenderVar, msgSenderConst),
      State.Binding.Scalar(msgValueVar, msgValueConst)
    )
    val allScalarBindings = scalarBindings ++ implicitBindings
    // Build a map from variable to its bound value from step 1 and implicit bindings
    val scalarBindingMap: Map[Variable, Constant] =
      allScalarBindings.collect { case State.Binding.Scalar(v, c) => v -> c }.toMap

    // Step 2: For each binding literal, bind the value variable to the lookup result from state
    val mapBindings = {
      val bindings = mutable.Buffer[State.Binding]()
      for (literal <- context.bindingLiterals) {
        val rel = literal.relation match {
          case r: SimpleRelation => r
          case _ => throw new IllegalArgumentException(s"Unsupported relation type: ${literal.relation}")
        }
        val keyIndices = relationIndices.getOrElse(rel,
          throw new IllegalArgumentException(s"Missing indices for relation: $rel"))
        // Evaluate key parameters from transaction (may be Constant or Variable)
        val keyParams = keyIndices.map(literal.fields(_))
        val keyInts = keyParams.map {
          case c: Constant => c.name.toInt
          case v: Variable =>
            scalarBindingMap.get(v).map(_.name.toInt).getOrElse(state.lookup(v.name))
        }
        // Find the value variable (the field not in keyIndices)
        val valueIndices = literal.fields.indices.diff(keyIndices)
        if (valueIndices.size != 1)
          throw new IllegalArgumentException(s"Expected exactly one value field for relation: $literal, found: ${valueIndices.size}")
        val valueIdx = valueIndices.head
        val valueVar = literal.fields(valueIdx) match {
          case v: Variable => v
          case other => throw new IllegalArgumentException(s"Value field must be Variable, got: $other")
        }
        // Lookup the value from state
        val lookupValue = state.lookup(rel, keyInts)
        // Bind valueVar to the looked up value as a Constant
        val valueType = valueVar._type
        val valueConst = Constant(valueType, lookupValue.toString)
        bindings += State.Binding.Scalar(valueVar, valueConst)
      }
      bindings.toSeq
    }
    allScalarBindings ++ mapBindings
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
    case Variable(_type, name) => state.lookup(name)
  }


}


object Interpreter {
  def test1(interpreterContext: InterpreterContext, rule: Rule, predicates: Set[Predicate]): Unit = {
    val txLiteral = extractTxLiteral(rule)
    val params: List[Constant] = txLiteral.relation.sig.map(randomConstant)
    val tx = Transaction(txLiteral.relation, params, ImplicitParameters())

    println(s"Tx: ${tx}")
    val interpreter = Interpreter(interpreterContext)
    val state = State()
    for (p <- predicates) {
      val result = interpreter.evaluate(state, tx ,p)
      println(s"$p : $result")
    }
  }

  def test2(interpreterContext: InterpreterContext, rule: Rule, predicates: Set[Predicate]): Unit = {
    // Small universe of addresses
    val addresses: List[String] = List("1", "2", "3")
    val addressConstants: List[Constant] = addresses.map(a => Constant(SymbolType("address"), a))

    // For each indexed relation, update state for all key combinations
    val state = State()
    for (rel <- interpreterContext.materializedRelations.collect { case r: SimpleRelation if interpreterContext.relationIndices.contains(r) => r }) {
      val indices = interpreterContext.relationIndices(rel)
      val keyTypes = indices.map(rel.sig(_))
      val valueIdx = rel.sig.indices.diff(indices).head
      val valueType = rel.sig(valueIdx)

      // Generate all key combinations (cartesian product)
      val keyUniverse: List[List[Constant]] = List.fill(keyTypes.size)(addressConstants)
      val keyCombos: List[List[Constant]] = keyUniverse.foldLeft(List(List.empty[Constant])) {
        (acc, curr) => for (a <- acc; b <- curr) yield a :+ b
      }
      for (keyParams <- keyCombos) {
        val valueParam = Interpreter.randomConstant(valueType)
        state.update(rel, keyParams.map(_.name.toInt), valueParam)
      }
    }

    // Make a random transaction using these addresses
    val txRel = rule.body.collectFirst { case lit if lit.relation.isInstanceOf[SimpleRelation] => lit.relation.asInstanceOf[SimpleRelation] }.getOrElse(
      throw new IllegalArgumentException("No SimpleRelation found in rule body"))
    val txParams: List[Constant] = txRel.sig.map {
      case _: SymbolType => addressConstants(scala.util.Random.nextInt(addressConstants.size))
      case t => Interpreter.randomConstant(t)
    }
    val tx = Transaction(txRel, txParams, ImplicitParameters())

    val interpreter = Interpreter(interpreterContext)
    for (p <- predicates) {
      val result = interpreter.evaluate(state, tx, p)
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