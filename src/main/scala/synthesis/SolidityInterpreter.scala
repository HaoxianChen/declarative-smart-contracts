package synthesis

import com.microsoft.z3.FuncDecl
import datalog.{Add, AnyType, Arithmetic, Balance, BinaryOperator, BooleanType, CompoundType, Constant, Div, Expr, Literal, Min, MsgSender, MsgValue, Mul, Negative, Now, NumberType, One, Param, Parameter, Program, Receive, Relation, ReservedRelation, Rule, Send, SimpleRelation, SingletonRelation, Sub, SymbolType, This, UnitType, Variable, Zero}
import imp.{And, Assign, BooleanFunction, Call, CallObjectMethod, Condition, Constructor, ConvertType, DeclContract, DeclEvent, DeclFunction, DeclModifier, DeclVariable, DefineStruct, Emit, False, ForLoop, Geq, GetObjectAttribute, Greater, GroundVar, If, Increment, Leq, Lesser, Match, MatchRelationField, Or, ReadArray, ReadTuple, ReadValueFromMap, Require, Return, Revert, SendEther, SetTuple, SolidityStatement, Statement, True, Unequal, UpdateMap, UpdateMapValue, Empty}
import synthesis.SolidityInterpreter.{msgSenderName, msgValueName}

/**
 * A small, modular Solidity interpreter.
 *
 * Current behavior (minimal implementation):
 * - Accepts a SolidityStatement and a Trace
 * - Produces an EvaluatedTrace where each transaction is paired with the
 *   resulting State after executing the statement for that transaction.
 * - Right now the interpreter is intentionally conservative: it does not
 *   modify the state (no-op semantics), but it returns deep-copied State
 *   snapshots so the returned trace is immutable from the caller's view.
 *
 * This class is kept small and modular: extend `evaluateTransaction` to add
 * actual semantics for specific SolidityStatement subclasses (UpdateMap,
 * SetTuple, SendEther, etc.).
 */
case class SolidityInterpreter(programOpt: Option[datalog.Program] = None,
                               externalFunctions: String = "") {

  /** Interpret a Solidity statement on a given trace and produce an EvaluatedTrace.
    * statement: the Solidity AST (currently unused by the no-op interpreter)
    * trace: the input sequence of transactions
    */
  def interpret(statement: Map[String, SolidityStatement], trace: Trace): EvaluatedTrace = {
    if (trace == null || trace.isEmpty) return EvaluatedTrace.empty

    // initial (empty) state
    val initialState = State()

    // iterate over transactions, apply statement (no-op by default), and
    // collect resulting state snapshots
    val steps = trace.steps.scanLeft((None: Option[Transaction], initialState)) {
      case ((_, prevState), tx) =>
        // Clone previous state to produce an isolated snapshot for the next state
        val stateAfter = cloneState(prevState)
        // Evaluate transaction against statement -- extension point
        evaluateTransaction(statement(tx.relation.name).asInstanceOf[DeclFunction], tx, stateAfter)
        // Compute and store UDF (function relation) values derived from the updated state
        programOpt.foreach(p => evaluateFunctionRelations(stateAfter, p))
        (Some(tx), stateAfter)
    }.collect { case (Some(tx), st) => (tx, st) }

    EvaluatedTrace(initialState, steps)
  }

  /**
   * Evaluate all `.function`-marked relations in the program against the current state
   * and store the computed values back into the state so they can be looked up during
   * predicate evaluation.
   *
   * For each function relation we find its defining rule(s), enumerate bindings by
   * iterating over the "driving" indexed body literal, evaluate any arithmetic functors,
   * and write the result into state.maps under the head relation key.
   *
   * When `externalFunctions` is non-empty, any function relation whose name matches a
   * function defined there is evaluated via `evaluateFunctionFromSolidity` instead of
   * (in addition to) the Datalog rule path.
   */
  private def evaluateFunctionRelations(state: State, program: datalog.Program): Unit = {
    // Parse external Solidity functions once per call (cheap for the small files used here)
    val solFuncs: Map[String, (String, String)] = if (externalFunctions.nonEmpty)
      parseSolidityFunctions(externalFunctions)
    else
      Map.empty

    for (rel <- program.functions) {
      rel match {
        case sr: SimpleRelation =>
          val keyIndices = program.relationIndices.getOrElse(sr, Nil)
          solFuncs.get(sr.name) match {
            case Some((paramName, returnExpr)) =>
              // Use Solidity expression evaluator for this relation
              evaluateFunctionFromSolidity(state, sr, paramName, returnExpr)
            case None =>
              // Fall back to Datalog rule evaluation
              val defRules = program.rules.filter(_.head.relation == rel)
              for (rule <- defRules) {
                evaluateFunctionRule(state, program, rule, sr, keyIndices)
              }
          }
        case _ => // singleton function relations not handled here
      }
    }
  }

  /**
   * Parse a block of Solidity function definitions and return a map from function name
   * to (firstParamName, returnExpression) for each `internal view returns` function.
   *
   * Only handles the simple pattern:
   *   function <name>(<type> <param>) internal view returns (...) {
   *       return <expr>;
   *   }
   */
  private def parseSolidityFunctions(src: String): Map[String, (String, String)] = {
    val result = scala.collection.mutable.Map[String, (String, String)]()
    // Match: function <name>(<type> <param>) ... { return <expr>; }
    val funcPattern = """function\s+(\w+)\s*\(([^)]*)\)[^{]*\{[^}]*return\s+([^;]+);[^}]*\}""".r
    for (m <- funcPattern.findAllMatchIn(src)) {
      val funcName  = m.group(1)
      val paramList = m.group(2).trim
      val returnExpr = m.group(3).trim
      // Extract first parameter name (last whitespace-separated token of first param declaration)
      val firstParamName = paramList.split(",").headOption.map(_.trim.split("\\s+").last).getOrElse("")
      if (firstParamName.nonEmpty) {
        result(funcName) = (firstParamName, returnExpr)
      }
    }
    result.toMap
  }

  /**
   * Evaluate a Solidity-native UDF for relation `rel` by:
   * 1. Iterating over all keys in any keyed map referenced in `returnExpr`
   * 2. For each key, evaluating the arithmetic expression
   * 3. Storing the result in state.maps(rel.name)
   *
   * The `paramName` is the parameter variable in the function signature (e.g. "p").
   * The `returnExpr` is the body of the return statement.
   */
  private def evaluateFunctionFromSolidity(state: State, rel: SimpleRelation,
                                           paramName: String, returnExpr: String): Unit = {
    // Find all keyed-map references [<paramName>] in the expression to drive iteration.
    // E.g. shares[p] -> "shares" is the driver relation.
    val driverPattern = s"""(\\w+)\\[$paramName\\]""".r
    val driverRelNames = driverPattern.findAllMatchIn(returnExpr).map(_.group(1)).toList.distinct

    // Use the first driver relation found to iterate over known keys
    val driverRelName = driverRelNames.headOption.getOrElse("")
    val keySet: Iterable[Int] = if (driverRelName.nonEmpty) {
      state.maps.get(driverRelName).map(_.keys.flatMap(_.headOption)).getOrElse(Iterable.empty)
    } else {
      Iterable.empty
    }

    for (keyVal <- keySet) {
      val bindings = scala.collection.mutable.Map[String, Int](paramName -> keyVal)
      val result = evalSolArith(returnExpr, bindings.toMap, state)
      state.update(rel.name, Seq(keyVal), result)
    }
  }

  /**
   * Evaluate a simple Solidity arithmetic expression string.
   *
   * Supported constructs:
   *   - Integer literals
   *   - `int256(x)` / `uint256(x)` casts — evaluate x
   *   - Bare variable name — look up in `bindings` first, then `state` scalar
   *   - `relName[varName]` — look up in state.maps by key from bindings
   *   - Binary operators: `+`, `-`, `*`, `/` (left-to-right, `*` and `/` before `+` and `-`)
   *   - Parentheses
   *
   * The evaluator uses a straightforward recursive-descent approach on the token stream.
   */
  private def evalSolArith(expr: String, bindings: Map[String, Int], state: State): Int = {
    // Tokenize: numbers, identifiers, operators, brackets, parens
    val tokenPattern = """(\d+|[a-zA-Z_]\w*|\[|\]|\(|\)|\+|-|\*|/)""".r
    val tokens = tokenPattern.findAllIn(expr.trim).toArray
    var pos = 0

    def peek: Option[String] = if (pos < tokens.length) Some(tokens(pos)) else None
    def consume(): String = { val t = tokens(pos); pos += 1; t }

    // Forward declarations via var
    var parseExpr: () => Int = null
    var parseTerm: () => Int = null
    var parseFactor: () => Int = null

    // expr = term (('+' | '-') term)*
    parseExpr = () => {
      var left = parseTerm()
      while (peek.contains("+") || peek.contains("-")) {
        val op = consume()
        val right = parseTerm()
        left = if (op == "+") left + right else left - right
      }
      left
    }

    // term = factor (('*' | '/') factor)*
    parseTerm = () => {
      var left = parseFactor()
      while (peek.contains("*") || peek.contains("/")) {
        val op = consume()
        val right = parseFactor()
        left = if (op == "*") left * right else if (right == 0) 0 else left / right
      }
      left
    }

    // factor = number | cast | mapAccess | identifier | '(' expr ')'
    parseFactor = () => {
      peek match {
        case Some(t) if t.forall(_.isDigit) =>
          consume(); t.toInt
        case Some(t) if t.matches("[a-zA-Z_]\\w*") =>
          // Could be: cast like int256(...) or uint256(...), mapAccess like shares[p], or bare var
          consume() // consume identifier
          peek match {
            case Some("(") =>
              // Cast: int256(x) or similar — evaluate inner expression
              consume() // consume '('
              val inner = parseExpr()
              if (peek.contains(")")) consume() // consume ')'
              inner
            case Some("[") =>
              // Map access: relName[keyExpr]
              consume() // consume '['
              val keyVal = parseExpr()
              if (peek.contains("]")) consume() // consume ']'
              state.maps.get(t).flatMap(_.get(Vector(keyVal))).getOrElse(0)
            case _ =>
              // Bare variable: look up in bindings, then state scalar
              bindings.getOrElse(t, state.lookup(t))
          }
        case Some("(") =>
          consume() // consume '('
          val v = parseExpr()
          if (peek.contains(")")) consume() // consume ')'
          v
        case Some("-") =>
          consume()
          -parseFactor()
        case _ => 0
      }
    }

    parseExpr()
  }

  private def evaluateFunctionRule(
      state: State,
      program: datalog.Program,
      rule: datalog.Rule,
      headRel: SimpleRelation,
      keyIndices: List[Int]): Unit = {

    val headKeyVars: List[datalog.Parameter] = keyIndices.map(rule.head.fields(_))
    val headValueIdx  = rule.head.fields.indices.diff(keyIndices).headOption.getOrElse(0)
    val headValueVar  = rule.head.fields(headValueIdx)

    val bodyLits = rule.body.toList

    // Collect singleton bindings (variables from SingletonRelation body literals)
    val singletonBindings = scala.collection.mutable.Map[String, Int]()
    for (lit <- bodyLits) {
      lit.relation match {
        case sr: SingletonRelation =>
          lit.fields.headOption.collect { case v: Variable => v }.foreach { v =>
            singletonBindings(v.name) = state.lookupSingleton(sr.name)
          }
        case _ =>
      }
    }

    // Collect indexed body literals
    val indexedLits = bodyLits.collect { case lit if lit.relation.isInstanceOf[SimpleRelation] => lit }

    // Find the "driver": an indexed literal whose key variable also appears in the head keys
    val headKeyVarNames = headKeyVars.collect { case v: Variable => v.name }.toSet
    val driverLitOpt = indexedLits.find { lit =>
      val litRel = lit.relation.asInstanceOf[SimpleRelation]
      val litKeyIdxs = program.relationIndices.getOrElse(litRel, Nil)
      litKeyIdxs.map(lit.fields(_)).collect { case v: Variable => v.name }.exists(headKeyVarNames.contains)
    }

    driverLitOpt.foreach { driverLit =>
      val driverRel     = driverLit.relation.asInstanceOf[SimpleRelation]
      val driverKeyIdxs = program.relationIndices.getOrElse(driverRel, Nil)
      val driverValueIdx = driverLit.fields.indices.diff(driverKeyIdxs).headOption.getOrElse(-1)

      val driverMap = state.maps.getOrElse(driverRel.name, scala.collection.mutable.Map.empty)

      for ((keyVec, driverValue) <- driverMap) {
        val iterBindings = singletonBindings.clone()

        // Bind driver key variables
        driverKeyIdxs.zip(keyVec).foreach { case (idx, keyVal) =>
          driverLit.fields(idx) match {
            case v: Variable => iterBindings(v.name) = keyVal
            case _ =>
          }
        }
        // Bind driver value variable
        if (driverValueIdx >= 0) {
          driverLit.fields(driverValueIdx) match {
            case v: Variable => iterBindings(v.name) = driverValue
            case _ =>
          }
        }

        // Look up remaining indexed body literals (other than driver)
        for (lit <- indexedLits if lit != driverLit) {
          val litRel     = lit.relation.asInstanceOf[SimpleRelation]
          val litKeyIdxs = program.relationIndices.getOrElse(litRel, Nil)
          val litKeys    = litKeyIdxs.map(lit.fields(_)).map {
            case v: Variable => iterBindings.getOrElse(v.name, 0)
            case c: Constant => c.name.toInt
          }
          val litValIdx = lit.fields.indices.diff(litKeyIdxs).headOption.getOrElse(-1)
          if (litValIdx >= 0) {
            val litValue = state.lookup(litRel.name, litKeys.toSeq)
            lit.fields(litValIdx) match {
              case v: Variable => iterBindings(v.name) = litValue
              case _ =>
            }
          }
        }

        // Evaluate Assign functors (n := <expr>)
        for (functor <- rule.functors) {
          functor match {
            case datalog.Assign(param, expr) =>
              param.p match {
                case v: Variable =>
                  iterBindings(v.name) = evalDlArith(expr, iterBindings.toMap)
                case _ =>
              }
            case _ =>
          }
        }

        // Compute head key values and head value
        val headKeys = headKeyVars.map {
          case v: Variable => iterBindings.getOrElse(v.name, 0)
          case c: Constant => c.name.toInt
        }
        val headValue = headValueVar match {
          case v: Variable => iterBindings.getOrElse(v.name, 0)
          case c: Constant => c.name.toInt
        }

        state.update(headRel.name, headKeys.toSeq, headValue)
      }
    }
  }

  /** Evaluate a datalog arithmetic expression given a variable-to-int binding map. */
  private def evalDlArith(expr: datalog.Expr, bindings: Map[String, Int]): Int = expr match {
    case datalog.Param(p) => p match {
      case v: Variable => bindings.getOrElse(v.name, 0)
      case c: Constant => c.name.toInt
    }
    case datalog.Zero(_)    => 0
    case datalog.One(_)     => 1
    case datalog.Negative(e) => -evalDlArith(e, bindings)
    case datalog.Add(a, b)  => evalDlArith(a, bindings) + evalDlArith(b, bindings)
    case datalog.Sub(a, b)  => evalDlArith(a, bindings) - evalDlArith(b, bindings)
    case datalog.Mul(a, b)  => evalDlArith(a, bindings) * evalDlArith(b, bindings)
    case datalog.Div(a, b)  =>
      val denom = evalDlArith(b, bindings)
      if (denom == 0) 0 else evalDlArith(a, bindings) / denom
    case datalog.Min(a, b)  => Math.min(evalDlArith(a, bindings), evalDlArith(b, bindings))
    case _ => 0
  }

  private def evaluateTransaction(funcDecl: DeclFunction, tx: Transaction, state: State): Unit = {
    // setup the state using input argument
    funcDecl.params.zip(tx.parameters).foreach {
      case (param, constant) => {
        param match {
          case v: Variable => state.update(v,constant)
          case _: Constant => throw new Exception(s"Unsupported constant: ${constant}")
        }
      }
    }
    // setup implicit parameters
    state.updateInt(msgSenderName, tx.implicitParameters.msgSender)
    state.updateInt(msgValueName, tx.implicitParameters.value)
    interpretStatement(funcDecl.stmt, state)
  }

  /** Extension point: apply the given statement to the state for the provided transaction.
    * Currently a no-op (state is untouched). Implement handlers here for specific
    * SolidityStatement subclasses as needed.
    */
  private def interpretStatement(statement: Statement, state: State): Unit = {
    // No-op interpreter: does not mutate state.
    // Example extension (pseudocode):
    // statement match {
    //   case UpdateMap(name, keys, tupleTypeName, params) => // update state.maps accordingly
    //   case SetTuple(rel, params) => // update singleton storage
    //   case Emit(event, params) => // record events (not modelled in State currently)
    //   case Require(cond, msg) => // evaluate condition and possibly throw
    // }
    // Keep this method intentionally small; real semantics should be added only
    // for the statements that the project needs.

    def _interpretExpr(e: Expr): Int = e match {
      case arithmetic: Arithmetic => arithmetic match {
        case Zero(_type) => 0
        case One(_type) => 1
        case Param(p) => p match {
          case Constant(_type, name) => _type match {
            case UnitType() => name.toInt
            case AnyType() => ???
            case SymbolType(_) => name.toInt
            case NumberType(_) => name.toInt
            case BooleanType() => name match {
              case "true" | "1" => 1
              case "false" | "0" => 0
              case other => throw new IllegalArgumentException(s"Invalid bool constant: $other")
            }
            case compoundType: CompoundType => ???
          }
          case Variable(_, name) => state.lookup(name)
        }
        case Negative(_e) => - _interpretExpr(_e)
        case operator: BinaryOperator => operator match {
          case Add(a, b) => _interpretExpr(a) + _interpretExpr(b)
          case Sub(a, b) => _interpretExpr(a) - _interpretExpr(b)
          case Mul(a, b) => _interpretExpr(a) * _interpretExpr(b)
          case Div(a, b) => _interpretExpr(a) / _interpretExpr(b)
          case Min(a, b) => Math.min(_interpretExpr(a), _interpretExpr(b))
        }
      }
    }

    def _interpretCond(cond: Condition): Boolean = cond match {
      case True() => true
      case False() => false
      case Match(a, b) => _interpretExpr(a) == _interpretExpr(b)
      case Greater(a, b) => _interpretExpr(a) > _interpretExpr(b)
      case Lesser(a, b) => _interpretExpr(a) < _interpretExpr(b)
      case Geq(a, b) => _interpretExpr(a) >= _interpretExpr(b)
      case Leq(a, b) => _interpretExpr(a) <= _interpretExpr(b)
      case Unequal(a, b) => _interpretExpr(a) != _interpretExpr(b)
      case And(a, b) => _interpretCond(a) && _interpretCond(b)
      case Or(a, b) => _interpretCond(a) || _interpretCond(b)
      // leave as todos
      case MatchRelationField(relation, keys, index, p, enableProjection) => {
        relation match {
          case reserved: ReservedRelation => reserved match {
            case Balance() => ???
            case MsgSender() => {
              val v1 = state.lookup(p.name)
              val v2 = state.lookup(msgSenderName)
              v1 == v2
            }
            case MsgValue() => {
              val v1 = state.lookup(p.name)
              val v2 = state.lookup(msgValueName)
              v1 == v2
            }
            case _ => ???
          }
          case _ => ???
        }
      }
      case BooleanFunction(name, parameters) => ???
    }

    def _interpretParam(p: Parameter): Int = p match {
      case Constant(_type, name) => _type match {
        case _:NumberType | _:SymbolType => name.toInt
        case UnitType() => ???
        case AnyType() => ???
        case BooleanType() => name match {
          case "true" | "1" => 1
          case "false" | "0" => 0
          case other => throw new IllegalArgumentException(s"Invalid bool constant: $other")
        }
        case compoundType: CompoundType => ???
      }
      case Variable(_type, name) => state.lookup(name)
    }

    def _interpret(statement: Statement): Unit = statement match {
      case imp.Seq(a,b) => {
        _interpret(a)
        _interpret(b)
      }
      case If(cond,stmt) => {
        if (_interpretCond(cond)) {
          _interpret(stmt)
        }
      }
      case Assign(p, expr) => {
        p.p match {
          case _: Constant => throw new Exception(s"Cannot assign to constant: $statement")
          case v: Variable => state.updateInt(v.name, _interpretExpr(expr))
        }
      }
      case Constructor(params, statement) => _interpret(statement)
      case ReadTuple(relation, keyList, outputVar) => {
        // Read a keyed tuple from state.maps and bind value fields as `outputVar.fieldName` variables.
        // The _valid pseudo-field indicates whether the tuple existed.
        val keyIds = keyList.map {
          case Constant(t, name) => t match {
            case BooleanType() => if (name.toBoolean) 1 else 0
            case _ => name.toInt
          }
          case v: Variable => state.lookup(v.name)
        }
        relation match {
          case sr: SimpleRelation =>
            val existingValue = state.maps.get(sr.name).flatMap(_.get(keyIds.toVector))
            state.updateInt(s"$outputVar._valid", if (existingValue.isDefined) 1 else 0)
            existingValue.foreach { value =>
              // Value fields follow the key fields in order
              val valueIndices = sr.sig.indices.toList.drop(keyList.size)
              valueIndices.headOption.foreach { i =>
                state.updateInt(s"$outputVar.${sr.memberNames(i)}", value)
              }
            }
          case sr: SingletonRelation =>
            val value = state.lookupSingleton(sr.name)
            sr.memberNames.headOption.foreach { n => state.updateInt(s"$outputVar.$n", value) }
            state.updateInt(s"$outputVar._valid", 1)
          case _ =>
            state.updateInt(s"$outputVar._valid", 0)
        }
      }
      case ReadArray(arrayName, iterator, outputVar) => ???
      case ReadValueFromMap(relation, keyList, output) => {
        val keys = keyList.map {
          case Constant(_type, name) => name.toInt
          case v: Variable => state.lookup(v.name)
        }
        relation match {
          case sr: SimpleRelation => {
            val v = state.lookup(sr, keys)
            state.updateInt(output.name,  v)
          }
          case _ => throw new Exception(s"Unsupported relation type: ${relation}")
        }
      }
      case GroundVar(p, relation, keys, valueIndex, enableProjection) => {
        val keyIds = keys.map{
          case Constant(_type, name) => name.toInt
          case v: Variable => state.lookup(v.name)
        }
        val value = if (keyIds.nonEmpty) {
          state.lookup(relation.name, keyIds)
        }
        else {
          relation match {
            case _: SingletonRelation => state.lookupSingleton(relation.name)
            case _ => state.lookup(relation.name)
          }
        }
        state.updateInt(p.name,value)
      }
      case UpdateMap(name, keys, tupleTypeName, params) => {
        val keyIds = keys.map {
          case Constant(_type, name) => name.toInt
          case v: Variable => state.lookup(v.name)
        }
        val value = _interpretParam(params.head)
        state.update(name, keyIds, value)
      }
      case UpdateMapValue(name, keys, fieldName, p) => {
        val keyIds = keys.map {
          case Constant(_type, name) => name.toInt
          case v: Variable => state.lookup(v.name)
        }
        // val updateValue = state.lookup(p.name)
        val updateValue = _interpretParam(p)
        state.update(name, keyIds, updateValue)
      }
      case SetTuple(relation, params) => {
        require(relation.sig.size==1, s"Assuming only has one fields: $relation.")
        val varId = relation.name
        val value = _interpretParam(params.head)
        state.updateInt(varId, value)
        state.updateTuple(varId, Vector(value))
      }
      case ConvertType(from, to) => {
        val v = _interpretExpr(from)
        state.updateInt(to.name, v)
      }
      case Increment(relation, lit, keyIndices, valueIndex, delta) => {
        val keys = keyIndices.map(i => lit.fields(i))
        val keyIds = keys.map {
          case Constant(_type, name) => name.toInt
          case v: Variable => state.lookup(v.name)
        }.toSeq
        val oldVal = state.lookup(relation.name, keyIds)
        val deltaVal = _interpretExpr(delta)
        val newVal = oldVal + deltaVal
        state.update(relation.name, keyIds, newVal)
      }
      case DeclFunction(name, params, returnType, stmt, metaData) => ???
      case DeclEvent(name, params) => ???
      case DeclModifier(name, params, beforeStatement, afterStatement) => ???
      case Call(functionName, params, optReturnVar) => {
        ???
      }
      case DefineStruct(name, _type) => ???
      case DeclVariable(name, _type) => ???
      case DeclContract(name, statement) => ???
      case ForLoop(iterator, initValue, loopCondition, nextValue, statement) => ???
      case GetObjectAttribute(objectName, attributeName, ret) => ???
      case CallObjectMethod(objectName, methodName, params, optRet) => ???
      case Return(p) => {
        ???
      }
      case Require(condition, msg) => ???
      case Revert(msg) => {
        /** todo: fix this; assuming everything approves for now*/
        println(s"[SolidityInterpreter] warning: ${statement} not processed.")
        ()
      }
      case SendEther(p, amount) => {
        println(s"[SolidityInterpreter] warning: ${statement} not processed.")
        ()
      }
      case Emit(event, parameters) => ()
      case Empty() => ()
    }
    _interpret(statement)
  }

  /** Create a deep copy of a State. This ensures each step in the EvaluatedTrace
    * contains an isolated snapshot and later mutations do not affect earlier
    * snapshots.
    */
  private def cloneState(s: State): State = {
    val newState = State()
    // copy scalar variables
    for ((k, v) <- s.state) {
      newState.state(k) = v
    }
    // copy maps (SimpleRelation -> Map[Vector[Int], Int])
    for ((rel, inner) <- s.maps) {
      val m = newState.maps.getOrElseUpdate(rel, scala.collection.mutable.Map.empty)
      for ((kvec, v) <- inner) {
        m(kvec) = v
      }
    }
    newState
  }
}


object SolidityInterpreter {
  val msgSenderName: String = s"msgSender"
  val msgValueName: String = s"msgValue"
}
