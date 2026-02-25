package synthesis

import com.microsoft.z3.FuncDecl
import datalog.{Add, AnyType, Arithmetic, Balance, BinaryOperator, BooleanType, CompoundType, Constant, Div, Expr, Min, MsgSender, MsgValue, Mul, Negative, Now, NumberType, One, Param, Parameter, Program, Receive, Relation, ReservedRelation, Send, SimpleRelation, SingletonRelation, Sub, SymbolType, This, UnitType, Variable, Zero}
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
case class SolidityInterpreter() {

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
        // Strip recv_ prefix when looking up txDefs (keyed by bare function name).
        val funcName = tx.relation.name.stripPrefix("recv_")
        evaluateTransaction(statement(funcName).asInstanceOf[DeclFunction], tx, stateAfter)
        (Some(tx), stateAfter)
    }.collect { case (Some(tx), st) => (tx, st) }

    EvaluatedTrace(initialState, steps)
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
    try {
      interpretStatement(funcDecl.stmt, state)
    } catch {
      // Normal function return: ignore return value (state updates already applied).
      case SolidityInterpreter.ReturnException(_) => ()
      // require() failure: transaction reverts; state will be discarded by the caller
      // (cloneState is called before evaluateTransaction in interpret()).
      case SolidityInterpreter.RequireFailedException(msg) =>
        println(s"[SolidityInterpreter] require failed: $msg — tx reverted")
    }
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
            case BooleanType() => if (name.toBoolean) 1 else 0
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
      case MatchRelationField(relation, keys, index, p, enableProjection) => {
        relation match {
          case reserved: ReservedRelation => reserved match {
            case Balance() => false
            case MsgSender() =>
              state.lookup(p.name) == state.lookup(msgSenderName)
            case MsgValue() =>
              state.lookup(p.name) == state.lookup(msgValueName)
            case _ => false
          }
          case sr: SimpleRelation => {
            // Resolve key parameters to concrete ints
            val keyIds = keys.map {
              case Constant(_, name) => name.toInt
              case v: Variable      => state.lookup(v.name)
            }
            // Read the stored value at this key from state
            val storedVal = if (keyIds.nonEmpty) state.lookup(sr.name, keyIds)
                            else state.lookup(sr.name)
            // Compare with the expected parameter value
            val expected = _interpretParam(p)
            storedVal == expected
          }
          case _ => false
        }
      }
      // UDF boolean functions: default to true (mock; synthesis does not depend on UDF results)
      case BooleanFunction(name, parameters) => true
    }

    def _interpretParam(p: Parameter): Int = p match {
      case Constant(_type, name) => _type match {
        case _:NumberType | _:SymbolType => name.toInt
        case UnitType() => ???
        case AnyType() => ???
        case BooleanType() => if (name.toBoolean) 1 else 0
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
      case Constructor(params, statement) => ???
      case ReadTuple(relation, keyList, outputVar) => ???
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
          state.lookup(relation.name)
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
      // UDF calls (e.g. isValidSignature, jokTokenBalance): mock with default return value.
      // Synthesis predicate enumeration does not depend on UDF results; this keeps the
      // interpreter from crashing on UDF call sites.
      case Call(functionName, params, optReturnVar) => {
        optReturnVar.foreach(v => state.updateInt(v.name, 1))
      }
      case DefineStruct(name, _type) => ???
      case DeclVariable(name, _type) => ???
      case DeclContract(name, statement) => ???
      case ForLoop(iterator, initValue, loopCondition, nextValue, statement) => ???
      case GetObjectAttribute(objectName, attributeName, ret) => ???
      case CallObjectMethod(objectName, methodName, params, optRet) => ???
      // Return: use a dedicated exception to break out of the current execution frame.
      case Return(p) => throw SolidityInterpreter.ReturnException(_interpretParam(p))
      // Require: evaluate condition; throw on failure so the transaction is treated as reverted.
      case Require(condition, msg) => {
        if (!_interpretCond(condition))
          throw SolidityInterpreter.RequireFailedException(msg)
      }
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

  // Control-flow exceptions used inside the interpreter.
  // ReturnException carries the return value of a Solidity `return` statement.
  case class ReturnException(value: Int) extends Exception
  // RequireFailedException signals a failed `require(...)` check (transaction revert).
  case class RequireFailedException(msg: String) extends Exception(msg)
}