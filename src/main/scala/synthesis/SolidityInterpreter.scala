package synthesis

import com.microsoft.z3.FuncDecl
import datalog.{Add, AnyType, Arithmetic, BinaryOperator, BooleanType, CompoundType, Constant, Div, Expr, Min, Mul, Negative, NumberType, One, Param, Program, Relation, ReservedRelation, SimpleRelation, SingletonRelation, Sub, SymbolType, UnitType, Variable, Zero}
import imp.{And, Assign, BooleanFunction, Call, CallObjectMethod, Condition, Constructor, ConvertType, DeclContract, DeclEvent, DeclFunction, DeclModifier, DeclVariable, DefineStruct, Emit, False, ForLoop, Geq, GetObjectAttribute, Greater, If, Increment, Leq, Lesser, Match, MatchRelationField, Or, ReadArray, ReadTuple, ReadValueFromMap, Require, Return, Revert, SendEther, SetTuple, SolidityStatement, Statement, True, Unequal, UpdateMap, UpdateMapValue}

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
        // Evaluate transaction against statement -- extension point
        evaluateTransaction(statement(tx.relation.name).asInstanceOf[DeclFunction], tx, stateAfter)
        (Some(tx), stateAfter)
    }.collect { case (Some(tx), st) => (tx, st) }

    EvaluatedTrace(initialState, steps)
  }

  private def evaluateTransaction(funcDecl: DeclFunction, tx: Transaction, state: State): Unit = {
    funcDecl.params.zip(tx.parameters).foreach {
      case (param, constant) => {
        param match {
          case v: Variable => state.update(v,constant)
          case _: Constant => throw new Exception(s"Unsupported constant: ${constant}")
        }
      }
    }
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
      // leave as todos
      case MatchRelationField(relation, keys, index, p, enableProjection) => ???
      case BooleanFunction(name, parameters) => ???
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
      case UpdateMap(name, keys, tupleTypeName, params) => ???
      case UpdateMapValue(name, keys, fieldName, p) => {
        val keyIds = keys.map {
          case Constant(_type, name) => name.toInt
          case v: Variable => state.lookup(v.name)
        }
        val updateValue = state.lookup(p.name)
        state.update(name, keyIds, updateValue)
      }
      case SetTuple(relation, params) => ???
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
      case Revert(msg) => ???
      case SendEther(p, amount) => ???
      case Emit(event, parameters) => ()
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
