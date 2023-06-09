package imp

import datalog.Arithmetic

class Simplifier {
  def simplify(statement: Statement): Statement = statement match {
    case Seq(a, b) => {
      if (a.isInstanceOf[Empty]) simplify(b)
      else if (b.isInstanceOf[Empty]) simplify(a)
      else Seq(simplify(a),simplify(b))
    }
    case If(condition, _statement) => condition match {
        case True() => simplify(_statement)
        case False() => Empty()
        case Unequal(a, b) => if (a==b) Empty() else If(condition, simplify(_statement))
        case Match(a, b) => if (a==b) simplify(_statement) else If(condition, simplify(_statement))
        case _ => If(condition, simplify(_statement))
      }
    case _on: OnStatement => _on match {
      case OnInsert(literal, updateTarget, _statement, ruleId) =>
        OnInsert(literal, updateTarget, simplify(_statement), ruleId)
      case OnDelete(literal, updateTarget, _statement, ruleId) =>
        OnDelete(literal, updateTarget, simplify(_statement), ruleId)
      case OnIncrement(literal, keyIndices, updateIndex, updateTarget, _statement, ruleId) =>
        OnIncrement(literal, keyIndices, updateIndex, updateTarget, simplify(_statement), ruleId)
    }
    case Search(relation, conditions, _statement) => Search(relation, conditions, simplify(_statement))
    case _updateStatement: UpdateStatement => _updateStatement match {
      case Increment(relation, literal, keyIndices, valueIndex, delta) => {
        Increment(relation, literal,keyIndices, valueIndex, Arithmetic.simplify(delta))
      }
      case IncrementAndInsert(increment) => IncrementAndInsert(simplify(increment).asInstanceOf[Increment])
      case _ => _updateStatement
    }
    case solidityStatement: SolidityStatement => solidityStatement match {
      case DeclFunction(name, params, returnType, stmt, metaData) => {
        DeclFunction(name, params, returnType, simplify(stmt), metaData)
      }
      case _ => solidityStatement
      // case Constructor(params, statement) => ???
      // case ReadTuple(relation, keyList, outputVar) => ???
      // case ReadArray(arrayName, iterator, outputVar) => ???
      // case ReadValueFromMap(relation, keyList, output) => ???
      // case UpdateMap(name, keys, tupleTypeName, params) => ???
      // case UpdateMapValue(name, keys, fieldName, p) => ???
      // case SetTuple(relation, params) => ???
      // case ConvertType(from, to) => ???
      // case DeclFunction(name, params, returnType, stmt, metaData) => ???
      // case DeclEvent(name, params) => ???
      // case DeclModifier(name, params, beforeStatement, afterStatement) => ???
      // case Call(functionName, params, optReturnVar) => ???
      // case DefineStruct(name, _type) => ???
      // case DeclVariable(name, _type) => ???
      // case DeclContract(name, statement) => ???
      // case ForLoop(iterator, initValue, loopCondition, nextValue, statement) => ???
      // case GetObjectAttribute(objectName, attributeName, ret) => ???
      // case CallObjectMethod(objectName, methodName, params, optRet) => ???
      // case Return(p) => ???
      // case Require(condition, msg) => ???
      // case Revert(msg) => ???
      // case SendEther(p, amount) => ???
      // case Emit(event, parameters) => ???
    }
    case _ => statement
    // case GroundVar(p, relation, index) => ???
    // case Assign(p, expr) => ???
    // case Seq(a, b) => ???
    // case If(condition, statement) => ???
    // case statement: OnStatement => ???
    // case Query(literal, statement) => ???
    // case statement: UpdateStatement => ???
    // case UpdateDependentRelations(update) => ???
    // case statement: SolidityStatement => ???
  }

}
