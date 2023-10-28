package imp

import datalog.{Add, AnyType, Balance, BooleanType, CompoundType, Constant, Literal, MapType, MsgSender, MsgValue, Now, NumberType, Param, Parameter, Receive, Relation, ReservedRelation, Send, SimpleRelation, SingletonRelation, StructType, SymbolType, This, Transaction, Type, UnitType, Variable}
import imp.DataStructureHelper.{getUpdateName, invalidBit, validBit, validField}
import view.View

case class DataStructureHelper(relation: Relation, indices: List[Int], enableProjection: Boolean) {
  require(indices.forall(i => relation.sig.indices.contains(i)))
  val keyTypes: List[Type] = indices.map(i=>relation.sig(i))
  val valueIndices: List[Int] = relation.sig.indices.filterNot(i=>indices.contains(i)).toList
  val valueType: StructType = {
    val name = s"${relation.name.capitalize}Tuple"
    val members = valueIndices.map(i=>relation.paramList(i))
    StructType(name, members:+validField)
  }
  val _type: Type = relation match {
    case _:SimpleRelation => getType(keyTypes, valueType)
    case _:SingletonRelation => valueType
    case reserved: ReservedRelation => reserved match {
      case MsgSender() => UnitType()
      case MsgValue() => UnitType()
      case Send() => UnitType()
      case Receive() => ???
      case Now() => UnitType()
      case Balance() => UnitType()
      case This() => Type.addressType
      case Transaction() => SymbolType("string")
    }
  }

  def translateSearchStatement(search: Search): Statement = {
    require(search.relation==relation)
    search.relation match {
      case _:SingletonRelation|_:ReservedRelation => {
        val condition = Condition.makeConjunction(search.conditions.toList:_*)
        If(condition, search.statement)
      }
      case rel: SimpleRelation => {
        val keys: List[Parameter] = indices.map ( i => {
          search.conditions.find(_.index==i) match {
            case Some(cond) => cond.p
            case None => throw new Exception(s"all keys must be in search conditions.\n$search")
          }
        })
        /** todo: handle situations when not all keys are in search conditions. */
        val readTuple = if (!enableProjection) ReadTuple(rel, keys) else Empty()
        val remainingConditions = search.conditions.filterNot(c => keys.contains(c.p))
        val condition = Condition.makeConjunction(remainingConditions.toList:_*)
        Statement.makeSeq(readTuple, If(condition, search.statement))
      }
    }
  }

  private def getInsertParams(literal: Literal): List[Parameter] = {
    valueIndices.map(i=>literal.fields(i)) :+ validBit
  }

  def getUpdateStatement(update: UpdateStatement, isInsertKey: Boolean): Statement = update match {
    case i:Increment => translateIncrement(i)
    case del:Delete => deleteStatement(del)
    case del:DeleteByKeys => Empty()
    case ins:Insert => ins.relation match {
      case rel: SingletonRelation => SetTuple(rel, getInsertParams(ins.literal))
      case rel: SimpleRelation => insertStatement(ins, isInsertKey)
      case rel :ReservedRelation => rel match {
        case Send() => {
          val (p,n) = (ins.literal.fields(0), ins.literal.fields(1))
          SendEther(p,n)
        }
        case _ => throw new Exception(
        s"Do not support insert tuple of ${rel.getClass}: $rel")
        }
      }
    case _: IncrementAndInsert => Empty()
  }

  private def _incrementToUpdateStatements(increment: Increment): (Statement, Variable) = {
    val valueType = increment.valueType
    val deltaType = View.getDeltaType(increment.valueType)
    val keyList = increment.keyParams
    val keyStr = keyList.map(k => s"[$k]").mkString("")
    val fieldName = increment.relation.memberNames(increment.valueIndex)
    val newValue = Variable(valueType, "newValue")
    val x = Variable(valueType, s"${increment.relation.name}$keyStr.$fieldName")
    // val delta = Variable(increment.delta._type, "_delta")
    val delta = Variable(deltaType, "_delta")
    val convertType = ConvertType(increment.delta, delta)
    val callUpdate = {
      Call(getUpdateName(valueType, deltaType), params = List(x,delta) , Some(newValue))
    }
    (Statement.makeSeq(convertType, callUpdate), newValue)
  }

  private def translateIncrement(increment: Increment): Statement = {
    val valueType = increment.valueType
    if (valueType == increment.delta._type) {
      increment
    }
    else {
      val (callUpdate, newValue) = _incrementToUpdateStatements(increment)
      val fieldName = increment.relation.memberNames(increment.valueIndex)
      val updateMapValue = UpdateMapValue(increment.relation.name, increment.keyParams, fieldName, newValue)
      Statement.makeSeq(callUpdate, updateMapValue)
    }
  }

  def callDependentFunctions(update: UpdateStatement,
                             dependentFunctions: Set[FunctionHelper]): Statement = {
    require(dependentFunctions.nonEmpty)
    update match {
      case del:DeleteByKeys => translateDeleteByKeys(del, dependentFunctions)
      case inc: IncrementAndInsert => translateIncrementAndInsert(inc, dependentFunctions)
      case _: Insert | _: Delete | _: Increment => _callDependentFunctions(update, dependentFunctions)
    }
  }

  private def translateIncrementAndInsert(incrementAndInsert: IncrementAndInsert,
                                          _dependentFunctions: Set[FunctionHelper]
                                         ): Statement = {
    val increment = incrementAndInsert.increment
    /** Read the tuple and update */
    val (callUpdate, newValue) = _incrementToUpdateStatements(increment)
    /** Call dependent functions */
    val insert: Insert = {
      val fields = increment.literal.fields.zipWithIndex.map{
        case (f,i) => if (i == increment.valueIndex) newValue else f
      }
      Insert(Literal(increment.relation, fields))
    }
    val call = _callDependentFunctions(insert, _dependentFunctions)
    Statement.makeSeq(callUpdate, call)
  }


  private def _callDependentFunctions(update: UpdateStatement,
                             dependentFunctions: Set[FunctionHelper]): Statement = {
    val allCalls = dependentFunctions.zipWithIndex.map {
      case(df,idx) => df.onStatement match {
        case _:OnInsert => update match {
          case _:Insert => df.getCallStatement(update,idx)
          case _ => Empty()
        }
        case _:OnDelete => update match {
          case _:Delete => df.getCallStatement(update,idx)
          case _ => Empty()
        }
        case _:OnIncrement => update match {
          case _:Increment => df.getCallStatement(update,idx)
          case _ => Empty()
        }
      }
    }
    Statement.makeSeq(allCalls.toList:_*)
  }

  def insertStatement(insert: Insert, isInsertKey: Boolean): Statement = {
    val keys: List[Parameter] = indices.map(i=>insert.literal.fields(i))
    val updateMap = UpdateMap(relation.name, keys, valueType.name, getInsertParams(insert.literal))
    val insertKey: Statement = if (isInsertKey) {
      ViolationHelper.getInsertKeyStatement(insert.relation, keys)
    }
    else {
      Empty()
    }
    Statement.makeSeq(updateMap, insertKey)
  }

  private def resetConstant(_type: Type): Constant = _type match {
    case SymbolType(name) => Constant(_type, s"$name(0)")
    case t:NumberType => Constant(t, "0")
    case BooleanType() => Constant(BooleanType(), "false")
    case t @ (_:UnitType|_:AnyType|_:CompoundType) => throw new Exception(s"Cannot reset type @$t")
  }

  private def resetTupleStatement(keys: List[Parameter], literal: Literal): Statement = {
    require(keys.nonEmpty || literal.relation.isInstanceOf[SingletonRelation])
    /** Reset values to zero. */
    val params: List[Parameter] = valueIndices.map(i => {
      resetConstant(relation.sig(i))
    }) :+ invalidBit
    UpdateMap(relation.name,keys,valueType.name,params)
  }

  private def translateDeleteByKeys(deleteByKeys: DeleteByKeys, _dependentFunctions: Set[FunctionHelper]): Statement = {
    val tupleName: String = "toDelete"
    val rel = deleteByKeys.relation
    val readTuple = ReadTuple(rel, deleteByKeys.keys, tupleName)
    val toDelete: Literal = {
      val fields: List[Parameter] = deleteByKeys.keys ++ valueIndices.map(i=>{
        val t = rel.sig(i)
        val n = s"$tupleName.${rel.memberNames(i)}"
        Variable(t,n)
      })
      Literal(deleteByKeys.relation, fields)
    }
    /** Check that toDelete exists. */
    val isTupleValid = Match(Param(Variable(BooleanType(), s"$tupleName.${validField.name}")), Param(validBit))

    /** Only update itself */
    val dependentFunctions = _dependentFunctions.filter(_.updateTarget==deleteByKeys.updateTarget)
    val deleteStatements = {
      val delete = Delete(toDelete)
      val calls = _callDependentFunctions(delete, dependentFunctions)
      calls
    }
    Statement.makeSeq(readTuple, If(isTupleValid, deleteStatements))
  }

  def deleteStatement(delete: Delete): Statement = delete.relation match {
    case SimpleRelation(name, sig, memberNames) => {
      assert(indices.nonEmpty)
      /** If tuple exists, reset it to zeros. */
      val keys: List[Parameter] = indices.map(i=>delete.literal.fields(i))
      val readTuple = if (!enableProjection) ReadTuple(delete.relation, keys) else Empty()
      val matches: List[MatchRelationField] = valueIndices.map(i=>{
          val p = delete.literal.fields(i)
          MatchRelationField(delete.relation, keys, i, p, enableProjection)
        })
      val resetTuple: Statement = resetTupleStatement(keys, delete.literal)
      val conditionalReset = If(Condition.makeConjunction(matches:_*), resetTuple)
      Statement.makeSeq(readTuple, conditionalReset)
    }
    case _: SingletonRelation => resetTupleStatement(List(), delete.literal)
    case rel: ReservedRelation => throw new Exception(s"Do not support delete tuple of ${rel.getClass}: $rel")
  }

  private def getType(keyTypes: List[Type], valueType: Type): Type = keyTypes match {
    case ::(head, next) => MapType(head, getType(next,valueType))
    case Nil => valueType
  }
}

object DataStructureHelper {
  val validBit: Constant = Constant(BooleanType(), "true")
  val invalidBit: Constant = Constant(BooleanType(), "false")
  val validField: Variable = Variable(BooleanType(), "_valid")
  def relationalTupleName(relation: Relation): String = s"${relation.name}Tuple"

  def getUpdateName(xType: Type, deltaType: Type): String = s"update${xType}By${deltaType}"
  def updateFunctionDecl(xType: Type, deltaType: Type): DeclFunction = {
    val x = Variable(xType, "x")
    val delta = Variable(deltaType, "delta")
    val statement = {
      if (xType == deltaType) {
        val newValue = Variable(xType, "newValue")
        val update = Assign(Param(newValue), Add(Param(x), Param(delta)))
        val ret = Return(newValue)
        Statement.makeSeq(update,ret)
      }
      else {
        val convertedX = Variable(deltaType, "convertedX")
        val convert = ConvertType(x,convertedX)
        val value = Variable(deltaType, "value")
        val update = Assign(Param(value), Add(Param(convertedX), Param(delta)))
        val convertedValue = Variable(xType, "convertedValue")
        val convertBack = ConvertType(value, convertedValue)
        val ret = Return(convertedValue)
        Statement.makeSeq(convert, update,convertBack, ret)
      }
    }
    DeclFunction(getUpdateName(xType,deltaType), List(x,delta), xType,statement,
      metaData = FunctionMetaData(Publicity.Private, isView = false, isTransaction = false, modifiers = Set()))
  }
}
