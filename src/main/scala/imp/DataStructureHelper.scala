package imp

import datalog.{AnyType, BooleanType, CompoundType, Constant, Literal, MapType, MsgSender, MsgValue, Now, NumberType, Parameter, Relation, ReservedRelation, Send, SimpleRelation, SingletonRelation, StructType, SymbolType, Type, UnitType, Variable}

case class DataStructureHelper(relation: Relation, indices: List[Int]) {
  require(indices.forall(i => relation.sig.indices.contains(i)))
  val keyTypes: List[Type] = indices.map(i=>relation.sig(i))
  val valueIndices: List[Int] = relation.sig.indices.filterNot(i=>indices.contains(i)).toList
  val valueType: StructType = {
    val name = s"${relation.name.capitalize}Tuple"
    val members = relation.sig.zip(relation.memberNames).map {
      case (t,n) => Variable(t,n)
    }
    StructType(name, members)
  }
  val _type: Type = relation match {
    case _:SimpleRelation => getType(keyTypes, valueType)
    case _:SingletonRelation => valueType
    case reserved: ReservedRelation => reserved match {
      case MsgSender() => UnitType()
      case MsgValue() => UnitType()
      case Send() => UnitType()
      case Now() => UnitType()
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
        val readTuple: ReadTuple = ReadTuple(rel, keys)
        val remainingConditions = search.conditions.filterNot(c => keys.contains(c.p))
        val condition = Condition.makeConjunction(remainingConditions.toList:_*)
        Statement.makeSeq(readTuple, If(condition, search.statement))
      }
    }
  }

  def getUpdateStatement(update: UpdateStatement, isInsertKey: Boolean): Statement = update match {
    case i:Increment => i
    case del:Delete => deleteStatement(del)
    case del:DeleteByKeys => Empty()
    case ins:Insert => ins.relation match {
      case rel: SingletonRelation => SetTuple(rel, ins.literal.fields)
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
  }

  def callDependentFunctions(update: UpdateStatement,
                             dependentFunctions: Set[FunctionHelper]): Statement = {
    require(dependentFunctions.nonEmpty)
    update match {
      case del:DeleteByKeys => {
        val (readTuple, delete) = translateDeleteByKeys(del)
        val calls = _callDependentFunctions(delete, dependentFunctions)
        Statement.makeSeq(readTuple, calls)
      }
      case _: Insert | _: Delete | _: Increment => _callDependentFunctions(update, dependentFunctions)
    }
  }

  private def _callDependentFunctions(update: UpdateStatement,
                             dependentFunctions: Set[FunctionHelper]): Statement = {
    val allCalls = dependentFunctions.map(df => df.onStatement match {
      case _:OnInsert => update match {
        case _:Insert => df.getCallStatement(update)
        case _ => Empty()
      }
      case _:OnDelete => update match {
        case _:Delete => df.getCallStatement(update)
        case _ => Empty()
      }
      case _:OnIncrement => update match {
        case _:Increment => df.getCallStatement(update)
        case _ => Empty()
      }
    }
    )
    Statement.makeSeq(allCalls.toList:_*)
  }

  def insertStatement(insert: Insert, isInsertKey: Boolean): Statement = {
    val keys: List[Parameter] = indices.map(i=>insert.literal.fields(i))
    val updateMap = UpdateMap(relation.name, keys, valueType.name, insert.literal.fields)
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
    val params: List[Parameter] = relation.sig.indices.map(i => {
      // if (indices.contains(i)) {
      //   literal.fields(i)
      // }
      // else {
      //   resetConstant(relation.sig(i))
      //}
      resetConstant(relation.sig(i))
    }).toList
    UpdateMap(relation.name,keys,valueType.name,params)
  }

  private def translateDeleteByKeys(deleteByKeys: DeleteByKeys): (ReadTuple, Delete) = {
    val tupleName: String = "toDelete"
    val rel = deleteByKeys.relation
    val readTuple = ReadTuple(rel, deleteByKeys.keys, tupleName)
    val toDelete: Literal = {
      val fields = rel.sig.zip(rel.memberNames).map{
        case (t,n) => Variable(t,s"$tupleName.$n")
      }
      Literal(deleteByKeys.relation, fields)
    }
    (readTuple, Delete(toDelete))
  }

  def deleteStatement(delete: Delete): Statement = delete.relation match {
    case SimpleRelation(name, sig, memberNames) => {
      assert(indices.nonEmpty)
      /** If tuple exists, reset it to zeros. */
      val keys: List[Parameter] = indices.map(i=>delete.literal.fields(i))
      val readTuple = ReadTuple(delete.relation, keys)
      val matches: List[MatchRelationField] = valueIndices.map(i=>{
          val p = delete.literal.fields(i)
          MatchRelationField(delete.relation, i, p)
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
  def relationalTupleName(relation: Relation): String = s"${relation.name}Tuple"
}
