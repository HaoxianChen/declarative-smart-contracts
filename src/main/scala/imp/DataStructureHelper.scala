package imp

import datalog.{AnyType, BooleanType, CompoundType, MapType, MsgSender, MsgValue, NumberType, Parameter, Relation, ReservedRelation, SimpleRelation, SingletonRelation, StructType, SymbolType, Type, UnitType, Variable}

case class DataStructureHelper(relation: Relation, indices: List[Int]) {
  require(indices.forall(i => relation.sig.indices.contains(i)))
  val keyTypes: List[Type] = indices.map(i=>relation.sig(i))
  // val valueIndices: List[Int] = relation.sig.indices.filterNot(i => indices.contains(i)).toList
  // val valueType: Type = {
  //   if (valueIndices.isEmpty) {
  //     UnitType()
  //     /** todo: Fix this. */
  //   }
  //   else if (valueIndices.size == 1) {
  //     relation.sig(valueIndices.head)
  //   }
  //   else {
  //     val name = s"${relation.name}Tuple"
  //     val members = valueIndices.map(i=>Variable(relation.sig(i),relation.memberNames(i)))
  //     StructType(name, members)
  //   }
  // }
  val valueType: Type = {
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
            case None => throw new Exception(s"all keys must be in search conditions.")
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

  private def getType(keyTypes: List[Type], valueType: Type): Type = keyTypes match {
    case ::(head, next) => MapType(head, getType(next,valueType))
    case Nil => valueType
  }
}
