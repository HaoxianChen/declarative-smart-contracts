package imp

import datalog.{Add, ArrayType, BooleanType, Constant, Param, Parameter, Relation, ReservedRelation, SimpleRelation, SingletonRelation, StructType, Type, UnitType, Variable}
import imp.ViolationHelper.{getKeyArrayName, getKeyStructName, getKeyTupleName, violationCheckingFunctionName}
import imp.DataStructureHelper.{validBit, validField}

case class ViolationHelper(violations: Set[Relation], primaryKeys: Map[SimpleRelation,List[Int]]) {
  private def getCheckingFunctionName(relation: Relation) = s"check${relation.name.capitalize}"

  def getViolationKeyArrayDecl(): Statement = {
    val allDecls = violations.flatMap {
      case rel:SimpleRelation => {
        val name = getKeyArrayName(rel)
        val keyStruct = getStructType(rel)
        val arrayType = ArrayType(keyStruct)
        Some(DeclVariable(name, arrayType))
      }
      case SingletonRelation(name, sig, memberNames) => None
      case relation: ReservedRelation => None
    }.toList
    Statement.makeSeq(allDecls:_*)
  }

  def getViolationCheckingFunction(relation: Relation) :Statement = {
    /** Iterate through the data structure for violation instances */
    val statement = relation match {
      case rel:SimpleRelation => iterateRelation(rel)
      case SingletonRelation(name, sig, memberNames) => {
        val readTuple = ReadTuple(relation, List())
        val revert = {
          val isTupleValid = Match(Param(Variable(BooleanType(),
            s"${relation.name}Tuple.${validField.name}")), Param(validBit))
          If(isTupleValid, Revert(name))
        }
        Statement.makeSeq(readTuple, revert)
      }
      case rel: ReservedRelation => throw new Exception(s"Reserved relation $rel can not be violation.")
    }
    DeclFunction(getCheckingFunctionName(relation), params = List(), returnType = UnitType(),
      stmt = statement, metaData = FunctionMetaData(Publicity.Private, isView = false, isTransaction = false,
        modifiers = Set()))
  }

  private def readViolationTupleAndRevert(relation: SimpleRelation, keys: List[Parameter]): Statement = {
    val readTuple = ReadTuple(relation, keys)
    val tupleName = DataStructureHelper.relationalTupleName(relation)
    val revert = {
      val a = Variable(BooleanType(), s"$tupleName.${validField.name}")
      val b = validBit
      val cond = Match(Param(a), Param(b))
      If(cond, Revert(relation.name))
    }
    Statement.makeSeq(readTuple, revert)
  }

  private def iterateRelation(relation: SimpleRelation): Statement = {
    val iterator = Variable(Type.uintType, "i")
    val initValue = Param(Constant(Type.uintType, "0"))
    /** Get the key array length */
    val N = Variable(Type.uintType, "N")
    val getArrayLength = GetObjectAttribute(objectName = getKeyArrayName(relation), attributeName = "length", ret = N)
    val loopCondition = Lesser(Param(iterator), Param(N))
    val nextValue = Add(Param(iterator), Param(Constant(Type.uintType, "1")))
    val revert = {
      val keyTupleName: String = getKeyTupleName(relation)
      val keyTupleVar = Variable(getStructType(relation), keyTupleName)
      val readArray = ReadArray(getKeyArrayName(relation), iterator, keyTupleVar)
      val keys = primaryKeys(relation).map(i => {
        val fieldName = relation.memberNames(i)
        Variable(relation.sig(i), s"$keyTupleName.$fieldName")
      })
      Statement.makeSeq(readArray,readViolationTupleAndRevert(relation, keys = keys))
    }
    val loop = ForLoop(iterator = iterator, initValue = initValue, loopCondition = loopCondition, nextValue = nextValue,
      statement = revert)
    Statement.makeSeq(getArrayLength, loop)
  }

  private def getStructType(relation: SimpleRelation): StructType = {
    val structTypeName = getKeyStructName(relation)
    val keys = primaryKeys(relation).map(i=>{
      Variable(relation.sig(i),relation.memberNames(i))
    })
    StructType(structTypeName, keys)
  }

  def getViolationKeyStructTypes(): Set[StructType] = {
    violations.flatMap {
      case rel:SimpleRelation => Some(getStructType(rel))
      case SingletonRelation(name, sig, memberNames) => None
      case relation: ReservedRelation => None
    }
  }

  def getViolationCheckingModifier(): Statement = {
    val statement = {
      val allCalss = violations.map(rel => Call(getCheckingFunctionName(rel), List()))
      Statement.makeSeq(allCalss.toList:_*)
    }
    DeclModifier(violationCheckingFunctionName, params = List(), beforeStatement = Empty(), afterStatement = statement )
  }
}
object ViolationHelper {
  val violationCheckingFunctionName = s"checkViolations"
  def getKeyStructName(relation: SimpleRelation): String = s"${relation.name.capitalize}KeyTuple"
  def getKeyTupleName(relation: SimpleRelation): String = s"${relation.name}KeyTuple"
  def getKeyArrayName(relation: SimpleRelation): String = s"${relation.name}KeyArray"
  def getInsertKeyStatement(relation: Relation, keys: List[Parameter]): Statement = relation match {
    case rel: SimpleRelation => {
      val keyStr = keys.mkString(",")
      val keyTupleName = s"${getKeyStructName(rel)}($keyStr)"
      CallObjectMethod(getKeyArrayName(rel), "push", List(keyTupleName))
    }
    case _: SingletonRelation => Empty()
    case rel: ReservedRelation => throw new Exception(s"Unexpected relation $rel.")
  }
}
