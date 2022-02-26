package imp

import datalog.{BooleanType, Constant, Relation, ReservedRelation, SimpleRelation, SingletonRelation, UnitType}
import imp.ViolationHelper.violationCheckingFunctionName

case class ViolationHelper(primaryKeys: Map[SimpleRelation,List[Int]]) {
  private def validBitIndex(relation: Relation) = relation.memberNames.indexOf("valid")
  private def getCheckingFunctionName(relation: Relation) = s"check${relation.name.capitalize}"

  def getViolationCheckingFunction(relation: Relation) :Statement = {
    /** Iterate through the data structure for violation instances */
    val statement = relation match {
      case SimpleRelation(name, sig, memberNames) => ???
      case SingletonRelation(name, sig, memberNames) => {
        val readTuple = ReadTuple(relation, List())
        val revert = {
          val cond = Match(relation, validBitIndex(relation), Constant(BooleanType(), "true"))
          If(cond, Revert(name))
        }
        Statement.makeSeq(readTuple, revert)
      }
      case rel: ReservedRelation => throw new Exception(s"Reserved relation $rel can not be violation.")
    }
    DeclFunction(getCheckingFunctionName(relation), params = List(), returnType = UnitType(),
      stmt = statement, metaData = FunctionMetaData(Publicity.Private, isView = false, isTransaction = false,
        modifiers = Set()))
  }

  def getViolationCheckingModifier(violations: Set[Relation]): Statement = {
    val statement = {
      val allCalss = violations.map(rel => Call(getCheckingFunctionName(rel), List()))
      Statement.makeSeq(allCalss.toList:_*)
    }
    DeclModifier(violationCheckingFunctionName, params = List(), beforeStatement = Empty(), afterStatement = statement )
  }
}
object ViolationHelper {
  val violationCheckingFunctionName = s"checkViolations"
}
