package imp

import datalog.{Literal, Param, Parameter, Relation, UnitType, Variable}
import imp.FunctionHelper.getFunName

case class FunctionHelper(onStatement: OnStatement) {
  val inRel = onStatement.relation
  private val keyIndices: List[Int] = onStatement match {
    case OnIncrement(literal, keyIndices, updateIndex, updateTarget, statement) => keyIndices
    case on @ (_:OnInsert|_:OnDelete) => {
      on.literal.fields.zipWithIndex.filterNot(_._1.name=="_").map(_._2)
    }
  }
  def getParam(literal: Literal): List[Parameter] = {
    require(literal.relation == inRel)
    keyIndices.map(i=>literal.fields(i))
  }
  def getCallStatement(update: UpdateStatement): Statement = {
    val funcName = getFunName(inRel, onStatement.updateTarget, update)
    update match {
      case Insert(literal) => Call(funcName, getParam(literal))
      case Delete(literal) => Call(funcName, getParam(literal))
      case _:DeleteByKeys => throw new Exception(s"Unhandled statement:$update")
      case Increment(relation, literal, keyIndices, valueIndex, delta) => {
        val outputType = relation.sig(valueIndex)
        val outVar = Variable(outputType, "delta")
        val params = getParam(literal) :+ outVar
        val assign = imp.Assign(Param(outVar), delta)
        val call = Call(functionName = funcName, params)
        Statement.makeSeq(assign,call)
      }
    }
  }
  def getCallStatementFromInterface(params: List[Parameter]): Statement = {
    val funcName = getFunName(inRel, onStatement.updateTarget, "Insert")
    Call(funcName, keyIndices.map(i=>params(i)))
  }
  def getFunctionDeclaration(): Statement = {
    val (funcName,params) = onStatement match {
      case OnInsert(literal, updateTarget, statement) => {
        val params = literal.fields.filterNot(_.name == "_")
        (getFunName(literal.relation, updateTarget, onStatement), params)
      }
      case OnDelete(literal, updateTarget, statement) => {
        val params = literal.fields.filterNot(_.name == "_")
        (getFunName(literal.relation, updateTarget, onStatement), params)
      }
      case onIncrement: OnIncrement => {
        val ps = onIncrement.keys :+ onIncrement.updateValue
        (getFunName(onIncrement.relation, onIncrement.updateTarget, onStatement), ps)
      }
    }

    val isTransaction: Boolean = onStatement.relation.name.startsWith(SolidityTranslator.transactionRelationPrefix)

    DeclFunction(funcName, params, returnType = UnitType(), onStatement.statement,
      metaData = FunctionMetaData(Publicity.Private, isView = false,
        isTransaction = isTransaction, modifiers = Set() ))
  }

  val deleteFuncName: String = s"delete${onStatement.relation.name.capitalize}ByKeys"
  def callDeleteFunctionByKey(keys: List[Parameter]): Statement = Call(deleteFuncName, keys)

  private def DeleteStatementByKeys(): Statement = {
    require(this.onStatement.isInstanceOf[OnDelete])
    val keys: List[Parameter] = keyIndices.map(i=>onStatement.literal.fields(i))
    val statements: Statement = {
      /** Read the struct and unpack it as arguments. */
      val readTuple = ReadTuple(onStatement.relation, keys)
      val params: List[Parameter] = {
        ???
      }
      val call = Call(getFunName(inRel,this.onStatement.updateTarget, "delete"), params)
      Statement.makeSeq(readTuple,call)
    }
    DeclFunction(deleteFuncName, keys, returnType = UnitType(), statements,
      metaData = FunctionMetaData(Publicity.Private, isView = false,
        isTransaction = false, modifiers = Set()))
  }
}
object FunctionHelper {
  private def getFunName(src: Relation, target: Relation, action: String): String = {
    s"update${target.name.capitalize}On$action${src.name.capitalize}"
  }
  private def getFunName(src: Relation, target: Relation, update: UpdateStatement): String = {
    val action: String = getUpdateAction(update)
    getFunName(src, target, action)
  }
  private def getFunName(src: Relation, target: Relation, on: OnStatement): String = {
    val action: String = getUpdateAction(on)
    getFunName(src, target, action)
  }
  private def getUpdateAction(on: OnStatement): String = on match {
    case OnInsert(literal, updateTarget, statement) => "Insert"
    case OnDelete(literal, updateTarget, statement) => "Delete"
    case OnIncrement(literal, keyIndices, updateIndex, updateTarget, statement) => "Increment"
  }
  private def getUpdateAction(update: UpdateStatement): String = update match {
    case Insert(literal) => "Insert"
    case Delete(literal) => "Delete"
    case _:DeleteByKeys => "DeleteByKeys"
    case Increment(relation, literal, keyIndices, valueIndex, delta) => "Increment"
  }

}
