package imp

import datalog.{Literal, Param, Parameter, Relation, UnitType, Variable}

case class FunctionHelper(onStatement: OnStatement) {
  val inRel = onStatement.relation
  private val functionName: String = {
    val action: String = onStatement match {
      case _:OnInsert => "Insert"
      case _:OnDelete => "Delete"
      case _:OnIncrement => "Increment"
    }
    val src = onStatement.relation
    val target = onStatement.updateTarget
    s"update${target.name.capitalize}On$action${src.name.capitalize}_r${onStatement.ruleId}"

  }
  private val keyIndices: List[Int] = onStatement match {
    case OnIncrement(literal, keyIndices, updateIndex, updateTarget, statement,_) => keyIndices
    case on @ (_:OnInsert|_:OnDelete) => {
      on.literal.fields.zipWithIndex.filterNot(_._1.name=="_").map(_._2)
    }
  }
  def getParam(literal: Literal): List[Parameter] = {
    require(literal.relation == inRel)
    keyIndices.map(i=>literal.fields(i))
  }
  def getCallStatement(update: UpdateStatement): Statement = {
    update match {
      case Insert(literal) => Call(functionName, getParam(literal))
      case Delete(literal) => Call(functionName, getParam(literal))
      case _:DeleteByKeys => throw new Exception(s"Unhandled statement:$update")
      case Increment(relation, literal, keyIndices, valueIndex, delta) => {
        val outputType = relation.sig(valueIndex)
        val outVar = Variable(outputType, "delta")
        val params = getParam(literal) :+ outVar
        val assign = imp.Assign(Param(outVar), delta)
        val call = Call(functionName = functionName, params)
        Statement.makeSeq(assign,call)
      }
    }
  }
  def getCallStatementFromInterface(params: List[Parameter]): Statement = {
    Call(functionName, keyIndices.map(i=>params(i)))
  }
  def getFunctionDeclaration(): Statement = {
    val (funcName,params) = onStatement match {
      case OnInsert(literal, updateTarget, _,_) => {
        val params = literal.fields.filterNot(_.name == "_")
        (functionName, params)
      }
      case OnDelete(literal, updateTarget, _,_) => {
        val params = literal.fields.filterNot(_.name == "_")
        (functionName, params)
      }
      case onIncrement: OnIncrement => {
        val ps = onIncrement.keys :+ onIncrement.updateValue
        (functionName, ps)
      }
    }

    val isTransaction: Boolean = onStatement.relation.name.startsWith(SolidityTranslator.transactionRelationPrefix)

    DeclFunction(funcName, params, returnType = UnitType(), onStatement.statement,
      metaData = FunctionMetaData(Publicity.Private, isView = false,
        isTransaction = isTransaction, modifiers = Set() ))
  }

  val deleteFuncName: String = s"delete${onStatement.relation.name.capitalize}ByKeys"
  def callDeleteFunctionByKey(keys: List[Parameter]): Statement = Call(deleteFuncName, keys)

}
