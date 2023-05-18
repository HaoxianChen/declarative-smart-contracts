package imp

import datalog.{BooleanType, Literal, Param, Parameter, Relation, UnitType, Variable}
import view.View

case class FunctionHelper(onStatement: OnStatement) {
  val isTransaction: Boolean = onStatement.relation.name.startsWith(SolidityTranslator.transactionRelationPrefix)
  val inRel = onStatement.relation
  val updateTarget = onStatement.updateTarget
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
  def getCallStatement(update: UpdateStatement, idx: Int): Statement = {
    update match {
      case Insert(literal) => Call(functionName, getParam(literal))
      case Delete(literal) => Call(functionName, getParam(literal))
      case _:DeleteByKeys => throw new Exception(s"Unhandled statement:$update")
      case _:IncrementAndInsert => throw new Exception(s"Unhandled statement:$update")
      case inc: Increment => {
        val delta = inc.delta
        val outVar = Variable(delta._type, s"delta$idx")
        // val assign = imp.Assign(Param(outVar), delta)
        val convertType: ConvertType = ConvertType(delta, outVar)
        val params = getParam(inc.literal) :+ outVar
        val call = Call(functionName = functionName, params)
        Statement.makeSeq(convertType,call)
      }
    }
  }
  def getCallStatementFromInterface(params: List[Parameter]): Call = {
    val returnVar = if (isTransaction) {
      Some(Variable(BooleanType(), s"r${onStatement.ruleId}"))
    }
    else {
      None
    }
    Call(functionName, keyIndices.map(i=>params(i)), returnVar)
  }
  def getFunctionDeclaration(): DeclFunction = {
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
        val delta = {
          Variable(View.getDeltaType(onIncrement.updateValue._type), onIncrement.updateValue.name)
        }
        val ps = onIncrement.keys :+ delta
        (functionName, ps)
      }
    }

    val returnType = if (isTransaction) BooleanType() else UnitType()

    DeclFunction(funcName, params, returnType = returnType, onStatement.statement,
      metaData = FunctionMetaData(Publicity.Private, isView = false,
        isTransaction = isTransaction, modifiers = Set() ))
  }

  val deleteFuncName: String = s"delete${onStatement.relation.name.capitalize}ByKeys"
  def callDeleteFunctionByKey(keys: List[Parameter]): Statement = Call(deleteFuncName, keys)

}
