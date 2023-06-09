package imp

import datalog.{Arithmetic, BooleanType, Literal, Param, Parameter, Relation, UnitType, Variable}
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
        val deltaType = View.getDeltaType(delta._type)
        val outVar = Variable(deltaType, s"delta$idx")
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
    val (funcName,params, newStatement) = onStatement match {
      case OnInsert(literal, updateTarget, statement,_) => {
        val params = literal.fields.filterNot(_.name == "_")
        (functionName, params, statement)
      }
      case OnDelete(literal, updateTarget, statement,_) => {
        val params = literal.fields.filterNot(_.name == "_")
        (functionName, params, statement)
      }
      case onIncrement: OnIncrement => {
        val _delta0 = Variable(onIncrement.updateValue._type, onIncrement.updateValue.name)
        val delta = {
          Variable(View.getDeltaType(_delta0._type), _delta0.name)
        }
        val ps = onIncrement.keys :+ delta
        val mapping: Map[Parameter, Parameter] = Map(_delta0 -> delta)
        val newStatement = Statement.renameParameters(onIncrement.statement, mapping)
        (functionName, ps, newStatement)
      }
    }

    val returnType = if (isTransaction) BooleanType() else UnitType()

    DeclFunction(funcName, params, returnType = returnType, newStatement,
      metaData = FunctionMetaData(Publicity.Private, isView = false,
        isTransaction = isTransaction, modifiers = Set() ))
  }

  val deleteFuncName: String = s"delete${onStatement.relation.name.capitalize}ByKeys"
  def callDeleteFunctionByKey(keys: List[Parameter]): Statement = Call(deleteFuncName, keys)

}
