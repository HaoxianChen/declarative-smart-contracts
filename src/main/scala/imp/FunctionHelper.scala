package imp

import datalog.{Literal, Param, Parameter, Relation, UnitType, Variable}

case class FunctionHelper(onStatement: OnStatement) {
  private val funcName: String = FunctionHelper.getFunName(onStatement.relation, onStatement.updateTarget)
  val inRel = onStatement.relation
  private val keyIndices: List[Int] = onStatement match {
    case OnIncrement(literal, keyIndices, updateIndex, updateTarget, statement) => keyIndices
    case OnInsert(literal, updateTarget, statement) => {
      literal.fields.zipWithIndex.filterNot(_._1.name=="_").map(_._2)
    }
  }
  def getParam(literal: Literal): List[Parameter] = {
    require(literal.relation == inRel)
    keyIndices.map(i=>literal.fields(i))
  }
  def getCallStatement(update: UpdateStatement): Statement = update match {
    case Insert(literal) => Call(funcName, getParam(literal))
    case Increment(relation, literal, keyIndices, valueIndex, delta) => {
      val outputType = relation.sig(valueIndex)
      val outVar = Variable(outputType, "delta")
      val params = getParam(literal) :+ outVar
      val assign = imp.Assign(Param(outVar), delta)
      val call = Call(functionName = funcName, params)
      Statement.makeSeq(assign,call)
    }
  }
  def getCallStatementFromInterface(params: List[Parameter]): Statement = {
    Call(funcName, keyIndices.map(i=>params(i)))
  }
}
object FunctionHelper {
  private def getFunName(src: Relation, target: Relation): String = {
    s"update${target.name.capitalize}On${src.name.capitalize}"
  }
  def getFunctionDeclaration(on: OnStatement): Statement = {
    val (funcName,params) = on match {
      case OnInsert(literal, updateTarget, statement) => {
        val params = literal.fields.filterNot(_.name == "_")
        (getFunName(literal.relation, updateTarget), params)
      }
      case onIncrement: OnIncrement => {
        val ps = onIncrement.keys :+ onIncrement.updateValue
        (getFunName(onIncrement.relation, onIncrement.updateTarget), ps)
      }
    }
    DeclFunction(funcName, params, returnType = UnitType(), on.statement,
      metaData = FunctionMetaData(Publicity.Private, false))
  }
}
