package imp

import datalog.{Literal, Param, Parameter, Relation, Variable}

case class FunctionHelper(funcName: String, inRel: Relation, keyIndices: List[Int]) {
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
    Call(funcName, params)
  }
  def getDeclStatement(): DeclFunction = {
    ???
  }
}
object FunctionHelper {
  private def getFunName(src: Relation, target: Relation): String = {
    s"update${target.name.capitalize}On${src.name.capitalize}"
  }
  def apply(onStatement: OnStatement): FunctionHelper = onStatement match {
    case OnInsert(literal, updateTarget, statement) =>
      val rel = literal.relation
      val keyIndices = literal.fields.zipWithIndex.filterNot(_._1.name=="_").map(_._2)
      FunctionHelper(funcName = getFunName(rel, updateTarget), inRel = rel, keyIndices = keyIndices)
    case OnIncrement(literal, keyIndices, updateIndex, updateTarget, statement) =>
      val rel = literal.relation
      FunctionHelper(funcName = getFunName(rel, updateTarget), inRel = rel, keyIndices = keyIndices)
  }
}
