package datalog

case class TypeChecker() {
  def updateTypes(program: Program): Program = {
    val newRules = program.rules.map(updateFunctorType)
    program.copy(rules=newRules)
  }
  private def getParamTypes(rule: Rule): Map[String, Type] = {
    rule.groundedParams.groupBy(_.name).map{
      case (name, ps) => {
        val types = ps.map(_._type)
        require(types.size == 1, s"$name has inconsistent type in ${rule}")
        name -> types.head
      }
    }
  }
  private def updateFunctorType(rule: Rule) :Rule= {
    /** todo: check types of arithemtic operations. */
    val paramTypes = getParamTypes(rule)
    val newFuntors = rule.functors.map {
      case assign: Assign => {
        val newType = paramTypes(assign.a.p.name)
        assign.updateOutputType(newType)
      }
      case other @ (_:Greater|_:Lesser|_:Geq|_:Leq) => other
    }
    rule.copy(functors = newFuntors)
  }
}
