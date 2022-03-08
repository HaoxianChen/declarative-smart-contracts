package datalog

import datalog.Arithmetic.updateArithmeticType

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
    val paramTypes = getParamTypes(rule)
    val newFuntors = rule.functors.map (
      f => inferType(f.a, paramTypes) match {
        case Some(at) => _updateFunctorType(f, at)
        case None => inferType(f.b, paramTypes) match {
          case Some(bt) => _updateFunctorType(f, bt)
          case None => f
        }
      })
    rule.copy(functors = newFuntors)
  }

  private def _updateFunctorType(functor: Functor, newType: Type): Functor = functor match {
    case Greater(a, b) => Greater(updateArithmeticType(a,newType), updateArithmeticType(b,newType))
    case Lesser(a, b) => Lesser(updateArithmeticType(a,newType), updateArithmeticType(b,newType))
    case Geq(a, b) => Geq(updateArithmeticType(a,newType), updateArithmeticType(b,newType))
    case Leq(a, b) => Leq(updateArithmeticType(a,newType), updateArithmeticType(b,newType))
    case Unequal(a, b) => Unequal(updateArithmeticType(a,newType), updateArithmeticType(b,newType))
    case Equal(a, b) => Equal(updateArithmeticType(a,newType), updateArithmeticType(b,newType))
    case Assign(a, b) => Assign(Param(a.p.setType(newType)), updateArithmeticType(b,newType))
  }

  private def inferType(arithmetic: Arithmetic, paramTypes: Map[String,Type]): Option[Type] = arithmetic match {
    case Zero(_) => None
    case One(_) => None
    case Param(p) => paramTypes.get(p.name)
    case Negative(e) => inferType(e, paramTypes)
    case bin: BinaryOperator => {
      val at = inferType(bin.a,paramTypes)
      val bt = inferType(bin.b,paramTypes)
      require(at==bt)
      at
    }
  }
}
