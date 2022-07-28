package verification

import com.microsoft.z3.{ArithSort, ArrayExpr, ArraySort, BoolExpr, Context, Expr, Solver, Sort, Status, Symbol, TupleSort}
import datalog.{Add, AnyType, Arithmetic, Assign, BinFunctor, BinaryOperator, BooleanType, CompoundType, Constant, Equal, Geq, Greater, Leq, Lesser, Literal, MsgSender, MsgValue, Mul, Negative, Now, NumberType, One, Param, Parameter, Program, Relation, ReservedRelation, Rule, Send, SimpleRelation, SingletonRelation, Sub, SymbolType, Type, Unequal, UnitType, Variable, Zero}
import imp.SolidityTranslator.transactionRelationPrefix
import imp.Translator.getMaterializedRelations
import imp.{AbstractImperativeTranslator, ImperativeAbstractProgram, InsertTuple, Trigger}
import verification.TransitionSystem.makeStateVar
import verification.Verifier.{addressSize, fieldsToConst, functorToZ3, getSort, literalToConst, makeTupleSort, paramToConst, typeToSort, uintSize}
import view.{CountView, JoinView, MaxView, SumView}

class Verifier(program: Program, impAbsProgram: ImperativeAbstractProgram)
  extends AbstractImperativeTranslator(program, isInstrument = false) {

  private val ctx: Context = new Context()

  protected val relations: Set[Relation] = program.relations
  protected val indices: Map[SimpleRelation, List[Int]] = impAbsProgram.indices

  private val materializedRelations: Set[Relation] = getMaterializedRelations(impAbsProgram, program.interfaces)
       .filterNot(_.isInstanceOf[ReservedRelation])

  private def getIndices(relation: Relation): List[Int] = relation match {
    case sr:SimpleRelation => indices(sr)
    case SingletonRelation(name, sig, memberNames) => List()
    case relation: ReservedRelation => ???
  }

  def check(): Unit = {
    val violationRules: Set[Rule] = program.rules.filter(r => program.violations.contains(r.head.relation))
    val tr = TransitionSystem(program.name, ctx)

    var initConditions: List[BoolExpr] = List()
    for (rel <- materializedRelations) {
      val sort = getSort(ctx, rel, getIndices(rel))
      val (v_in, _) = tr.newVar(rel.name, sort)
      initConditions :+= getInitConstraints(rel, v_in)
    }
    tr.setInit(ctx.mkAnd(initConditions.toArray:_*))

    val transitionConditions = getTransitionConstraints()
    tr.setTr(transitionConditions)

    for (vr <- violationRules) {
      val property = getProperty(ctx, vr)
      val (resInit, resTr) = tr.inductiveProve(ctx, property)
      println(property)
      println(s"Init: $resInit")
      println(s"Tr: $resTr")
    }
  }

  private def initValue(_type: Type): Expr[_<:Sort] = _type.name match {
    case "address" => ctx.mkBV(0, addressSize)
    case "int" => ctx.mkInt(0)
    case "uint" => ctx.mkBV(0, uintSize)
  }

  private def getArrayInitValues[T<:Sort](name: String, keyTypes: List[Type], initValues: Expr[T], depth: Int): Expr[_] = keyTypes match {
    case ::(head, next) => {
      // val (p, keySort) = paramToConst(ctx, head, prefix="")
      val keySort = typeToSort(ctx, head)
      val p = ctx.mkConst(s"${head.name.toLowerCase}${depth}", keySort)
      val _arrayInitValues = getArrayInitValues(name, next, initValues, depth+1)
      val arrayName = if (depth > 0) s"$name$depth" else name
      val array = ctx.mkArrayConst(arrayName, keySort, _arrayInitValues.getSort)
      ctx.mkForall(Array(p), ctx.mkEq(ctx.mkSelect(array, p), _arrayInitValues),
        1, null, null, ctx.mkSymbol(s"Q${name}${depth}"), ctx.mkSymbol(s"skid${name}${depth}")
      )
    }
    case Nil => initValues
  }

  private def getInitConstraints(relation: Relation, const: Expr[Sort]): BoolExpr = relation match {
    case sr: SimpleRelation => {
      val keyTypes: List[Type] = indices(sr).map(i=>relation.sig(i))
      val valueTypes: List[Type] = relation.sig.filterNot(t => keyTypes.contains(t))
      val initValues = if (valueTypes.size == 1) {
        initValue(valueTypes.head)
      }
      else {
        val _initValues = valueTypes.toArray.map(initValue)
        val tupleSort = makeTupleSort(ctx, s"${sr.name}Tuple", valueTypes.toArray)
        tupleSort.mkDecl().apply(_initValues:_*)
      }
      val head = keyTypes.head
      val keySort = typeToSort(ctx, head)
      val p = ctx.mkConst(s"${head.name.toLowerCase}", keySort)
      val arrayInitValues = getArrayInitValues(sr.name, keyTypes.tail, initValues, 1)
      ctx.mkForall(Array(p), ctx.mkEq(ctx.mkSelect(const.asInstanceOf[ArrayExpr[Sort,_<:Sort]], p), arrayInitValues),
        1, null, null, ctx.mkSymbol(s"Q${sr.name}"), ctx.mkSymbol(s"skid${sr.name}"))
    }
    case SingletonRelation(name, sig, memberNames) => ctx.mkEq(const, initValue(sig.head))
    case rel: ReservedRelation => ???
  }

  private def getProperty(ctx: Context, rule: Rule): BoolExpr = {
    /** Each violation query rule is translated into a property as follows:
     *  ! \E (V), P1 /\ P2 /\ ...
     *  where V is the set of variable appears in the rule body,
     *  P1, P2, ... are predicates translated from each rule body literal.
     *  */
    val prefix = "p"
    val _indices = rule.head.relation match {
      case rel: SimpleRelation => indices(rel)
      case SingletonRelation(name, sig, memberNames) => ???
      case relation: ReservedRelation => ???
    }
    val _vars: Array[Expr[_]] = rule.body.flatMap(_.fields).toArray.map(p => paramToConst(ctx,p,prefix)._1)
    val bodyConstraints = rule.body.map(lit => literalToConst(ctx, lit, _indices, prefix)).toArray
    val functorConstraints = rule.functors.map(f => functorToZ3(ctx,f, prefix)).toArray
    ctx.mkNot(ctx.mkExists(
        _vars,
        ctx.mkAnd(bodyConstraints++functorConstraints:_*),
        1, null, null, ctx.mkSymbol("Q"), ctx.mkSymbol("skid2")
      )
    )
  }

  private def getTransitionConstraints(): BoolExpr = {

    val triggers: Set[Trigger] = {
      val relationsToTrigger = program.interfaces.map(_.relation).filter(r => r.name.startsWith(transactionRelationPrefix))
      val relationsInBodies = program.rules.flatMap(_.body).map(_.relation)
      val toTrigger = relationsInBodies.intersect(relationsToTrigger)
      toTrigger.map(rel => InsertTuple(rel,primaryKeyIndices(rel)))
    }

    var transactionConstraints: List[BoolExpr] = List()
    for (t <- triggers) {
      val triggeredRules: Set[Rule] = getTriggeredRules(t)
      for (rule <- triggeredRules) {
        val c = ruleToExpr(rule, t, 0).simplify().asInstanceOf[BoolExpr]
        transactionConstraints +:= c
      }
    }

    ctx.mkOr(transactionConstraints.toArray:_*)
  }

  private def ruleToExpr(rule: Rule, trigger: Trigger, depth: Int): BoolExpr = {

    val z3Prefix = s"d${depth}"
    val isMaterialized = materializedRelations.contains(rule.head.relation)
    val bodyConstraints = views(rule).getZ3Constraint(ctx, trigger, isMaterialized, z3Prefix)
    val nextTriggers = views(rule).getNextTriggers(trigger)

    var exprs: List[BoolExpr] = List(bodyConstraints)
    for (t <- nextTriggers) {
      val dependentRules: Set[Rule] = getTriggeredRules(t)
      for (dr <- dependentRules) {
        val dependentConstraints = ruleToExpr(dr,t, depth+1)
        val (_, from, to) = getNamingConstraints(rule, dr, depth)
        val renamed = dependentConstraints.substitute(from,to).asInstanceOf[BoolExpr]
        exprs +:= renamed
      }
    }
    ctx.mkAnd(exprs.toArray:_*)
  }

  private def getNamingConstraints(rule: Rule, dependentRule: Rule, depth: Int): (BoolExpr, Array[Expr[_]], Array[Expr[_]]) = {
    val headLiteral = rule.head
    val bodyLiteral = views(dependentRule) match {
      case CountView(rule, primaryKeyIndices, ruleId) => ???
      case _: JoinView => {
        val _s = dependentRule.body.filter(_.relation == rule.head.relation)
        assert(_s.size==1)
        _s.head
      }
      case MaxView(rule, primaryKeyIndices, ruleId) => ???
      case sv: SumView => {
        val lit = sv.sum.literal
        lit.rename(sv.sum.aggParam, sv.sum.aggResult)
      }
      case _ => ???
    }

    var expr: List[BoolExpr] = List()
    var from: List[Expr[_]] = List()
    var to: List[Expr[_]] = List()
    for (v <- headLiteral.fields.zip(bodyLiteral.fields)) {
      if (v._1.name != "_" && v._2.name != "_") {
        val (x1,_) = paramToConst(ctx, v._1, s"d${depth}")
        val (x2,_) = paramToConst(ctx, v._2, s"d${depth+1}")
        expr +:= ctx.mkEq(x1,x2)
        to +:= x1
        from +:= x2
      }
    }
    (ctx.mkAnd(expr.toArray:_*), from.toArray, to.toArray)
  }

}

object Verifier {
  val uintSize: Int = 32
  val addressSize: Int = 16
  def typeToSort(ctx: Context, t: Type): Sort = t.name match {
      case "address" => ctx.mkBitVecSort(addressSize)
      case "int" => ctx.mkIntSort()
      case "uint" => ctx.mkBitVecSort(uintSize)
      case _ => ???
    }

  def makeTupleSort(ctx: Context, name: String, types: Array[Type]): TupleSort = {
    val sorts = types.map(t => typeToSort(ctx, t))
    val symbols: Array[Symbol] = types.map(t => ctx.mkSymbol(t.name))
    ctx.mkTupleSort(ctx.mkSymbol(name), symbols, sorts)
  }

  def paramToConst(ctx: Context, param: Parameter, prefix: String): (Expr[_<:Sort], Sort) = {
    val sort = typeToSort(ctx, param._type)
    val _newX = param match {
      case Constant(_type, name) => {
        _type.name match {
          case "address" => ctx.mkBV(name.toInt, addressSize)
          case "int" => ctx.mkInt(name.toInt)
          case "uint" => ctx.mkBV(name.toInt, uintSize)
          case _ => ???
        }
      }
      case Variable(_,name) => {
        ctx.mkConst(s"${prefix}_${name}", sort)

      }
    }
    (_newX, sort)
  }

  def fieldsToConst(ctx: Context, fields: List[Parameter], prefix: String): (Expr[_], Sort) = {
    if (fields.size==1) {
      paramToConst(ctx, fields.head, prefix)
    }
    else {
      ???
    }
  }

  def literalToConst(ctx: Context, lit: Literal, indices: List[Int], prefix: String): BoolExpr = {
    lit.relation match {
      case SimpleRelation(name, sig, memberNames) => {
        val keys = indices.map(i => lit.fields(i))
        val values = lit.fields.filterNot(f => keys.contains(f))
        if (keys.nonEmpty) {
          val (keyConst, keySort) = fieldsToConst(ctx,keys,prefix)
          val (valueConst, valueSort) = fieldsToConst(ctx,values,prefix)
          val arrayConst = ctx.mkArrayConst(lit.relation.name, keySort, valueSort)
          ctx.mkEq(ctx.mkSelect(arrayConst, keyConst.asInstanceOf[Expr[Sort]]), valueConst)
        }
        else {
          ???
        }
      }
      case SingletonRelation(name, sig, memberNames) => {
        val (x,_) = fieldsToConst(ctx, lit.fields,prefix)
        val relConst = ctx.mkConst(lit.relation.name, getSort(ctx, lit.relation, indices))
        ctx.mkEq(relConst, x)
      }
      case reserved: ReservedRelation => {
        reserved match {
          case MsgSender() | MsgValue() | Now() => {
            val (x,_) = fieldsToConst(ctx, lit.fields, prefix)
            val relConst = ctx.mkConst(lit.relation.name, getSort(ctx, lit.relation, indices))
            ctx.mkEq(relConst, x)
          }
          case Send() => ???
        }
      }
    }
  }

  def getSort(ctx: Context, relation: Relation, indices: List[Int]): Sort = relation match {
    case rel: SimpleRelation => {
      val keyTypes = indices.map(i => rel.sig(i))
      val keySort: Sort = if (keyTypes.size == 1) {
        typeToSort(ctx, keyTypes.head)
      }
      else {
        val tupleSortName: String = s"${rel.name}Key"
        makeTupleSort(ctx, tupleSortName, keyTypes.toArray)
      }
      val valueSort: Sort = {
        val remainingFields = rel.sig.diff(keyTypes)
        if (remainingFields.size == 1) {
          typeToSort(ctx, remainingFields.head)
        }
        else {
          val tupleSortName: String = s"${rel.name}Value"
          makeTupleSort(ctx, tupleSortName, remainingFields.toArray)
        }
      }
      ctx.mkArraySort(keySort, valueSort)
    }
    case SingletonRelation(name, sig, _) => {
      val t = sig.head
      typeToSort(ctx, t)
    }
    case reserved :ReservedRelation => reserved match {
      case MsgSender() | MsgValue() | Now() => typeToSort(ctx, reserved.sig.head)
      case Send() => ???
    }
  }

  def arithmeticToZ3(ctx: Context, arithmetic: Arithmetic, prefix: String): Expr[ArithSort] = {
    val sort = typeToSort(ctx, arithmetic._type)
    arithmetic match {
      case Zero(_type) => _type.name match {
        case "int" => ctx.mkInt(0).asInstanceOf[Expr[ArithSort]]
        case "uint" => ctx.mkBV(0,uintSize).asInstanceOf[Expr[ArithSort]]
        case _ => ???
      }
      case One(_type) => _type.name match {
        case "int" => ctx.mkInt(1).asInstanceOf[Expr[ArithSort]]
        case "uint" => ctx.mkBV(1,uintSize).asInstanceOf[Expr[ArithSort]]
        case _ => ???
      }
      case Param(p) => {
        paramToConst(ctx, p, prefix)._1.asInstanceOf[Expr[ArithSort]]
        // ctx.mkConst(s"${prefix}_${p.name}", sort).asInstanceOf[Expr[ArithSort]]
      }
      case Negative(e) => {
        assert(e._type.name == "int")
        arithmeticToZ3(ctx,Sub(Zero(e._type),e), prefix)
      }
      case operator: BinaryOperator => {
        val x = arithmeticToZ3(ctx, operator.a, prefix)
        val y = arithmeticToZ3(ctx, operator.b, prefix)
        operator match {
          case _:Add => ctx.mkAdd(x,y)
          case _:Sub => ctx.mkSub(x,y)
          case _:Mul => ctx.mkMul(x,y)
        }
      }
    }
  }

  def functorToZ3(ctx: Context, binFunctor: BinFunctor, prefix: String): BoolExpr = {
    val x = arithmeticToZ3(ctx, binFunctor.a, prefix)
    val y = arithmeticToZ3(ctx, binFunctor.b, prefix)

    binFunctor match {
      case _:Greater => ctx.mkGt(x,y)
      case _:Lesser => ctx.mkLt(x,y)
      case _:Geq => ctx.mkGe(x,y)
      case _:Leq => ctx.mkLe(x,y)
      case _:Unequal => ctx.mkNot(ctx.mkEq(x,y))
      case _:Equal => ctx.mkEq(x,y)
      case _:Assign => ctx.mkEq(x,y)
    }
  }

}
