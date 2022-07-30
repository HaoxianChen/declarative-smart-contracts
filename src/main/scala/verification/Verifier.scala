package verification

import com.microsoft.z3.{ArithSort, ArrayExpr, ArraySort, BitVecSort, BoolExpr, Context, Expr, Sort, Symbol, TupleSort}
import datalog.{Add, AnyType, Arithmetic, Assign, BinFunctor, BinaryOperator, BooleanType, CompoundType, Constant, Equal, Geq, Greater, Leq, Lesser, Literal, MsgSender, MsgValue, Mul, Negative, Now, NumberType, One, Param, Parameter, Program, Relation, ReservedRelation, Rule, Send, SimpleRelation, SingletonRelation, Sub, SymbolType, Type, Unequal, UnitType, Variable, Zero}
import imp.SolidityTranslator.transactionRelationPrefix
import imp.Translator.getMaterializedRelations
import imp.{AbstractImperativeTranslator, DeleteTuple, ImperativeAbstractProgram, IncrementValue, InsertTuple, ReplacedByKey, Trigger}
import verification.TransitionSystem.makeStateVar
import verification.Verifier.{addressSize, functorToZ3, getSort, literalToConst, makeTupleSort, paramToConst, typeToSort, uintSize}
import view.{CountView, JoinView, MaxView, SumView, View}

class Verifier(program: Program, impAbsProgram: ImperativeAbstractProgram)
  extends AbstractImperativeTranslator(program, isInstrument = true) {

  private val ctx: Context = new Context()

  protected val relations: Set[Relation] = program.relations
  protected val indices: Map[SimpleRelation, List[Int]] = impAbsProgram.indices

  private val materializedRelations: Set[Relation] = getMaterializedRelations(impAbsProgram, program.interfaces)
       .filterNot(_.isInstanceOf[ReservedRelation])
  override val rulesToEvaluate: Set[Rule] = getRulesToEvaluate().filterNot(r => program.violations.contains(r.head.relation))

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

      val keyConstArray: Array[Expr[_]] = keyTypes.toArray.zipWithIndex.map{
          case (t,i) => ctx.mkConst(s"${t.name.toLowerCase()}$i", typeToSort(ctx,t))
      }

      ctx.mkForall(keyConstArray, ctx.mkEq(
            ctx.mkSelect(const.asInstanceOf[ArrayExpr[Sort,Sort]], keyConstArray),
            initValues),
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
    val _vars: Array[Expr[_]] = rule.body.flatMap(_.fields).toArray.map(p => paramToConst(ctx,p,prefix)._1)
    val bodyConstraints = rule.body.map(lit => literalToConst(ctx, lit, getIndices(lit.relation), prefix)).toArray
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
        val c = ruleToExpr(rule, t).simplify().asInstanceOf[BoolExpr]

        /** Add the "unchanged" constraints */
        var unchangedConstraints: List[BoolExpr] = List()
        for (rel <- materializedRelations) {
          val sort = getSort(ctx, rel, getIndices(rel))
          val (v_in, v_out) = makeStateVar(ctx, rel.name, sort)
          val allVars = Prove.get_vars(c)
          if (!allVars.contains(v_out)) {
            unchangedConstraints :+= ctx.mkEq(v_out, v_in)
          }
        }
        transactionConstraints +:= ctx.mkAnd((c::unchangedConstraints).toArray:_*)
      }
    }

    ctx.mkOr(transactionConstraints.toArray:_*)
  }

  private def ruleToExpr(rule: Rule, trigger: Trigger): BoolExpr = {
    var id = 0

    def _ruleToExpr(rule: Rule, trigger: Trigger): (BoolExpr, Int) = {
      val _id = id
      id += 1

      val view = views(rule)
      val isMaterialized = materializedRelations.contains(rule.head.relation)
      val bodyConstraints = view.getZ3Constraint(ctx, trigger, isMaterialized, getPrefix(_id))
      val nextTriggers = view.getNextTriggers(trigger)

      var exprs: List[BoolExpr] = List(bodyConstraints)
      for (t <- nextTriggers) {
        val dependentRules: Set[Rule] = getTriggeredRules(t)
        for (dr <- dependentRules) {
          val (dependentConstraints, nid) = _ruleToExpr(dr, t)
          val (_, from, to) = getNamingConstraints(rule, dr, _id, nid)
          val renamed = dependentConstraints.substitute(from, to).asInstanceOf[BoolExpr]
          exprs +:= renamed
        }
      }
      (ctx.mkAnd(exprs.toArray: _*), _id)
    }

    _ruleToExpr(rule,trigger)._1
  }

  private def getNamingConstraints(rule: Rule, dependentRule: Rule, id1: Int, id2: Int): (BoolExpr, Array[Expr[_]], Array[Expr[_]]) = {
    val headLiteral = rule.head
    val bodyLiteral = views(dependentRule) match {
      case CountView(rule, primaryKeyIndices, ruleId) => ???
      case _: JoinView => {
        val _s = dependentRule.body.filter(_.relation == rule.head.relation)
        assert(_s.size==1)
        _s.head
      }
      case MaxView(rule, primaryKeyIndices, ruleId) => ???
      case sv: SumView => sv.sum.literal
      case _ => ???
    }

    var expr: List[BoolExpr] = List()
    var from: List[Expr[_]] = List()
    var to: List[Expr[_]] = List()
    for (v <- headLiteral.fields.zip(bodyLiteral.fields)) {
      if (v._1.name != "_" && v._2.name != "_") {
        val (x1,_) = paramToConst(ctx, v._1, getPrefix(id1))
        val (x2,_) = paramToConst(ctx, v._2, getPrefix(id2))
        expr +:= ctx.mkEq(x1,x2)
        to +:= x1
        from +:= x2
      }
    }
    (ctx.mkAnd(expr.toArray:_*), from.toArray, to.toArray)
  }

  private def getPrefix(id: Int): String = s"i$id"

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
      val fieldTypes = fields.map(_._type)
      val fieldTypeNames = fieldTypes.mkString(",")
      val tupleSortName: String = s"Tuple($fieldTypeNames)"
      val tupleSort = makeTupleSort(ctx, tupleSortName, fieldTypes.toArray)
      val params = fields.map(f => paramToConst(ctx, f, prefix)._1).toArray
      (tupleSort.mkDecl().apply(params:_*), tupleSort)
    }
  }

  def literalToConst(ctx: Context, lit: Literal, indices: List[Int], prefix: String): BoolExpr = {
    lit.relation match {
      case SimpleRelation(name, sig, memberNames) => {
        val keys = indices.map(i => lit.fields(i))
        val values = lit.fields.filterNot(f => keys.contains(f))
        if (keys.nonEmpty) {
          val (valueConst, _) = fieldsToConst(ctx,values,prefix)
          val sort = getSort(ctx, lit.relation, indices)
          val arrayConst = ctx.mkConst(name, sort)
          val keyConsts: Array[Expr[_]] = keys.toArray.map(f => paramToConst(ctx, f, prefix)._1)
          ctx.mkEq(ctx.mkSelect(arrayConst.asInstanceOf[Expr[ArraySort[Sort,Sort]]], keyConsts), valueConst)
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

  def getArraySort(ctx: Context, keyTypes: List[Type], valueTypes: List[Type]): Sort = {
    val keySorts = keyTypes.toArray.map(t => typeToSort(ctx,t))
    val valueSort = if  (valueTypes.size == 1) {
      typeToSort(ctx,valueTypes.head)
    }
    else {
      val fieldNames = valueTypes.mkString(",")
      makeTupleSort(ctx, s"Tuple($fieldNames)", valueTypes.toArray)
    }
    ctx.mkArraySort(keySorts, valueSort)
  }

  def getSort(ctx: Context, relation: Relation, indices: List[Int]): Sort = relation match {
    case rel: SimpleRelation => {
      val keyTypes = indices.map(i => rel.sig(i))
      val valueTypes = rel.sig.filterNot(t => keyTypes.contains(t))
      getArraySort(ctx, keyTypes, valueTypes)
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

  def arithmeticToZ3(ctx: Context, arithmetic: Arithmetic, prefix: String): Expr[_] = {
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
        // assert(e._type.name == "int")
        arithmeticToZ3(ctx,Sub(Zero(e._type),e), prefix)
      }
      case operator: BinaryOperator => {
        require(operator.a._type==operator.b._type)
        val _type = operator.a._type
        val x = arithmeticToZ3(ctx, operator.a, prefix)
        val y = arithmeticToZ3(ctx, operator.b, prefix)
        operator match {
          case _:Add => _type.name match {
            case "int" => ctx.mkAdd(x.asInstanceOf[Expr[ArithSort]],y.asInstanceOf[Expr[ArithSort]])
            case "uint" => ctx.mkBVAdd(x.asInstanceOf[Expr[BitVecSort]],y.asInstanceOf[Expr[BitVecSort]])
          }
          case _:Sub =>_type.name match {
            case "int" => ctx.mkSub(x.asInstanceOf[Expr[ArithSort]],y.asInstanceOf[Expr[ArithSort]])
            case "uint" => ctx.mkBVSub(x.asInstanceOf[Expr[BitVecSort]],y.asInstanceOf[Expr[BitVecSort]])
          }
          case _:Mul => _type.name match {
            case "int" => ctx.mkMul(x.asInstanceOf[Expr[ArithSort]],y.asInstanceOf[Expr[ArithSort]])
            case "uint" => ctx.mkBVMul(x.asInstanceOf[Expr[BitVecSort]],y.asInstanceOf[Expr[BitVecSort]])
          }
        }
      }
    }
  }

  def functorToZ3(ctx: Context, binFunctor: BinFunctor, prefix: String): BoolExpr = {
    require(binFunctor.a._type == binFunctor.b._type)
    val _type = binFunctor.a._type

    val x = arithmeticToZ3(ctx, binFunctor.a, prefix)
    val y = arithmeticToZ3(ctx, binFunctor.b, prefix)

    binFunctor match {
      case _:Greater => _type.name match {
        case "int" => ctx.mkGt(x.asInstanceOf[Expr[ArithSort]],y.asInstanceOf[Expr[ArithSort]])
        case "uint" => ctx.mkBVUGE(x.asInstanceOf[Expr[BitVecSort]],y.asInstanceOf[Expr[BitVecSort]])
      }
      case _:Lesser => _type.name match {
        case "int" => ctx.mkLt(x.asInstanceOf[Expr[ArithSort]],y.asInstanceOf[Expr[ArithSort]])
        case "uint" => ctx.mkBVULT(x.asInstanceOf[Expr[BitVecSort]],y.asInstanceOf[Expr[BitVecSort]])
      }
      case _:Geq => _type.name match {
        case "int" => ctx.mkGe(x.asInstanceOf[Expr[ArithSort]],y.asInstanceOf[Expr[ArithSort]])
        case "uint" => ctx.mkBVUGE(x.asInstanceOf[Expr[BitVecSort]],y.asInstanceOf[Expr[BitVecSort]])
      }
      case _:Leq => _type.name match {
        case "int" => ctx.mkLe(x.asInstanceOf[Expr[ArithSort]],y.asInstanceOf[Expr[ArithSort]])
        case "uint" => ctx.mkBVULE(x.asInstanceOf[Expr[BitVecSort]],y.asInstanceOf[Expr[BitVecSort]])
      }
      case _:Unequal => ctx.mkNot(ctx.mkEq(x,y))
      case _:Equal => ctx.mkEq(x,y)
      case _:Assign => ctx.mkEq(x,y)
    }
  }

}
