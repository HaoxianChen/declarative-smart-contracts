package verification

import com.microsoft.z3.{ArithSort, BoolExpr, Context, Expr, Solver, Sort, Status, Symbol}
import datalog.{Add, AnyType, Arithmetic, Assign, BinFunctor, BinaryOperator, BooleanType, CompoundType, Constant, Equal, Geq, Greater, Leq, Lesser, Literal, MsgSender, MsgValue, Mul, Negative, Now, NumberType, One, Param, Parameter, Program, Relation, ReservedRelation, Rule, Send, SimpleRelation, SingletonRelation, Sub, SymbolType, Type, Unequal, UnitType, Variable, Zero}
import imp.SolidityTranslator.transactionRelationPrefix
import imp.Translator.getMaterializedRelations
import imp.{AbstractImperativeTranslator, ImperativeAbstractProgram, InsertTuple, Trigger}
import verification.Verifier.{fieldsToConst, functorToZ3, literalToConst, paramToConst}
import view.{CountView, JoinView, MaxView, SumView}

class Verifier(program: Program, impAbsProgram: ImperativeAbstractProgram)
  extends AbstractImperativeTranslator(program, isInstrument = false) {

  private val ctx: Context = new Context()

  protected val relations: Set[Relation] = program.relations
  protected val indices: Map[SimpleRelation, List[Int]] = impAbsProgram.indices

  private val materializedRelations: Set[Relation] = getMaterializedRelations(impAbsProgram, program.interfaces)
       .filterNot(_.isInstanceOf[ReservedRelation])

  def check(): Status = {
    val solver: Solver = ctx.mkSolver()
    val verificationConditions = getVerificationCondition()
    solver.add(verificationConditions)
    solver.check()
  }

  private def getProperty(ctx: Context, rule: Rule): BoolExpr = {
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

  private def getVerificationCondition(): BoolExpr = {

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

  def makeTupleSort(ctx: Context, name: String, types: Array[Type]): Sort = {
    val sorts = types.map(t => typeToSort(ctx, t))
    val symbols: Array[Symbol] = types.map(t => ctx.mkSymbol(t.name))
    ctx.mkTupleSort(ctx.mkSymbol(name), symbols, sorts)
  }

  def paramToConst(ctx: Context, param: Parameter, prefix: String): (Expr[_], Sort) = {
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
