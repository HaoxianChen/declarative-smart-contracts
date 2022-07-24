package verification

import com.microsoft.z3.{ArithSort, BoolExpr, Context, Expr, Solver, Sort, Status, Symbol}
import datalog.{Add, Arithmetic, Assign, BinFunctor, BinaryOperator, Equal, Geq, Greater, Leq, Lesser, Literal, MsgSender, MsgValue, Mul, Negative, Now, One, Param, Parameter, Program, Relation, ReservedRelation, Rule, Send, SimpleRelation, SingletonRelation, Sub, Type, Unequal, Zero}
import imp.SolidityTranslator.transactionRelationPrefix
import imp.Translator.getMaterializedRelations
import imp.{AbstractImperativeTranslator, ImperativeAbstractProgram, InsertTuple, Trigger}

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
        val c = ruleToExpr(rule, t)
        transactionConstraints +:= c
      }
    }

    ctx.mkOr(transactionConstraints.toArray:_*)
  }

  private def ruleToExpr(rule: Rule, trigger: Trigger): BoolExpr = {

    val isMaterialized = materializedRelations.contains(rule.head.relation)
    val bodyConstraints = views(rule).getZ3Constraint(ctx, trigger, isMaterialized)
    val nextTriggers = views(rule).getNextTriggers(trigger)

    var exprs: List[BoolExpr] = List(bodyConstraints)
    for (t <- nextTriggers) {
      val dependentRules: Set[Rule] = getTriggeredRules(t)
      val dependentConstraints: Array[BoolExpr] = dependentRules.toArray.map(r => ruleToExpr(r,t))
      exprs +:= ctx.mkAnd(dependentConstraints:_*)
    }
    ctx.mkAnd(exprs.toArray:_*)
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

  def fieldsToConst(ctx: Context, fields: List[Parameter]): (Expr[Sort], Sort) = {
    if (fields.size==1) {
      val param = fields.head
      val sort = typeToSort(ctx, param._type)
      val _newX: Expr[Sort] = ctx.mkConst(param.name, sort)
      (_newX, sort)
    }
    else {
      ???
    }
  }

  def literalToConst(ctx: Context, lit: Literal, indices: List[Int]): BoolExpr = {
    lit.relation match {
      case SimpleRelation(name, sig, memberNames) => {
        val keys = indices.map(i => lit.fields(i))
        val values = lit.fields.filterNot(f => keys.contains(f))
        if (keys.nonEmpty) {
          val (keyConst, keySort) = fieldsToConst(ctx,keys)
          val (valueConst, valueSort) = fieldsToConst(ctx,values)
          val arrayConst = ctx.mkArrayConst(lit.relation.name, keySort, valueSort)
          ctx.mkEq(ctx.mkSelect(arrayConst, keyConst), valueConst)
        }
        else {
          ???
        }
      }
      case SingletonRelation(name, sig, memberNames) => {
        val (x,_) = fieldsToConst(ctx, lit.fields)
        val relConst = ctx.mkConst(lit.relation.name, getSort(ctx, lit.relation, indices))
        ctx.mkEq(relConst, x)
      }
      case reserved: ReservedRelation => {
        reserved match {
          case MsgSender() | MsgValue() | Now() => {
            val (x,_) = fieldsToConst(ctx, lit.fields)
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

  def arithmeticToZ3(ctx: Context, arithmetic: Arithmetic): Expr[ArithSort] = {
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
      case Param(p) => ctx.mkConst(p.name, sort).asInstanceOf[Expr[ArithSort]]
      case Negative(e) => {
        assert(e._type.name == "int")
        arithmeticToZ3(ctx,Sub(Zero(e._type),e))
      }
      case operator: BinaryOperator => {
        val x = arithmeticToZ3(ctx, operator.a)
        val y = arithmeticToZ3(ctx, operator.b)
        operator match {
          case _:Add => ctx.mkAdd(x,y)
          case _:Sub => ctx.mkSub(x,y)
          case _:Mul => ctx.mkMul(x,y)
        }
      }
    }
  }

  def functorToZ3(ctx: Context, binFunctor: BinFunctor): BoolExpr = {
    val x = arithmeticToZ3(ctx, binFunctor.a)
    val y = arithmeticToZ3(ctx, binFunctor.b)

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
