package view

import com.microsoft.z3.{ArithExpr, ArithSort, ArraySort, BitVecSort, BoolExpr, Context, Expr, Sort}
import datalog.Arithmetic.updateArithmeticType
import datalog._
import imp._
import verification.TransitionSystem.makeStateVar
import verification.Z3Helper.{arithmeticToZ3, fieldsToConst, getSort, literalToConst, paramToConst, typeToSort}

abstract class View {
  def rule: Rule
  def primaryKeyIndices: List[Int]
  def ruleId: Int
  val relation: Relation = rule.head.relation

  /** Interfaces */
  def insertRow(insertTuple: InsertTuple): OnStatement
  def deleteRow(deleteTuple: DeleteTuple): OnStatement
  def updateRow(incrementValue: IncrementValue): OnStatement

  def getUpdateStatement(trigger: Trigger): OnStatement = trigger match {
    case it: InsertTuple => insertRow(it)
    case dt: DeleteTuple => deleteRow(dt)
    case iv: IncrementValue => updateRow(iv)
    case ReplacedByKey(relation, keyIndices, targetRelation) => {
      require(targetRelation==this.relation)
      deleteRow(DeleteTuple(relation,keyIndices))
    }
  }

  /** Interfaces to generate Z3 constraints */
  def insertRowZ3(ctx: Context, insertTuple: InsertTuple, isMaterialized: Boolean, z3Prefix: String): BoolExpr
  // def deleteRowZ3(deleteTuple: DeleteTuple): BoolExpr
  def updateRowZ3(ctx: Context, incrementValue: IncrementValue, isMaterialized: Boolean, z3Prefix: String): BoolExpr

  def getZ3Constraint(ctx: Context, trigger: Trigger, isMaterialized: Boolean, z3Prefix: String): BoolExpr = trigger match {
    case it: InsertTuple => insertRowZ3(ctx, it, isMaterialized, z3Prefix: String)
    case DeleteTuple(relation, keyIndices) => ???
    case ReplacedByKey(relation, keyIndices, targetRelation) => ???
    case ic: IncrementValue => updateRowZ3(ctx, ic, isMaterialized, z3Prefix: String)
  }

  def getNextTriggers(trigger: Trigger): Set[Trigger]

  protected def isDeleteBeforeInsert(relation: Relation, keyIndices: List[Int]): Boolean = {
    // todo: skip deletion when the inserted literal share the same key with the head.
    keyIndices.nonEmpty || relation.isInstanceOf[SingletonRelation]
  }

  protected def deleteByKeysStatement(literal: Literal, keyIndices: List[Int]): Statement = {
      val keys = keyIndices.map(i=>literal.fields(i))
      DeleteByKeys(literal.relation, keys, updateTarget = this.relation)
  }

  protected def updateTargetRelationZ3(ctx: Context, insertedLiteral: Literal, delta: Arithmetic, resultIndex: Int,
                                       isMaterialized: Boolean,
                                       z3Prefix: String): BoolExpr = {
    val keys = primaryKeyIndices.map(i=>insertedLiteral.fields(i))
    val valueType = this.relation.sig(resultIndex)
    val deltaz3 = arithmeticToZ3(ctx, updateArithmeticType(delta, valueType), z3Prefix)
    val (diffConst,_) = {
      val _y = this.rule.head.fields(resultIndex)
      paramToConst(ctx, _y, z3Prefix)
    }
    val diffEq = ctx.mkEq(diffConst, deltaz3)

    val updateZ3 = if (primaryKeyIndices.nonEmpty) {
      val arraySort = getSort(ctx, insertedLiteral.relation, primaryKeyIndices)
      val (v_in, v_out) = makeStateVar(ctx, relation.name, arraySort)
      val keyConstArray: Array[Expr[_]] = keys.toArray.map(f => paramToConst(ctx, f, z3Prefix)._1)

      val valueConst = ctx.mkSelect(v_in.asInstanceOf[Expr[ArraySort[Sort, Sort]]],
                                      keyConstArray)

      val newValue = valueType.name match {
        case "int" => ctx.mkAdd(valueConst.asInstanceOf[Expr[ArithSort]], diffConst.asInstanceOf[Expr[ArithSort]])
        case "uint" => ctx.mkBVAdd(valueConst.asInstanceOf[Expr[BitVecSort]], diffConst.asInstanceOf[Expr[BitVecSort]])
        case _ => ???
      }

      val store = ctx.mkStore(v_in.asInstanceOf[Expr[ArraySort[Sort, Sort]]], keyConstArray,
                                newValue.asInstanceOf[Expr[Sort]])
      ctx.mkEq(v_out, store)
    }
    else {
      assert(this.relation.isInstanceOf[SingletonRelation])
      val (v_in, v_out) = makeStateVar(ctx, relation.name, typeToSort(ctx, this.relation.sig.head))

      val update = valueType.name match {
        case "int" => ctx.mkAdd(v_in.asInstanceOf[Expr[ArithSort]], diffConst.asInstanceOf[Expr[ArithSort]])
        case "uint" => ctx.mkBVAdd(v_in.asInstanceOf[Expr[BitVecSort]], diffConst.asInstanceOf[Expr[BitVecSort]])
      }
      ctx.mkEq(v_out, update)
    }

    if (isMaterialized) ctx.mkAnd(diffEq, updateZ3)
    else diffEq
  }
}
object View {
  def apply(rule: Rule, primaryKeyIndices: List[Int], ruleId: Int, allIndices: Map[Relation, List[Int]]): View = {
    require(rule.aggregators.size <= 1)
    if (rule.aggregators.isEmpty) {
      JoinView(rule, primaryKeyIndices, ruleId, allIndices)
    }
    else {
      rule.aggregators.head match {
        case _:Sum => SumView(rule, primaryKeyIndices, ruleId)
        case _:Max => MaxView(rule, primaryKeyIndices, ruleId)
        case _:Count => CountView(rule, primaryKeyIndices, ruleId)
      }
    }
  }

  def getDeltaType(t: Type): Type = t match {
      case NumberType(name) => {
        if (name == "uint") Type.integerType
        else if (name == "int") Type.integerType
        else {
          ???
        }
      }
      case _ => ???
    }
}

