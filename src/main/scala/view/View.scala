package view

import com.microsoft.z3.{ArithExpr, ArithSort, BoolExpr, Context, Expr, Sort}
import datalog._
import imp._
import verification.TransitionSystem.makeStateVar
import verification.Verifier.{arithmeticToZ3, fieldsToConst, typeToSort}

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
                                       z3Prefix: String): BoolExpr = {
    val keyIndices = rule.head.fields.indices.toList.filterNot(_ == resultIndex)
    val keys = keyIndices.map(i=>insertedLiteral.fields(i))
    val values = rule.head.fields.filterNot(f => keys.contains(f))

    if (keyIndices.nonEmpty) {
      val (keyConst, keySort) = fieldsToConst(ctx, keys, z3Prefix)
      val (_, valueSort) = fieldsToConst(ctx,values, z3Prefix)
      val arraySort = ctx.mkArraySort(keySort, valueSort)
      val (v_in, v_out) = makeStateVar(ctx, relation.name, arraySort)
      val valueConst: ArithExpr[_] = ctx.mkSelect(v_in, keyConst.asInstanceOf[Expr[Sort]]).asInstanceOf[ArithExpr[_]]
      val newValue: Expr[Sort] = ctx.mkAdd(valueConst.asInstanceOf[Expr[ArithSort]], arithmeticToZ3(ctx, delta, z3Prefix)).asInstanceOf[Expr[Sort]]
      val update = ctx.mkStore(v_in, keyConst.asInstanceOf[Expr[Sort]], newValue)
      ctx.mkEq(v_out, update)
    }
    else {
      assert(this.relation.isInstanceOf[SingletonRelation])
      val (v_in, v_out) = makeStateVar(ctx, relation.name, typeToSort(ctx, this.relation.sig.head))
      val update = ctx.mkAdd(v_in.asInstanceOf[Expr[ArithSort]], arithmeticToZ3(ctx, delta, z3Prefix))
      ctx.mkEq(v_out, update)
    }
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

