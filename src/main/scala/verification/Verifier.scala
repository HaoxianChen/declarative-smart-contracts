package verification

import com.microsoft.z3.{ArrayExpr, BoolExpr, Context, Expr, IntSort, Sort, TupleSort}
import datalog.{Constant, Program, Relation, ReservedRelation, Rule, SimpleRelation, SingletonRelation, Type, Variable}
import imp.SolidityTranslator.transactionRelationPrefix
import imp.Translator.getMaterializedRelations
import imp.{AbstractImperativeTranslator, DeleteTuple, ImperativeAbstractProgram, IncrementValue, InsertTuple, ReplacedByKey, Trigger}
import verification.RuleZ3Constraints.getVersionedVariableName
import verification.TransitionSystem.makeStateVar
import verification.Z3Helper.{addressSize, extractEq, functorToZ3, getArraySort, getSort, initValue, literalToConst, makeTupleSort, paramToConst, typeToSort, uintSize}
import view.{CountView, JoinView, MaxView, SumView, View}

case class RuleZ3Constraints(ruleConstraints: BoolExpr,
                             updateConstraint: BoolExpr,
                             updateExprs: Array[(Expr[Sort], Expr[Sort], Expr[_<:Sort])],
                             nextTriggers: Set[Trigger]) {
  def allConstraints(ctx: Context): BoolExpr = ctx.mkAnd(ruleConstraints, updateConstraint)

  def getVersionedConstraint(ctx: Context, relation: Relation, indices: List[Int], version: Int): BoolExpr = {
    if (updateExprs.nonEmpty) {
      val allUpdates = {
        val _exprs: Array[BoolExpr] = updateExprs.map(t => ctx.mkEq(t._2, t._3))
        ctx.mkAnd(_exprs:_*)
      }
      val versionedUpdates: BoolExpr = versionUpdateExpr(ctx, allUpdates, relation, indices, version).asInstanceOf[BoolExpr]
      ctx.mkAnd(ruleConstraints, updateConstraint, versionedUpdates)
    }
    else {
      ctx.mkAnd(ruleConstraints, updateConstraint)
    }
  }

  private def versionUpdateExpr(ctx: Context, expr: Expr[_], relation: Relation, indices: List[Int], version: Int): Expr[_] = {
    val sort = getSort(ctx, relation, indices)
    val (v_in, v_out) = makeStateVar(ctx, relation.name, sort)
    val v_in_versioned = ctx.mkConst(getVersionedVariableName(relation, version), sort)
    val v_out_versioned = ctx.mkConst(getVersionedVariableName(relation, version+1), sort)
    val from: Array[Expr[_]] = Array(v_in, v_out)
    val to: Array[Expr[_]] = Array(v_in_versioned, v_out_versioned)
    expr.substitute(from, to)
  }
}
object RuleZ3Constraints {
  def apply(ctx:Context): RuleZ3Constraints = RuleZ3Constraints(ctx.mkTrue(), ctx.mkTrue(), Array(), Set())

  def getVersionedVariableName(relation: Relation, version: Int): String = {
    if (version > 0) s"${relation.name}_v${version}"
    else s"${relation.name}"
  }
}

class Verifier(program: Program, impAbsProgram: ImperativeAbstractProgram)
  extends AbstractImperativeTranslator(program, isInstrument = true) {

  private val ctx: Context = new Context()

  protected val relations: Set[Relation] = program.relations
  protected val indices: Map[SimpleRelation, List[Int]] = impAbsProgram.indices

  private val materializedRelations: Set[Relation] = getMaterializedRelations(impAbsProgram, program.interfaces)
       .filterNot(_.isInstanceOf[ReservedRelation])
  override val rulesToEvaluate: Set[Rule] = getRulesToEvaluate().filterNot(r => program.violations.contains(r.head.relation))

  val stateVars: Set[(Expr[_], Expr[_])] = materializedRelations.map(rel => {
    val sort = getSort(ctx, rel, getIndices(rel))
    makeStateVar(ctx, rel.name, sort)
  })

  private def getIndices(relation: Relation): List[Int] = relation match {
    case sr:SimpleRelation => indices.getOrElse(sr, List())
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

  private def getInitConstraints(relation: Relation, const: Expr[Sort]): BoolExpr = relation match {
    case sr: SimpleRelation => {
      val (arraySort, keySorts, valueSort) = getArraySort(ctx, sr, indices(sr))
      val keyTypes: Array[Type] = indices(sr).map(i=>relation.sig(i)).toArray
      val valueIndices = relation.sig.indices.filterNot(i=>indices(sr).contains(i))
      val valueTypes: Array[Type] = valueIndices.map(i=>sr.sig(i)).toArray

      val initValues = if (!valueSort.isInstanceOf[TupleSort]) {
        initValue(ctx,valueTypes.head)
      }
      else {
        val _initValues = valueTypes.map(t => initValue(ctx,t))
        valueSort.asInstanceOf[TupleSort].mkDecl().apply(_initValues:_*)
      }

      val keyConstArray: Array[Expr[_]] = keyTypes.zipWithIndex.map{
          case (t,i) => ctx.mkConst(s"${t.name.toLowerCase()}$i", typeToSort(ctx,t))
      }

      ctx.mkForall(keyConstArray, ctx.mkEq(
            ctx.mkSelect(const.asInstanceOf[ArrayExpr[Sort,Sort]], keyConstArray),
            initValues),
        1, null, null, ctx.mkSymbol(s"Q${sr.name}"), ctx.mkSymbol(s"skid${sr.name}"))
    }
    case SingletonRelation(name, sig, memberNames) => {
      if (sig.size==1) {
        ctx.mkEq(const, initValue(ctx, sig.head))
      }
      else {
        val tupleSort = makeTupleSort(ctx, relation, sig.toArray, memberNames.toArray)
        val tupleConst = ctx.mkConst(name, tupleSort)
        val eqs = sig.zip(tupleSort.getFieldDecls).map {
          case (t, decl) => ctx.mkEq(decl.apply(tupleConst), initValue(ctx,t))
        }.toArray
        ctx.mkAnd(eqs: _*)
      }
    }
    case rel: ReservedRelation => ???
  }

  private def getProperty(ctx: Context, rule: Rule): BoolExpr = {
    /** Each violation query rule is translated into a property as follows:
     *  ! \E (V), P1 /\ P2 /\ ...
     *  where V is the set of variable appears in the rule body,
     *  P1, P2, ... are predicates translated from each rule body literal.
     *  */
    val prefix = "p"
    val _vars: Array[Expr[_]] = rule.body.flatMap(_.fields).toArray.flatMap(p => p match {
      case Constant(_type, name) => None
      case Variable(_type, name) => Some(paramToConst(ctx,p,prefix)._1)
    })
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
    var transactionConst: List[Expr[IntSort]] = List()
    for (t <- triggers) {
      var i: Int = 0
      val triggeredRules: Set[Rule] = getTriggeredRules(t)
      for (rule <- triggeredRules) {
        val ruleConsrtaint: BoolExpr = ruleToExpr(rule, t)

        /** Add the "unchanged" constraints */
        val unchangedConstraints: List[BoolExpr] = getUnchangedConstraints(ruleConsrtaint)

        /** A boolean value indicating which transaction branch gets evaluate to true */
        val trConst = ctx.mkIntConst(s"${t.relation.name}$i")
        i += 1

        transactionConst +:= trConst
        transactionConstraints +:= ctx.mkAnd(
          (ctx.mkEq(trConst, ctx.mkInt(1)) :: ruleConsrtaint :: unchangedConstraints).toArray: _*)
      }
    }

    ctx.mkAnd(
      ctx.mkAnd(transactionConst.map(c => ctx.mkOr(ctx.mkEq(c,ctx.mkInt(0)), ctx.mkEq(c,ctx.mkInt(1)))).toArray:_*),
      ctx.mkEq(ctx.mkAdd(transactionConst.toArray:_*), ctx.mkInt(1)),
      ctx.mkOr(transactionConstraints.toArray:_*)
    )
  }

  private def getUnchangedConstraints(_expr: BoolExpr): List[BoolExpr] = {
    var unchangedConstraints: List[BoolExpr] = List()
    val allVars = Prove.get_vars(_expr)
    for (rel <- materializedRelations) {
      val sort = getSort(ctx, rel, getIndices(rel))
      val (v_in, v_out) = makeStateVar(ctx, rel.name, sort)
      if (!allVars.contains(v_out)) {
        unchangedConstraints :+= ctx.mkEq(v_out, v_in)
      }
    }
    unchangedConstraints
  }

  private def ruleToExpr(rule: Rule, trigger: Trigger): BoolExpr = {
    var id = 0
    var versions: Map[Relation, Int] = Map()

    def _ruleToExpr(rule: Rule, trigger: Trigger):
      (Int, BoolExpr) = {
      val thisId = id
      id += 1

      val view = views(rule)
      val thisRelation = rule.head.relation
      val isMaterialized = materializedRelations.contains(thisRelation)

      val thisVersion = versions.getOrElse(rule.head.relation, 0)
      if (isMaterialized) {
        versions = versions.updated(rule.head.relation, thisVersion+1)
      }
      /** Add naming constraints  */
      def _renameConstraints(trigger: Trigger, nextId: Int, dependentRule: Rule, _body: BoolExpr):
      BoolExpr = {
        val (_, from, to) = getNamingConstraints(rule, dependentRule, trigger, thisId, nextId)
        val _renamed: BoolExpr = _body.substitute(from, to).asInstanceOf[BoolExpr]
        _renamed
      }


      val (trueBranch, falseBranch) = view.getZ3Constraint(ctx, trigger, isMaterialized, getPrefix(thisId))

      val trueBranchWithDependentConstraints = {
        val versionedConstraint = {
          trueBranch.getVersionedConstraint(ctx, thisRelation, getIndices(thisRelation), thisVersion)
        }

        if (trueBranch.nextTriggers.nonEmpty) {

          val dependentRulesAndTriggers: List[(Trigger,Rule)] = {
            var ret: List[(Trigger,Rule)] = List()
            for (t <- trueBranch.nextTriggers) {
              for (tr <- getTriggeredRules(t)) {
                for (nt <- views(tr).getTriggersForView(t)) {
                  ret :+= (nt, tr)
                }
              }
            }
            ret
          }

          var allDependentConstraints: List[BoolExpr] = List()
          for ((t, dr) <- dependentRulesAndTriggers) {
            val (nextId, dependentConstraint) = _ruleToExpr(dr, t)
            val renamedConstraint = _renameConstraints(t, nextId, dr, dependentConstraint)
            allDependentConstraints :+= renamedConstraint
          }

          val _dependentConstraints = ctx.mkAnd(allDependentConstraints :_*)

          ctx.mkImplies(versionedConstraint, _dependentConstraints)
        }
        else {
          versionedConstraint
        }
      }



      val allConstraints = if (!falseBranch.ruleConstraints.simplify().isFalse && thisId > 0) {
        ctx.mkXor(trueBranchWithDependentConstraints,
          falseBranch.getVersionedConstraint(ctx, thisRelation, getIndices(thisRelation), thisVersion))
      }
      else {
        trueBranchWithDependentConstraints
      }

      (thisId, allConstraints)
    }

    val (_, expr) = _ruleToExpr(rule,trigger)
    val renamed = {
      /** Rename the latest version of state variables into [v_out] */
      var _from: Array[Expr[_]] = Array()
      var _to: Array[Expr[_]] = Array()
      for ((rel, latestVersion) <- versions) {
        val sort = getSort(ctx, rel, getIndices(rel))
        val (_, v_out) = makeStateVar(ctx, rel.name, sort)
        val v_id = ctx.mkConst(getVersionedVariableName(rel, latestVersion), sort)
        _from :+= v_id
        _to :+= v_out
      }
      expr.substitute(_from, _to).asInstanceOf[BoolExpr]
    }

    val simplified = simplifyByRenamingConst(renamed)
    simplified.asInstanceOf[BoolExpr]
  }

  private def getNamingConstraints(rule: Rule, dependentRule: Rule, trigger: Trigger, id1: Int, id2: Int): (BoolExpr, Array[Expr[_]], Array[Expr[_]]) = {
    val headLiteral = rule.head
    val bodyLiteral = views(dependentRule) match {
      case _: JoinView => {
        val _s = dependentRule.body.filter(_.relation == rule.head.relation)
        assert(_s.size==1)
        _s.head
      }
      case cv: CountView =>  cv.count.literal
      case mv: MaxView => mv.max.literal
      case sv: SumView => sv.sum.literal
      case _ => ???
    }

    /** If the trigger is delete tuple, rename the index variables only. */
    val indexOnly = trigger.isInstanceOf[DeleteTuple]
    val _indices = getIndices(headLiteral.relation)

    var expr: List[BoolExpr] = List()
    var from: List[Expr[_]] = List()
    var to: List[Expr[_]] = List()
    for ((v,i) <- headLiteral.fields.zip(bodyLiteral.fields).zipWithIndex) {
      if (v._1.name != "_" && v._2.name != "_" && (_indices.contains(i) || !indexOnly)) {
        val (x1,_) = paramToConst(ctx, v._1, getPrefix(id1))
        val (x2,_) = paramToConst(ctx, v._2, getPrefix(id2))
        expr +:= ctx.mkEq(x1,x2)
        to +:= x1
        from +:= x2
      }
    }
    (ctx.mkAnd(expr.toArray:_*), from.toArray, to.toArray)
  }

  private def simplifyByRenamingConst[T<:Sort](expr: Expr[T]): Expr[T] = {
    val exceptions = stateVars.flatMap(t=>Set(t._1,t._2))
    var pairs = extractEq(expr, exceptions)
    var newExpr = expr
    while (pairs.nonEmpty) {
      val renamed = newExpr.substitute(pairs.map(_._1), pairs.map(_._2))
      newExpr = renamed.simplify()
      pairs = extractEq(newExpr, exceptions)
    }
    newExpr
  }


  private def getPrefix(id: Int): String = s"i$id"

}
