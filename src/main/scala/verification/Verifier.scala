package verification

import com.microsoft.z3.{ArithSort, ArrayExpr, BoolExpr, Context, Expr, IntSort, Sort, Status, TupleSort}
import datalog.{Balance, Constant, Parameter, Program, Relation, ReservedRelation, Rule, Send, SimpleRelation, SingletonRelation, Type, Variable}
import imp.SolidityTranslator.transactionRelationPrefix
import imp.Translator.getMaterializedRelations
import imp.{AbstractImperativeTranslator, DeleteTuple, ImperativeAbstractProgram, IncrementValue, InsertTuple, ReplacedByKey, Trigger}
import util.Misc.parseProgramFromRawString
import verification.Prove.{get_vars, prove}
import verification.RuleZ3Constraints.getVersionedVariableName
import verification.TransitionSystem.makeStateVar
import verification.Verifier.{addBuiltInRules, getInitConstraints, simplifyByRenamingConst}
import verification.Z3Helper.{addressSize, extractEq, functorToZ3, getArraySort, getSort, initValue, literalToConst, makeTupleSort, paramToConst, relToTupleName, typeToSort, uintSize}
import view.{CountView, JoinView, MaxView, SumView, View}

class Verifier(_program: Program, impAbsProgram: ImperativeAbstractProgram, debug: Boolean = false)
  extends AbstractImperativeTranslator(addBuiltInRules(_program), isInstrument = true, monitorViolations = false) {

  private val program = addBuiltInRules(_program)

  private val ctx: Context = new Context()
  protected val relations: Set[Relation] = program.relations
  protected val indices: Map[SimpleRelation, List[Int]] = impAbsProgram.indices

  private val materializedRelations: Set[Relation] = {
    val fromStatements = getMaterializedRelations(impAbsProgram, program.interfaces)
    val violationRules = program.rules.filter(r => program.violations.contains(r.head.relation))
    val readByViolationRules = violationRules.flatMap(r => r.body.map(_.relation))
    (fromStatements++readByViolationRules).filterNot(_.isInstanceOf[ReservedRelation])
  }

  override val rulesToEvaluate: Set[Rule] = getRulesToEvaluate().filterNot(r => program.violations.contains(r.head.relation))

  val stateVars: Set[(Expr[_], Expr[_])] = materializedRelations.map(rel => {
    val sort = getSort(ctx, rel, getIndices(rel))
    makeStateVar(ctx, rel.name, sort)
  })

  private val invariantGenerator: InvariantGenerator = InvariantGenerator(ctx, program,
                                              materializedRelations, impAbsProgram.indices)

  private def getIndices(relation: Relation): List[Int] = relation match {
    case sr:SimpleRelation => indices.getOrElse(sr, List())
    case SingletonRelation(name, sig, memberNames) => List()
    case relation: ReservedRelation => List()
  }

  def check(): Unit = {
    val violationRules: Set[Rule] = program.rules.filter(r => program.violations.contains(r.head.relation))
    val tr = TransitionSystem(program.name, ctx)

    var initConditions: List[BoolExpr] = List()
    for (rel <- materializedRelations) {
      val sort = getSort(ctx, rel, getIndices(rel))
      val (v_in, _) = tr.newVar(rel.name, sort)
      val (_init, _,_) = getInitConstraints(ctx, rel, v_in, indices)
      initConditions :+= _init
    }
    tr.setInit(ctx.mkAnd(initConditions.toArray:_*))

    val transitionConditions = getTransitionConstraints()
    tr.setTr(transitionConditions)

    for (vr <- violationRules) {
      val property = getProperty(ctx, vr)
      println(property)

      val (resInit, _resTr) = inductiveProve(ctx, tr, property)
      val resTr = _resTr match {
        case Status.UNSATISFIABLE => _resTr
        case Status.UNKNOWN | Status.SATISFIABLE => {
          invariantGenerator.findInvariant(tr, vr) match {
            case Some(inv) => {
              validateInvariant(inv, tr, property)
              println(s"invariant: ${inv}")
              Status.UNSATISFIABLE
            }
            case None => _resTr
          }
        }
      }
      println(s"Init: $resInit")
      println(s"Tr: $resTr")
    }
  }

  def inductiveProve(ctx: Context, ts: TransitionSystem, property: BoolExpr): (Status, Status) = {
    val (resInit,_) = prove(ctx, ctx.mkImplies(ts.getInit(), property))
    val f2 = ctx.mkImplies(ctx.mkAnd(property, ts.getTr()), ts.toPost(property))
    val (resTr,_) = prove(ctx, f2)
    (resInit, resTr)
  }

  def inductiveProve(ctx: Context, ts: TransitionSystem, property: BoolExpr, lemma: BoolExpr): (Status, Status) = {
    val (resInit,_) = prove(ctx, ctx.mkImplies(ts.getInit(), property))
    val f2 = ctx.mkImplies(ctx.mkAnd(property, ts.getTr(), lemma), ts.toPost(property))
    val (resTr,_) = prove(ctx, f2)
    (resInit, resTr)
  }

  private def validateInvariant(inv: BoolExpr, tr: TransitionSystem, property: BoolExpr): Unit = {
    val (initRes, trRes) = inductiveProve(ctx, tr, inv)
    assert(initRes==Status.UNSATISFIABLE)
    assert(trRes==Status.UNSATISFIABLE)
    val (initRes2, trRes2) = inductiveProve(ctx, tr, property, inv)
    assert(initRes2 == Status.UNSATISFIABLE)
    assert(trRes2 == Status.UNSATISFIABLE)
  }

  private def getProperty(ctx: Context, rule: Rule): BoolExpr = {
    /** Each violation query rule is translated into a property as follows:
     *  ! \E (V), P1 /\ P2 /\ ...
     *  where V is the set of variable appears in the rule body,
     *  P1, P2, ... are predicates translated from each rule body literal.
     *  */
    val prefix = "i"
    val bodyConstraints = rule.body.map(lit => literalToConst(ctx, lit, getIndices(lit.relation), prefix)).toArray
    val functorConstraints = rule.functors.map(f => functorToZ3(ctx,f, prefix)).toArray

    val keyConsts: Array[Expr[_]] = {
      var keys: Set[Parameter] = Set()
      for (lit <- rule.body) {
        val _indicies = getIndices(lit.relation)
        keys ++= _indicies.map(i=>lit.fields(i)).toSet
      }
      keys.map(p => paramToConst(ctx,p,prefix)._1).toArray
    }
    val constraints = {
      val _c = ctx.mkAnd(bodyConstraints++functorConstraints:_*)
      val renamed = simplifyByRenamingConst(_c, constOnly = false).simplify()
      renamed
    }

    if (keyConsts.nonEmpty) {
      ctx.mkNot(ctx.mkExists(
        keyConsts,
        constraints,
        1, null, null, ctx.mkSymbol("Q"), ctx.mkSymbol("skid2")
        )
      )
    }
    else {
      ctx.mkNot(constraints)
    }
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

          val updateAndDependentConstraints = ctx.mkAnd(trueBranch.getVersionedUpdateConstraint(ctx, thisRelation, getIndices(thisRelation), thisVersion),
            _dependentConstraints)

          if (thisId == 0) {
            ctx.mkAnd(trueBranch.ruleConstraints, updateAndDependentConstraints)
          }
          else {
            ctx.mkImplies(trueBranch.ruleConstraints, updateAndDependentConstraints)
          }
        }
        else {
          trueBranch.ruleConstraints
        }
      }



      val allConstraints = if (!falseBranch.ruleConstraints.simplify().isFalse && thisId > 0) {
        ctx.mkXor(trueBranchWithDependentConstraints, falseBranch.ruleConstraints)
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

  private def getPrefix(id: Int): String = s"i$id"

}

object Verifier {

  def simplifyByRenamingConst[T<:Sort](expr: Expr[T], constOnly:Boolean=true, exceptions: Set[Expr[_]]= Set()): Expr[T] = {
    def _isTempVar(e: Expr[_]): Boolean = {
      if (e.isApp) {
        if (e.getArgs.length == 0) {
          e.getSExpr.startsWith("i")
        }
        else false
      }
      else false
    }
    def _filterEquation(pair: (Expr[_], Expr[_])): Option[(Expr[_], Expr[_])] = {
      val (e1,e2) = pair
      if (_isTempVar(e1) && !exceptions.contains(e1)) {
        Some(pair)
      }
      else if (_isTempVar(e2) && !exceptions.contains(e2)) {
        Some(Tuple2(e2,e1))
      }
      else None
    }
    // val exceptions = stateVars.flatMap(t=>Set(t._1,t._2))
    // var pairs = extractEq(expr, exceptions).filter(_._1.getSExpr.startsWith("i"))
    var pairs = {
      val _ps = extractEq(expr,constOnly)
      val _filtered = _ps.flatMap(_filterEquation)
      _filtered
    }
    var newExpr = expr
    while (pairs.nonEmpty) {
      val renamed = newExpr.substitute(pairs.map(_._1), pairs.map(_._2))
      newExpr = renamed.simplify()
      // pairs = extractEq(newExpr, exceptions).filter(_._1.getSExpr.startsWith("i"))
      pairs = extractEq(newExpr,constOnly).flatMap(_filterEquation)
    }
    newExpr
  }

  def getInitConstraints(ctx: Context, relation: Relation, const: Expr[Sort], indices: Map[SimpleRelation, List[Int]], isQuantified:Boolean=true): (BoolExpr, Array[Expr[_]], Array[Type]) = relation match {
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

      val initConstraints = if (isQuantified) {
        ctx.mkForall(keyConstArray, ctx.mkEq(
          ctx.mkSelect(const.asInstanceOf[ArrayExpr[Sort,Sort]], keyConstArray),
          initValues),
          1, null, null, ctx.mkSymbol(s"Q${sr.name}"), ctx.mkSymbol(s"skid${sr.name}"))
      }
      else {
        ctx.mkEq(ctx.mkSelect(const.asInstanceOf[ArrayExpr[Sort,Sort]], keyConstArray), initValues)
      }
      (initConstraints, keyConstArray, keyTypes)
    }
    case SingletonRelation(name, sig, memberNames) => {
      val initConstraints = if (sig.size==1) {
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
      (initConstraints, Array(), Array())
    }
    case rel: ReservedRelation => ???
  }


  def addBuiltInRules(p: Program): Program = {
     val builtInRules = parseProgramFromRawString(BuiltInRules.ruleStr).rules
     p.addRules(builtInRules)
  }
}