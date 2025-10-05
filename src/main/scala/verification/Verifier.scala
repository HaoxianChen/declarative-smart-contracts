package verification

import com.microsoft.z3.{ArithSort, ArrayExpr, ArraySort, BoolExpr, Context, Expr, IntSort, Sort, Status, TupleSort}
import datalog.{Balance, Constant, Parameter, Program, Relation, ReservedRelation, Rule, Send, SimpleRelation, SingletonRelation, Type, Variable}
import imp.SolidityTranslator.transactionRelationPrefix
import imp.Translator.getMaterializedRelations
import imp.{AbstractImperativeTranslator, DeleteTuple, ImperativeAbstractProgram, IncrementValue, InsertTuple, ReplacedByKey, Trigger}
import util.Misc.parseProgramFromRawString
import verification.Prove.{get_vars, prove}
import verification.RuleZ3Constraints.getVersionedVariableName
import verification.TransitionSystem.makeStateVar
import verification.Verifier.{_getDefaultConstraints, addBuiltInRules, simplifyByRenamingConst}
import verification.Z3Helper.{addressSize, extractEq, functorToZ3, getArraySort, getSort, initValue, literalToConst, makeTupleSort, paramToConst, relToTupleName, typeToSort, uintSize}
import view.{CountView, JoinView, MaxView, SumView, View}

import scala.collection.mutable.Queue

class Verifier(_program: Program, impAbsProgram: ImperativeAbstractProgram, debug: Boolean = false)
  extends AbstractImperativeTranslator(addBuiltInRules(_program), materializedRelations = Set(),
    isInstrument = true, monitorViolations = false, enableProjection = true, arithmeticOptimization = true) {

  private val program = addBuiltInRules(_program)

  private val ctx: Context = new Context()
  protected val relations: Set[Relation] = program.relations
  protected val indices: Map[SimpleRelation, List[Int]] = impAbsProgram.indices

  private val materializedRelations: Set[Relation] = {
    val fromStatements = getMaterializedRelations(impAbsProgram, program.interfaces)
    val violationRules = program.rules.filter(r => program.violations.contains(r.head.relation))
    val readByViolationRules = violationRules.flatMap(r => r.body.map(_.relation))
    
    // Auto-materialize relations referenced in temporal properties
    val fromTemporalPropertiesRaw = collectRelationsFromTemporalProperties(program.temporalProperties)
    
    // Filter out relations that cannot be materialized (SimpleRelations without indices)
    val fromTemporalProperties = fromTemporalPropertiesRaw.filter { rel =>
      rel match {
        case sr: SimpleRelation => indices.contains(sr) && indices(sr).nonEmpty
        case _: SingletonRelation => true
        case _: ReservedRelation => false
        case _ => true
      }
    }
    
    val skipped = fromTemporalPropertiesRaw -- fromTemporalProperties
    if (skipped.nonEmpty) {
      println(s"[Auto-Materialize] Skipping ${skipped.size} relations without indices: ${skipped.map(_.name).mkString(", ")}")
      println(s"  Note: These relations appear in temporal properties but cannot be materialized")
      println(s"  They may be transactions/events rather than state relations")
    }
    
    if (fromTemporalProperties.nonEmpty) {
      println(s"[Auto-Materialize] Adding ${fromTemporalProperties.size} relations from temporal properties: ${fromTemporalProperties.map(_.name).mkString(", ")}")
    }
    
    (fromStatements ++ readByViolationRules ++ fromTemporalProperties).filterNot(_.isInstanceOf[ReservedRelation])
  }
  
  /**
   * Collect all relations referenced in temporal properties
   */
  private def collectRelationsFromTemporalProperties(properties: List[temporal.TemporalProperty]): Set[Relation] = {
    properties.flatMap { prop =>
      collectRelationsFromExpr(prop.expr)
    }.toSet
  }
  
  /**
   * Recursively collect relations from temporal expressions
   */
  private def collectRelationsFromExpr(expr: temporal.TemporalExpr): Set[Relation] = {
    expr match {
      case temporal.TemporalExpr.FunctionCall(name, args) =>
        // Find relation by name
        val relation = program.relations.find(_.name == name).toSet
        // Recursively collect from arguments
        relation ++ args.flatMap(collectRelationsFromExpr)
      
      case temporal.TemporalExpr.Not(inner) =>
        collectRelationsFromExpr(inner)
      
      case temporal.TemporalExpr.And(left, right) =>
        collectRelationsFromExpr(left) ++ collectRelationsFromExpr(right)
      
      case temporal.TemporalExpr.Or(left, right) =>
        collectRelationsFromExpr(left) ++ collectRelationsFromExpr(right)
      
      case temporal.TemporalExpr.Imply(left, right) =>
        collectRelationsFromExpr(left) ++ collectRelationsFromExpr(right)
      
      case temporal.TemporalExpr.Eq(left, right) =>
        collectRelationsFromExpr(left) ++ collectRelationsFromExpr(right)
      
      case temporal.TemporalExpr.Neq(left, right) =>
        collectRelationsFromExpr(left) ++ collectRelationsFromExpr(right)
      
      case temporal.TemporalExpr.Lt(left, right) =>
        collectRelationsFromExpr(left) ++ collectRelationsFromExpr(right)
      
      case temporal.TemporalExpr.Le(left, right) =>
        collectRelationsFromExpr(left) ++ collectRelationsFromExpr(right)
      
      case temporal.TemporalExpr.Gt(left, right) =>
        collectRelationsFromExpr(left) ++ collectRelationsFromExpr(right)
      
      case temporal.TemporalExpr.Ge(left, right) =>
        collectRelationsFromExpr(left) ++ collectRelationsFromExpr(right)
      
      case temporal.TemporalExpr.Once(inner) =>
        collectRelationsFromExpr(inner)
      
      case temporal.TemporalExpr.Always(inner) =>
        collectRelationsFromExpr(inner)
      
      // Literals and identifiers don't reference relations
      case _ => Set()
    }
  }

  override val rulesToEvaluate: Set[Rule] = getRulesToEvaluate().filterNot(r => program.violations.contains(r.head.relation))

  val stateVars: Set[(Expr[_], Expr[_])] = materializedRelations.map(rel => {
    val sort = getSort(ctx, rel, getIndices(rel))
    makeStateVar(ctx, rel.name, sort)
  })

  val initializationRules = program.rules.filter(_r=>_r.body.exists(_l=>_l.relation.name=="constructor"))

  def getInitConstraintsPartial(_relation: Relation, _const: Expr[Sort]): (BoolExpr, Array[Expr[_]], Array[Type]) = {
    val initRule = initializationRules.find(_.head.relation==_relation)
    getInitConstraints(ctx, _relation, _const, indices, initRule, isQuantified = false)
  }
  private val invariantGenerator: InvariantGenerator = InvariantGenerator(ctx, program,
                                              materializedRelations, impAbsProgram.indices,
                                            getInitConstraintsPartial)

  private val queryConstraints: Map[Relation, BoolExpr] = program.functions.map(
    rel => (rel -> generateQueryConstraints(rel))).toMap

  private def generateQueryConstraints(relation: Relation): BoolExpr = {
    val allDefRules = program.rules.filter(_.head.relation==relation)
    val allConstraints = allDefRules.zipWithIndex.map {
      case (rule,i) => {
        val prefix = s"q$i"
        val expr = views(rule).getZ3QueryConstraint(ctx,prefix)
        /** rename following head variable */
        val from: Array[Expr[_]] = rule.head.fields.map(p => paramToConst(ctx,p,prefix)._1).toArray
        val to: Array[Expr[_]] = rule.head.fields.map(p => paramToConst(ctx,p,"")._1).toArray
        expr.substitute(from,to)
        /** todo: call this method recursively to generate query constraints */
      }
    }
    ctx.mkOr(allConstraints.toSeq:_*)
  }

  private def getIndices(relation: Relation): List[Int] = relation match {
    case sr:SimpleRelation => indices.getOrElse(sr, List())
    case SingletonRelation(name, sig, memberNames) => List()
    case relation: ReservedRelation => List()
  }

  def getTransitionSystem(): (TransitionSystem, Map[Relation, (Expr[_], Expr[_])], Expr[_], Set[com.microsoft.z3.BoolExpr]) = {
    val tr = TransitionSystem(program.name, ctx)

    /** Variable keeps track of the current transaction name. */
    val (transactionThis, transactionNext) = tr.newVar("transaction", ctx.mkStringSort())

    /**
     * Transaction parameter state variables
     *
     * For each interface (i.e., externally callable transaction), we create explicit
     * state variables to capture its input parameters per step. This enables us to
     * later reconstruct concrete arguments from the SMT model when building traces.
     *
     * Naming convention:
     *   tx_<relationName>_<paramName>
     * If a member name is unavailable, we fallback to:
     *   tx_<relationName>_arg<index>
     *
     * The mapping key is (relationName, paramIndex) -> (inVar, outVar)
     */
    val txParamVars: Map[(String, Int), (Expr[_], Expr[_])] = {
      var acc: Map[(String, Int), (Expr[_], Expr[_])] = Map()
      for (intf <- program.interfaces) {
        val rel = intf.relation
        val relName = rel.name
        val memberNames = rel.memberNames
        // Only create variables for input indices; return value (if any) is ignored here
        for (idx <- intf.inputIndices) {
          val baseName = if (idx >= 0 && idx < memberNames.length && memberNames(idx) != null && memberNames(idx).nonEmpty) {
            s"tx_${relName}_${memberNames(idx)}"
          } else {
            s"tx_${relName}_arg${idx}"
          }
          val sort = typeToSort(ctx, rel.sig(idx))
          val (v_in, v_out) = tr.newVar(baseName, sort)
          acc = acc.updated((relName, idx), (v_in, v_out))
        }

        /**
         * Also create shadow parameter variables for reserved relations
         * msgSender (p: address) and msgValue (amount: uint), so that for
         * recv_* style interfaces without explicit inputs we still surface
         * the caller and value per step in the SMT model.
         * Keys: (relName, -1) -> p, (relName, -2) -> amount
         */
        val (p_in, p_out) = tr.newVar(s"tx_${relName}_p", typeToSort(ctx, datalog.Type.addressType))
        acc = acc.updated((relName, -1), (p_in, p_out))
        val (amt_in, amt_out) = tr.newVar(s"tx_${relName}_amount", typeToSort(ctx, datalog.Type.uintType))
        acc = acc.updated((relName, -2), (amt_in, amt_out))
      }
      acc
    }

    /** 
     * State variable mapping for temporal property translation
     * Maps each materialized relation to its (current, next) state variables
     */
    var stateVarMap: Map[Relation, (Expr[_], Expr[_])] = Map()

    /** Generate initial constraints. */
    var initConditions: List[BoolExpr] = List()
    for (rel <- materializedRelations) {
      val sort = getSort(ctx, rel, getIndices(rel))
      val (v_in, v_out) = tr.newVar(rel.name, sort)
      stateVarMap = stateVarMap.updated(rel, (v_in, v_out))
      val (_init, _,_) = getInitConstraints(ctx, rel, v_in, indices, initializationRules.find(_.head.relation==rel))
      initConditions :+= _init
    }
    tr.setInit(ctx.mkAnd(initConditions.toArray:_*))

    val (fullTransitionCondition, transactionConditions) = getTransitionConstraints(transactionThis, transactionNext, txParamVars)
    tr.setTr(fullTransitionCondition, transactionConditions)
    (tr, stateVarMap, transactionThis, transactionConditions)
  }

  def traverseExpression(): Unit = {
    val (tr, _, _, _) = getTransitionSystem()
    val constraint = tr.getTrs().head

    val queue: Queue[Expr[_]] = Queue()
    for (arg <- constraint.getArgs) {
      queue.enqueue(arg)
    }
    while (queue.nonEmpty) {
      val e: Expr[_] = queue.dequeue()
      if (e.isConst) {
        println(e,e.getSort)
      }
      else {
        for (a <- e.getArgs) {
            queue.enqueue(a)
        }
      }
    }

  }

  /**
   * Format Z3 expression for better readability
   */
  private def formatZ3Expr(expr: com.microsoft.z3.BoolExpr, indent: Int = 0): String = {
    val str = expr.toString
    val indentStr = " " * indent
    
    // Simple formatting: break long expressions into multiple lines
    if (str.length < 100) {
      str
    } else {
      // Add line breaks after logical operators
      str.replaceAll("""\(forall """, "\n" + indentStr + "(forall ")
         .replaceAll("""\(and """, "\n" + indentStr + "  (and ")
         .replaceAll("""\(or """, "\n" + indentStr + "  (or ")
         .replaceAll("""\(=> """, "\n" + indentStr + "  (=> ")
         .replaceAll("""\(not """, "\n" + indentStr + "  (not ")
    }
  }
  
  def check(): Unit = {
    val (tr, stateVarMap, transactionThis, transactionConditions) = getTransitionSystem()
    
    // Check violation rules (legacy approach)
    for (vr <- program.violationRules) {
      val property = getProperty(ctx, vr)
      println(property)

      val isTransactionProperty = vr.body.exists(_.relation.name=="transaction")

      val (resInit, _resTr) = inductiveProve(ctx, tr, property, isTransactionProperty)
      val resTr = _resTr match {
        case Status.UNSATISFIABLE => _resTr
        case Status.UNKNOWN | Status.SATISFIABLE => {
          invariantGenerator.findInvariant(tr, vr) match {
            case Some(inv) => {
              // validateInvariant(inv, tr, property)
              val (_invInit, _invTr) = inductiveProve(ctx,tr,ctx.mkAnd(property,inv), isTransactionProperty)
              println(s"invariant: ${inv}")
              _invTr
            }
            case None => _resTr
          }
        }
      }
      println(s"Init: $resInit")
      println(s"Tr: $resTr")
    }
    
    // Check temporal properties (new approach)
    if (program.temporalProperties.nonEmpty) {
      println(s"\n=== Translating ${program.temporalProperties.size} temporal properties ===")
      
      try {
        // Create translator
        val translator = new temporal.TemporalPropertyTranslator(
          ctx, program, tr, stateVarMap, indices, Some(transactionThis)
        )
        
        // Translate all properties first (to collect all ONCE variables)
        val translatedProperties = scala.collection.mutable.ListBuffer[(temporal.TemporalProperty, com.microsoft.z3.BoolExpr)]()
        
        for (tp <- program.temporalProperties) {
          println(s"\n" + "="*80)
          println(s"Property (line ${tp.line}): ${tp.rawText}")
          tp.comment.foreach(c => println(s"  Comment: $c"))
          println(s"\n  Parsed AST:")
          println(s"    ${tp.expr}")
          
          // Collect free variables
          val freeVars = translator.collectFreeVariables(tp.expr)
          if (freeVars.nonEmpty) {
            println(s"\n  Free variables: ${freeVars.mkString(", ")}")
          }
          
          try {
            val z3Expr = translator.translate(tp)
            translatedProperties += ((tp, z3Expr))
            
            println(s"\n  ✓ Translation successful!")
            println(s"\n  Z3 Expression:")
            println(s"    ${formatZ3Expr(z3Expr, indent = 4)}")
            
            // Show variable bindings if quantified
            if (freeVars.nonEmpty) {
              println(s"\n  Quantification:")
              println(s"    ∀ ${freeVars.mkString(", ")} . (...)")
            }
            
          } catch {
            case e: Exception =>
              println(s"\n  ✗ Translation error: ${e.getMessage}")
              throw e  // Re-throw to stop processing
          }
        }
        
        println(s"\n" + "="*80)
        
        // After all translations, add ONCE constraints and verify
        println(s"\n--- ONCE Variable Analysis ---")
        
        val onceInitConstraints = translator.getOnceInitConstraints()
        var onceConstraintsAdded = false
        
        if (!onceInitConstraints.toString.equals("true")) {
          println(s"\n  ONCE variables created: ${translator.onceVars.size}")
          translator.onceVars.zipWithIndex.foreach { case ((key, (varIn, varNext, expr)), idx) =>
            println(s"\n  once_$idx:")
            println(s"    For expression: $expr")
            println(s"    Current state: $varIn")
            println(s"    Next state: $varNext")
          }
          
          println(s"\n  ONCE initialization constraints:")
          println(s"    ${onceInitConstraints}")
          
          // Try to generate update constraints with error handling
          try {
            val onceUpdateConstraints = translator.getOnceUpdateConstraints()
            println(s"\n  ONCE update constraints:")
            println(s"    ${onceUpdateConstraints}")
            
            // Add ONCE constraints to transition system
            println(s"\n  ⭐ Adding ONCE constraints to transition system...")
            val oldInit = tr.getInit()
            val newInit = ctx.mkAnd(oldInit, onceInitConstraints)
            tr.setInit(newInit)
            
            val oldTr = tr.getTr()
            val newTr = ctx.mkAnd(oldTr, onceUpdateConstraints)
            tr.setTr(newTr, transactionConditions)
            
            onceConstraintsAdded = true
            println(s"  ✓ ONCE constraints successfully added to transition system")
          } catch {
            case e: Exception =>
              println(s"\n  ✗ Cannot generate ONCE update constraints: ${e.getMessage}")
              println(s"  Note: Some ONCE expressions reference relations without state variables")
              println(s"        (e.g., transactions/events like withdraw(), refund())")
              println(s"  Workaround: ONCE variables will be created but not automatically updated")
          }
        } else {
          println(s"  (No ONCE operators used in properties)")
        }
        
        println(s"\n" + "="*80)
        println(s"=== Translation Summary ===")
        println(s"  Total properties translated: ${translatedProperties.size}")
        println(s"  State relations materialized: ${stateVarMap.size}")
        println(s"  ONCE variables created: ${translator.onceVars.size}")
        println(s"  ONCE constraints added: ${if (onceConstraintsAdded) "Yes" else "No"}")
        println(s"="*80)
        
        // Perform verification for each property
        if (translatedProperties.nonEmpty) {
          println(s"\n" + "="*80)
          println(s"=== Verification Results ===\n")
          
          translatedProperties.zipWithIndex.foreach { case ((tp, z3Expr), idx) =>
            println(s"Property ${idx + 1} (line ${tp.line}): ${tp.rawText}")
            tp.comment.foreach(c => println(s"  Comment: $c"))
            
            try {
              // Perform inductive proof
              val (resInit, resTr) = inductiveProve(ctx, tr, z3Expr, isTransactionProperty = false)
              
              println(s"  Init check: $resInit")
              println(s"  Transition check: $resTr")
              
              // Interpret results
              if (resInit == com.microsoft.z3.Status.UNSATISFIABLE && resTr == com.microsoft.z3.Status.UNSATISFIABLE) {
                println(s"  ✓ Property VERIFIED")
              } else if (resInit == com.microsoft.z3.Status.SATISFIABLE) {
                println(s"  ✗ Property VIOLATED in initial state")
              } else if (resTr == com.microsoft.z3.Status.SATISFIABLE) {
                println(s"  ✗ Property may be VIOLATED in some transition")
              } else {
                println(s"  ? Verification UNKNOWN")
              }
              
            } catch {
              case e: Exception =>
                println(s"  ✗ Verification error: ${e.getMessage}")
            }
            println()
          }
          
          println(s"="*80)
        }
        
      } catch {
        case e: Exception =>
          println(s"\n✗ Error during temporal property translation: ${e.getMessage}")
          e.printStackTrace()
      }
    }
  }

  def inductiveProve(ctx: Context, ts: TransitionSystem, property: BoolExpr, isTransactionProperty: Boolean=false): (Status, Status) = {
    val (resInit,_) = if (isTransactionProperty) {
      prove(ctx, ctx.mkImplies(ctx.mkAnd(ts.getInit(), ts.getTr()), ts.toPost(property)))
    }
    else {
      prove(ctx, ctx.mkImplies(ts.getInit(), property))
    }
    val f2 = ctx.mkImplies(ctx.mkAnd(property, ts.getTr()), ts.toPost(property))
    val (resTr,_) = prove(ctx, f2)
    // Debugging
    // for (_tr <- ts.getTrs()) {
    //   val _f = ctx.mkImplies(ctx.mkAnd(property, _tr), ts.toPost(property))
    //   val (_resTr, _model) = prove(ctx, _f)
    //   if (_resTr == Status.SATISFIABLE) {
    //     println(_resTr)
    //   }
    // }
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
    // assert(initRes==Status.UNSATISFIABLE)
    assert(trRes==Status.UNSATISFIABLE)
    val (initRes2, trRes2) = inductiveProve(ctx, tr, property, inv)
    // assert(initRes2 == Status.UNSATISFIABLE)
    assert(trRes2 == Status.UNSATISFIABLE)
  }

  def getProperty(ctx: Context, rule: Rule): BoolExpr = {
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
  
  /**
   * Alias for getProperty, for compatibility with BoundedModelChecker.
   */
  def getViolationCheck(ctx: Context, rule: Rule): BoolExpr = getProperty(ctx, rule)

  /**
   * Build the combined transition relation.
   *
   * Additionally, we assign transaction parameter state variables for the active
   * transaction branch, and keep others unchanged. This ensures parameters are
   * materialized in the model so that traces can include concrete arguments.
   */
  private def getTransitionConstraints(transactionThis: Expr[_], transactionNext: Expr[_],
                                       txParamVars: Map[(String, Int), (Expr[_], Expr[_])]): (BoolExpr, Set[BoolExpr]) = {

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
        val ruleConstraint: BoolExpr = ruleToExpr(rule, t)

        /**
         * Add the "unchanged" constraints for state relations and transaction
         * parameter variables (except those explicitly assigned for this branch).
         */
        val relName = t.relation.name
        val inputIndicesForRel: Set[Int] = program.interfaces.find(_.relation == t.relation)
          .map(_.inputIndices.toSet).getOrElse(Set()) ++ Set(-1, -2) // include reserved shadows

        // Exclude the parameters that will be assigned in this branch
        val excludeParamKeys: Set[(String, Int)] = inputIndicesForRel.map(idx => (relName, idx))
        val unchangedConstraints: List[BoolExpr] = getUnchangedConstraintsWithParams(ruleConstraint, txParamVars, excludeParamKeys)

        /** A boolean value indicating which transaction branch gets evaluate to true */
        val trConst = ctx.mkIntConst(s"${t.relation.name}$i")
        i += 1

        /** Indicator for transaction name. */
        val trNameConstraint = {
          val txName = ctx.mkString(t.relation.name)
          ctx.mkEq(transactionNext, txName)
        }

        /**
         * Assign concrete values to this transaction's input parameter variables.
         * We use the same naming prefix as the top-level rule encoding (i0), so
         * the parameter constants align with those appearing in ruleConstraint.
         */
        val paramAssignConstraints: List[BoolExpr] = {
          val maybeIntf = program.interfaces.find(_.relation == t.relation)
          maybeIntf.map { intf =>
            intf.inputIndices.flatMap { idx =>
              txParamVars.get((relName, idx)).map { case (_in, out) =>
                // Use the interface relation's formal parameter at position idx
                // to build a constant with the canonical prefix. This guarantees
                // the sort matches the tx parameter variable's sort.
                val formalParam = intf.relation.paramList(idx)
                val constExpr = paramToConst(ctx, formalParam, getPrefix(0))._1
                ctx.mkEq(out, constExpr)
              }
            } ++ {
              /**
               * Bind shadow variables to reserved relation constants
               * msgSender and msgValue, so they reflect caller and value.
               */
              val assigns: List[BoolExpr] = List(
                txParamVars.get((relName, -1)).map { case (_in, out) =>
                  val senderConst = ctx.mkConst("msgSender", typeToSort(ctx, datalog.Type.addressType))
                  ctx.mkEq(out, senderConst)
                },
                txParamVars.get((relName, -2)).map { case (_in, out) =>
                  val valueConst = ctx.mkConst("msgValue", typeToSort(ctx, datalog.Type.uintType))
                  ctx.mkEq(out, valueConst)
                }
              ).flatten
              assigns
            }
          }.getOrElse(List())
        }

        transactionConst +:= trConst
        transactionConstraints +:= ctx.mkAnd(
          (trNameConstraint
            :: ctx.mkEq(trConst, ctx.mkInt(1))
            :: ruleConstraint
            :: (unchangedConstraints ++ paramAssignConstraints)).toArray: _*)
      }
    }

    val fullConstraint = ctx.mkAnd(
      ctx.mkAnd(transactionConst.map(c => ctx.mkOr(ctx.mkEq(c,ctx.mkInt(0)), ctx.mkEq(c,ctx.mkInt(1)))).toArray:_*),
      ctx.mkEq(ctx.mkAdd(transactionConst.toArray:_*), ctx.mkInt(1)),
      ctx.mkOr(transactionConstraints.toArray:_*)
    )
    (fullConstraint, transactionConstraints.toSet)
  }

  /**
   * Generate "unchanged" constraints for both relation state variables and
   * transaction parameter state variables that are not modified in this branch.
   *
   * excludeParamKeys: a set of (relationName, paramIndex) to skip, because they
   * will be explicitly assigned in this branch.
   */
  private def getUnchangedConstraintsWithParams(_expr: BoolExpr,
                                                txParamVars: Map[(String, Int), (Expr[_], Expr[_])],
                                                excludeParamKeys: Set[(String, Int)]): List[BoolExpr] = {
    var unchangedConstraints: List[BoolExpr] = List()
    val allVars = Prove.get_vars(_expr)
    // Keep materialized relation state variables unchanged if their "out" var is not used in this branch
    for (rel <- materializedRelations) {
      val sort = getSort(ctx, rel, getIndices(rel))
      val (v_in, v_out) = makeStateVar(ctx, rel.name, sort)
      if (!allVars.contains(v_out)) {
        unchangedConstraints :+= ctx.mkEq(v_out, v_in)
      }
    }
    // Keep tx parameter variables unchanged unless they are assigned for this branch
    for ((key, (v_in, v_out)) <- txParamVars if !excludeParamKeys.contains(key)) {
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


      val queryConstraints: BoolExpr = getQueryConstraints(rule, getPrefix(thisId))

      val (_trueBranch, falseBranch) = view.getZ3Constraint(ctx, trigger, isMaterialized, getPrefix(thisId))
      val trueBranch = _trueBranch.copy(ruleConstraints = ctx.mkAnd(_trueBranch.ruleConstraints, queryConstraints))

      val trueBranchWithDependentConstraints = {

        if (trueBranch.nextTriggers.nonEmpty) {

          val dependentRulesAndTriggers: List[(Trigger,Rule)] = {
            var ret: List[(Trigger,Rule)] = List()
            for (t <- trueBranch.nextTriggers) {
              for (tr <- getTriggeredRules(t)) {
                ret :+= (t,tr)
                // for (nt <- views(tr).getTriggersForView(t)) {
                //    ret :+= (nt, tr)
                // }
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

      val allConstraintsSimplified = allConstraints.simplify().asInstanceOf[BoolExpr]

      (thisId, allConstraintsSimplified)
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

  private def getQueryConstraints(rule: Rule, prefix: String): BoolExpr = {
    val queries = rule.body.filter(lit=>program.functions.contains(lit.relation))

    val exprs = queries.map {
      case lit => {
        val expr = queryConstraints(lit.relation)
        /** rename */
        val from: Array[Expr[_]] = lit.relation.paramList.map(p => paramToConst(ctx, p, "")._1).toArray
        val to: Array[Expr[_]] = lit.fields.map(p => paramToConst(ctx, p, prefix)._1).toArray
        expr.substitute(from, to)
      }
    }
    ctx.mkAnd(exprs.toSeq:_*)
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


  def getInitConstraints(ctx: Context, relation: Relation, const: Expr[Sort],
                         indices: Map[SimpleRelation, List[Int]],
                         initRule: Option[Rule],
                         isQuantified:Boolean=true): (BoolExpr, Array[Expr[_]], Array[Type]) = initRule match {
    case Some(rule) => {
      val bodyConstraints: Set[BoolExpr] = rule.body.filterNot(_.relation.name==s"constructor").
        map(lit=>literalToConst(ctx,lit,getIndices(lit.relation),""))

      relation match {
        case sr: SimpleRelation => {
          val const0 = ctx.mkConst(s"_${const.toString}0", const.getSort)
          val (defaultConstraints,_keyConst,_keyTypes) = _getDefaultConstraints(ctx,relation, const0, indices, isQuantified)

          val (arraySort, keySorts, valueSort) = getArraySort(ctx, sr, indices(sr))
          val valueIndices = relation.sig.indices.filterNot(i=>indices(sr).contains(i))

          val initValues: Expr[Sort] = if (!valueSort.isInstanceOf[TupleSort]) {
            val _p = rule.head.fields(valueIndices.head)
            paramToConst(ctx,_p,s"")._1
          }.asInstanceOf[Expr[Sort]]
          else {
            val valueIndices = relation.sig.indices.filterNot(i=>indices(sr).contains(i))
            val valueTypes: Array[Type] = valueIndices.map(i=>sr.sig(i)).toArray
            val _initValues = valueTypes.map(t => initValue(ctx,t))
            valueSort.asInstanceOf[TupleSort].mkDecl().apply(_initValues:_*).asInstanceOf[Expr[Sort]]
          }
          val keys = indices(sr).map(i=>rule.head.fields(i))
          val keyConstArray: Array[Expr[_]] = keys.map(p=>paramToConst(ctx,p,s"")._1).toArray
          val storeConstraint = ctx.mkStore(const0.asInstanceOf[ArrayExpr[Sort,Sort]], keyConstArray, initValues)
          val matchConstraint = ctx.mkEq(const,storeConstraint)

          (ctx.mkAnd(defaultConstraints, matchConstraint),keyConstArray,_keyTypes)
        }
        case SingletonRelation(name, sig, memberNames) => {
          val assignExpr: BoolExpr = if (sig.size == 1) {
            val litConst = literalToConst(ctx,rule.head, List(), prefix = "")
            // ctx.mkEq(const, literalToConst(ctx,rule.head, List(), prefix = ""))
            litConst
          }
          else {
            ???
          }
          (ctx.mkAnd((bodyConstraints+assignExpr).toArray:_*), Array(), Array())

        }
        case relation: ReservedRelation => ???
      }
    }
    case None => _getDefaultConstraints(ctx,relation, const, indices, isQuantified)
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

  def _getDefaultConstraints(ctx: Context, relation: Relation, const: Expr[Sort],
                         indices: Map[SimpleRelation, List[Int]],
                         isQuantified:Boolean=true): (BoolExpr, Array[Expr[_]], Array[Type]) = relation match {
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