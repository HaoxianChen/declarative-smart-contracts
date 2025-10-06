package synthesis

import com.microsoft.z3.{BoolExpr, Context, Model}
import datalog.{Program, Relation, ReservedRelation, Rule, SimpleRelation, SingletonRelation}
import synthesis.EvaluatedTrace.shiftTrace
import imp.SolidityTranslator.transactionRelationPrefix

/** Given an EvaluatedTrace object, a set of predicates, return
 * a mapping, each transaction type to a bit vector encoding,
 * indicating which predicate is selected at each transaction's
 * condition guard. */
case class InductiveSynthesis(
  predicatesPerRule: Map[Rule,Set[Predicate]],
  interpreterContext: InterpreterContext
) {
  val interpreter: Interpreter = Interpreter(interpreterContext)

  /** Reorganize and make the predicate lookup by relation efficient. */
  val predicates: Map[Relation, Set[Predicate]] = {
    predicatesPerRule.map { case (rule, preds) =>
      val txLiteral = PredicateEnumerator.extractTxLiteral(rule)
      txLiteral.relation -> preds
    }
  }

  // Initialize Z3 context as a member
  val z3ctx: Context = new Context()

  // Initialize encoding as a member using makeEncoding
  val encodings: Map[Relation, List[BoolExpr]] = makeEncoding(z3ctx)

  /** For each Rule, create a list of Z3 Bool variables, one for each predicate.
    * The length of the list matches the number of predicates for that Rule. */
  def makeEncoding(z3ctx: Context): Map[Relation, List[BoolExpr]] = {
    predicates.map { case (rel, preds) =>
      val boolVars = preds.toList.zipWithIndex.map { case (_, i) =>
        z3ctx.mkBoolConst(s"pred_${rel.name}_$i")
      }
      rel -> boolVars
    }
  }

  /**
    * For a given EvaluatedTrace, return a sequence of (Transaction, List[Boolean])
    * where each transaction is paired with the previous state and the predicate
    * evaluation results for that state and transaction.
    */
  def evaluatePredicates(evaluatedTrace: EvaluatedTrace): Seq[(Transaction, List[Boolean])] = {
    val pairs = shiftTrace(evaluatedTrace)
    pairs.map { case (state, tx) =>
      val preds = predicates(tx.relation)
      val results = preds.toList.map(p => interpreter.evaluate(state, tx, p))
      (tx, results)
    }
  }

  /**
    * For each (tx, boolList) in evalResults:
    *   - Fetch the list of Z3 Bool variables from encodings for that relation.
    *   - For each i, assert boolVar[i] implies boolList[i].
    *   - Conjunct all assertions for the transaction.
    * Conjunct all transaction constraints into a single Z3 BoolExpr and return.
   *  Assert that the trace cannot go through.
    */
  def makeConstraints(evalResults: Seq[(Transaction, List[Boolean])]): BoolExpr = {
    val txConstraints = evalResults.map { case (tx, boolList) =>
      val boolVars = encodings(tx.relation)
      val assertions = boolList.zipWithIndex.map { case (b, i) =>
        val premise = boolVars(i)
        val conclusion = z3ctx.mkBool(b)
        z3ctx.mkImplies(premise, conclusion)
      }
      z3ctx.mkAnd(assertions: _*)
    }
    val acceptTrace = z3ctx.mkAnd(txConstraints: _*)
    z3ctx.mkNot(acceptTrace)
  }

  /**
    * Given a Z3 model, extract for each relation the list of predicate assignments
    * (true if the predicate variable is true in the model, false otherwise).
    * Also print the selected predicates for each relation.
    */
  private def interpretModel(model: Model): Map[Relation, List[Boolean]] = {
    encodings.map { case (rel, boolVars) =>
      val preds = predicates(rel).toList
      val assignments = boolVars.map { v =>
        val value = model.eval(v, true)
        value.isTrue
      }
      val selectedPreds = preds.zip(assignments).collect {
        case (p, true) => p
      }
      println(s"Relation: ${rel.name}")
      selectedPreds.foreach(p => println(s"  Selected: ${p}"))
      rel -> assignments
    }
  }

  /** Rename relatio in trace with the recv_ prefix */
  private def renameTxRelationInTrace(old: EvaluatedTrace): EvaluatedTrace = {

    def toTxTriggerRelation(relation: Relation): Relation = {
      require(!relation.name.startsWith(transactionRelationPrefix), "Assuming non tx relation")
      relation match {
        case SimpleRelation(name, sig, memberNames) =>
          SimpleRelation(s"$transactionRelationPrefix$name", sig, memberNames)
        case SingletonRelation(name, sig, memberNames) => ???
        case relation: ReservedRelation => ???
      }
    }

    val newSteps = old.steps.map{case (tx, state) =>
      val triggerRelation = toTxTriggerRelation(tx.relation)
      (tx.updateRelation(triggerRelation), state)
    }
    old.copy(steps=newSteps)
  }

  /** Perform the synthesis given an EvaluatedTrace and predicates. */
  def synthesize(sketch:Program, evaluatedTrace: EvaluatedTrace): Program = {
    // rename relations in Evaluated Trace to ones with recv_ prefix
    val renamedTrace = renameTxRelationInTrace(evaluatedTrace)
    val evalResults = evaluatePredicates(renamedTrace)
    val constraint = makeConstraints(evalResults)
    val solver = z3ctx.mkSolver()
    solver.add(constraint)
    val status = solver.check()
    val selection: Map[Relation, List[Boolean]] = if (status == com.microsoft.z3.Status.SATISFIABLE) {
      val model = solver.getModel
      val selection = interpretModel(model)
      // Return the predicate assignments discovered by the solver
      selection
    } else {
      Map.empty
    }
    makeProgram(sketch, selection)
  }

  private def makeProgram(sketch: Program, predicateSelection: Map[Relation, List[Boolean]]): Program = {
    // For each rule in the sketch, if it is a transaction rule, replace it with a rule
    // that includes the selected predicates' binding literals in the body and predicate functors
    // in the rule's functors set. Non-transaction rules are kept as-is.

    val newRules: Set[Rule] = sketch.rules.map { r =>
      // Check if this rule is a transaction rule by finding its transaction literal (if any)
      val txLiteralOpt = try {
        Some(PredicateEnumerator.extractTxLiteral(r))
      } catch { case _: Throwable => None }

      txLiteralOpt match {
        case Some(txLit) => {
          // Find selected predicates for the transaction relation
          val rel = txLit.relation
          val selection: List[Boolean] = predicateSelection.getOrElse(rel, List.empty)
          val candidates: List[Predicate] = predicates.getOrElse(rel, Set.empty).toList

          // Pair candidates with selection booleans; if selection shorter than candidates, treat missing as false
          val selectedPreds: Set[Predicate] = candidates.zipAll(selection, null, false)
            .collect { case (p: Predicate, true) => p }.toSet

          val newRule = makeRule(r, selectedPreds)
          println(s"[makeProgram] selected predicates: $selectedPreds")
          println(s"[makeProgram] new rule: $newRule")
          newRule
        }
        case None => r
      }
    }

    // Reuse program metadata from sketch
    datalog.Program(newRules, sketch.interfaces, sketch.relationIndices, sketch.functions, sketch.violations, sketch.name)
  }

  private def makeRule(sketchRule: Rule, predicates: Set[Predicate]): Rule = {
    // Collect all binding literals from selected predicates' contexts
    val bindingLits: Set[datalog.Literal] = predicates.flatMap(p => p.context.bindingLiterals)

    // Collect all predicate functors
    val predicateFunctors: Set[datalog.Functor] = predicates.map(_.functor)

    // New body: original body plus binding literals (avoid duplicates)
    val newBody: Set[datalog.Literal] = sketchRule.body ++ bindingLits

    // New functors: original functors plus selected predicate functors
    val newFunctors: Set[datalog.Functor] = sketchRule.functors ++ predicateFunctors

    // if predicate refer to variable in the context literals,
    // add those literal to the rule as well.
    val addMsgSender: Set[datalog.Literal] = if (predicates.exists(_.referredMsgSender())) Set(synthesis.Context.msgSender) else Set.empty
    val addMsgValue: Set[datalog.Literal] = if (predicates.exists(_.referredMsgValue())) Set(synthesis.Context.msgValue) else Set.empty

    // Combine bodies: original body + binding literals + possible implicit context literals
    val finalBody: Set[datalog.Literal] = newBody ++ addMsgSender ++ addMsgValue

    // Keep aggregators unchanged
    val newAggregators = sketchRule.aggregators

    Rule(sketchRule.head, finalBody, newFunctors, newAggregators)
  }

  /** Validate the synthesis results. */
  def validate(): Boolean = {
    // Not implemented: placeholder returns false
    false
  }
}