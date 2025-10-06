package synthesis

import com.microsoft.z3.{BoolExpr, BoolSort, Context, Expr, Model}
import datalog.{Constant, Literal, Parameter, Program, Relation, ReservedRelation, Rule, SimpleRelation, SingletonRelation, Variable}
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

  case class Representation(map: Map[Relation, Set[Predicate]]) {
    def getPredicates(rel: Relation): Set[Predicate] = map(rel)
  }

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
  private def interpretModel(model: Model): Representation = {
    val mapping = encodings.map { case (rel, boolVars) =>
      val preds = predicates(rel).toList
      val assignments = boolVars.map { v =>
        val value = model.eval(v, true)
        value.isTrue
      }
      val selectedPreds = preds.zip(assignments).collect {
        case (p, true) => p
      }
      // println(s"Relation: ${rel.name}")
      // selectedPreds.foreach(p => println(s"  Selected: ${p}"))
      //rel -> assignments
      rel -> selectedPreds.toSet
    }
    Representation(mapping)
  }

  /** Rename relation in trace with the recv_ prefix */
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

  def synthesize(sketch: Program,
                 evaluatedTraces: Set[EvaluatedTrace],
                 maxSolutions: Int,
                 disambiguationTraces: Set[EvaluatedTrace]): Program = {
    val candidates = synthesizeMultiSolution(evaluatedTraces, maxSolutions)
    val selection = disambiguate(disambiguationTraces, candidates.toSet)
    makeProgram(sketch, selection)
  }

  private def disambiguate(disambiguationTraces: Set[EvaluatedTrace],
                           candidates: Set[Representation]): Representation = {

    def accept(trace: EvaluatedTrace, repr: Representation): Boolean = {
      trace.iterateTxAndStateBefore.forall{
        case (state, tx) =>
          val predicates = repr.getPredicates(tx.relation)
          predicates.forall(p => interpreter.evaluate(state, tx, p))
      }
    }

    def permissiveness(traces: Set[EvaluatedTrace], repr: Representation): Int =
      traces.count(t => accept(t, repr))

    val renamedTrace = disambiguationTraces.map(renameTxRelationInTrace)

    val permissivenessScores: Map[Representation, Int] = {
      candidates.map( c => c -> permissiveness(renamedTrace, c) ).toMap
    }
    candidates.maxBy(permissivenessScores)
  }

  /** Perform the synthesis given EvaluatedTraces and predicates, returning up to maxSolutions programs. */
  def synthesizeMultiSolution(evaluatedTraces: Set[EvaluatedTrace], maxSolutions: Int = 1): List[Representation] = {
    // rename relations in Evaluated Trace to ones with recv_ prefix
    val renamedTraces = evaluatedTraces.map(renameTxRelationInTrace)
    val traceConstraints = renamedTraces.map(t => {
      val evalResults = evaluatePredicates(t)
      makeConstraints(evalResults).asInstanceOf[Expr[BoolSort]]
    })
    val constraint = z3ctx.mkAnd(traceConstraints.toSeq:_*)
    val solver = z3ctx.mkSolver()
    solver.add(constraint)
    var solutions = List.empty[Representation]
    var found = 0
    while (found < maxSolutions && solver.check() == com.microsoft.z3.Status.SATISFIABLE) {
      val model = solver.getModel
      val selection = interpretModel(model)
      solutions = solutions :+ selection
      // Add blocking clause to prevent finding the same model again
      val block = encodings.flatMap { case (_, boolVars) =>
        boolVars.map { v =>
          val value = model.eval(v, true)
          if (value.isTrue) z3ctx.mkNot(v) else v
        }
      }.toSeq
      solver.add(z3ctx.mkOr(block:_*))
      found += 1
    }
    solutions
  }

  private def makeProgram(sketch: Program, repr: Representation): Program = {
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
          // // Find selected predicates for the transaction relation
          // val rel = txLit.relation
          // val selection: List[Boolean] = predicateSelection.getOrElse(rel, List.empty)
          // val candidates: List[Predicate] = predicates.getOrElse(rel, Set.empty).toList

          // // Pair candidates with selection booleans; if selection shorter than candidates, treat missing as false
          // val selectedPreds: Set[Predicate] = candidates.zipAll(selection, null, false)
          //   .collect { case (p: Predicate, true) => p }.toSet
          val selectedPreds = repr.getPredicates(txLit.relation)
          val newRule = makeRule(r, selectedPreds)
          println(s"[makeProgram] selected predicates: $selectedPreds")
          println(s"[makeProgram] new rule: $newRule")
          newRule
        }
        case None => r
      }
    }

    // Reuse program metadata from sketch
    // datalog.Program(newRules, sketch.interfaces, sketch.relationIndices, sketch.functions, sketch.violations, sketch.name)
    sketch.copy(rules=newRules)
  }

  private def makeRule(sketchRule: Rule, predicates: Set[Predicate]): Rule = {
    // Collect all binding literals from selected predicates' contexts
    var bindingLits: Set[datalog.Literal] = predicates.flatMap(p => p.context.bindingLiterals)

    // rename binding literal values to avoid naming collision
    // make an id, and then add prefix
    if (bindingLits.size > 1) {
      val updatedLits = bindingLits.zipWithIndex.map { case (lit, idx) =>
        val (_, valueParam) = interpreter.extractKeyValueVar(lit)
        val newName: String = s"${valueParam.name}_$idx"
        val newParameter = valueParam match {
          case _: Constant => throw new Exception(s"Expected variable at bidning literal: $lit")
          case v: Variable => v.copy(name=newName)
        }
        val newFields = lit.fields.map {
          case p if p == valueParam => newParameter
          case p => p
        }
        lit.copy(fields = newFields)
      }
      bindingLits = updatedLits.toSet
    }

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