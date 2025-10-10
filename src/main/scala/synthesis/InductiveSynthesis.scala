package synthesis

import com.microsoft.z3.{BoolExpr, BoolSort, Context, Expr, IntExpr, Model}
import datalog.{Constant, Literal, Parameter, Program, Relation, ReservedRelation, Rule, SimpleRelation, SingletonRelation, Variable}
import synthesis.EvaluatedTrace.shiftTrace
import imp.SolidityTranslator.transactionRelationPrefix
import scala.collection.mutable

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

    override def toString: String =
      map.map { case (rel, preds) => s"${rel.name}: ${preds.mkString("\n")}" }.mkString("\n")
  }

  val interpreter: Interpreter = Interpreter(interpreterContext)

  private val synthesisCache: mutable.Map[Set[EvaluatedTrace], List[Representation]] = mutable.Map.empty

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
   *     Conjunct all transaction constraints into a single Z3 BoolExpr and return.
   *     Assert that the trace cannot go through.
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

    val newSteps = old.steps.map { case (tx, state) =>
      val triggerRelation = toTxTriggerRelation(tx.relation)
      (tx.updateRelation(triggerRelation), state)
    }
    old.copy(steps = newSteps)
  }

  def synthesize(sketch: Program,
                 evaluatedTraces: Set[EvaluatedTrace],
                 maxSolutions: Int,
                 disambiguationTraces: Set[EvaluatedTrace]): Program = {
    val renamedDisambiguationTrace = disambiguationTraces.map(renameTxRelationInTrace)
    val renamedSafetyTrace = evaluatedTraces.map(renameTxRelationInTrace)

    // val candidates = synthesizeMultiSolution(renamedSafetyTrace, maxSolutions, renamedDisambiguationTrace)
    // val selection = disambiguate(renamedDisambiguationTrace, candidates)
    val selection = synthesizePerRelation(renamedSafetyTrace, maxSolutions,
      renamedDisambiguationTrace)
    makeProgram(sketch, selection)
  }

  /** Synthesize predicates for each relation independently, then combine best selections. */
  private def synthesizePerRelation(evaluatedTraces: Set[EvaluatedTrace],
                                       maxSolutions: Int,
                                       disambiguationTraces: Set[EvaluatedTrace]
                                     ): Representation = {
    // Group traces by last transaction relation
    val grouped: Map[Relation, Set[EvaluatedTrace]] =
      evaluatedTraces.groupBy(_.steps.last._1.relation)

    // For each relation, synthesize and disambiguate
    val allSolutions: Map[Relation, List[Representation]] = grouped.map { case (rel, traces) =>
      synthesisCache.get(traces) match {
        case Some(cached) =>
          println(s"[synthesizePerRelation] Using cached synthesis for relation ${rel.name}")
          rel -> cached
        case None => {
          val solver = z3ctx.mkOptimize()
          val traceConstraints = traces.map { t =>
            val evalResults = evaluatePredicates(t)
            makeConstraints(evalResults).asInstanceOf[Expr[BoolSort]]
          }
          val constraint = z3ctx.mkAnd(traceConstraints.toSeq: _*)
          solver.Add(constraint)

          // Block all other relations
          val otherRelations = encodings.keySet - rel
          val blockOthers = otherRelations.flatMap(encodings).map(z3ctx.mkNot)
          if (blockOthers.nonEmpty) solver.Add(z3ctx.mkAnd(blockOthers.toSeq: _*))

          // Maximize permissiveness for this relation
          val permissivenessObjective = makePermissivenessObjective(disambiguationTraces, encodings, z3ctx)
          solver.MkMaximize(permissivenessObjective)

          var selections = List.empty[Representation]
          var found = 0
          while (found < maxSolutions && solver.Check() == com.microsoft.z3.Status.SATISFIABLE) {
            val model = solver.getModel
            val selection = interpretModel(model)

            // Block this model for next candidate
            val block = encodings.flatMap { case (_, boolVars) =>
              boolVars.map { v =>
                val value = model.eval(v, true)
                if (value.isTrue) z3ctx.mkNot(v) else v
              }
            }.toSeq
            solver.Add(z3ctx.mkOr(block: _*))

            selections :+= selection
            found += 1
          }
          synthesisCache.update(traces,selections)
          rel -> selections
        }
      }
    }

    // Disambiguate for each relation after collecting all solutions
    val bestSelections: Map[Relation, Set[Predicate]] = allSolutions.map { case (rel, candidates) =>
      val best = disambiguate(disambiguationTraces, candidates)
      rel -> best.getPredicates(rel)
    }

    // Combine best selections for all relations, defaulting to empty set for missing keys
    val allRelations = encodings.keySet
    val completeSelections: Map[Relation, Set[Predicate]] = allRelations.map { rel =>
      rel -> bestSelections.getOrElse(rel, Set.empty[Predicate])
    }.toMap

    Representation(completeSelections)
  }

  private def permissiveness(disambiguationTraces: Set[EvaluatedTrace], repr: Representation): Int = {

    def accept(trace: EvaluatedTrace, repr: Representation): Boolean = {
      trace.iterateTxAndStateBefore.forall {
        case (state, tx) =>
          val predicates = repr.getPredicates(tx.relation)
          val accepts = predicates.map(p => p -> interpreter.evaluate(state, tx, p)).toMap
          predicates.forall(accepts)
      }
    }

    // val renamedTrace = disambiguationTraces.map(renameTxRelationInTrace)
    disambiguationTraces.count(t => accept(t, repr))
  }

  private def disambiguate(disambiguationTraces: Set[EvaluatedTrace],
                           candidates: List[Representation]): Representation = {

    val permissivenessScores: Map[Representation, Int] = {
      candidates.map(c => c -> permissiveness(disambiguationTraces, c)).toMap
    }
    // val best = candidates.maxBy(permissivenessScores)
    // val bestScore = permissivenessScores(best)
    // if (bestScore == 0) {
    //   println(s"[Warning]")
    // }
    // println(s"Selected $best with permissive score: $bestScore / ${disambiguationTraces.size}.")
    val maxScore = permissivenessScores.values.max
    val bestCandidates = candidates.filter(c => permissivenessScores(c) == maxScore)

    def numPredicates(repr: Representation): Int =
      repr.map.values.map(_.size).sum

    val minPredCount = bestCandidates.map(numPredicates).min
    val finalCandidates = bestCandidates.filter(c => numPredicates(c) == minPredCount)
    val best = finalCandidates.head
    println(s"Selected $best with permissive score: $maxScore / ${disambiguationTraces.size}, min predicates: $minPredCount.")

    best
  }

  /** Perform the synthesis given EvaluatedTraces and predicates, returning up to maxSolutions programs. */
  private def synthesizeMultiSolution(evaluatedTraces: Set[EvaluatedTrace],
                                       maxSolutions: Int,
                                       disambiguationTraces: Set[EvaluatedTrace]
                                     ): List[Representation] = {
    val traceConstraints = evaluatedTraces.map(t => {
      val evalResults = evaluatePredicates(t)
      makeConstraints(evalResults).asInstanceOf[Expr[BoolSort]]
    })
    val constraint = z3ctx.mkAnd(traceConstraints.toSeq: _*)
    val solver = z3ctx.mkOptimize()
    solver.Add(constraint)

    /** Metric: maximize permissiveness */
    val permissivenessObjective = makePermissivenessObjective(disambiguationTraces, encodings, z3ctx)
    solver.MkMaximize(permissivenessObjective)

    var solutions = List.empty[Representation]
    var found = 0

    while (found < maxSolutions && solver.Check() == com.microsoft.z3.Status.SATISFIABLE) {
      val model = solver.getModel
      val selection = interpretModel(model)

      val block = encodings.flatMap { case (_, boolVars) =>
        boolVars.map { v =>
          val value = model.eval(v, true)
          if (value.isTrue) z3ctx.mkNot(v) else v
        }
      }.toSeq
      solver.Add(z3ctx.mkOr(block: _*))

      solutions = solutions :+ selection
      found += 1
    }
    solutions
  }

  /** Perform the synthesis given EvaluatedTraces and predicates, returning up to maxSolutions programs. */
  private def synthesizeMultiSolutionBatch(evaluatedTraces: Set[EvaluatedTrace], maxSolutions: Int, disambiguationTraces: Set[EvaluatedTrace]): List[Representation] = {
    val traceConstraints = evaluatedTraces.map(t => {
      val evalResults = evaluatePredicates(t)
      makeConstraints(evalResults).asInstanceOf[Expr[BoolSort]]
    })
    val constraint = z3ctx.mkAnd(traceConstraints.toSeq: _*)
    // val solver = z3ctx.mkSolver()
    val solver = z3ctx.mkOptimize()
    solver.Add(constraint)

    /** Metric: minimize the number of selected predicats */
   //  val numSelection = {
   //    def boolToInt(b: BoolExpr): IntExpr = z3ctx.mkITE(b, z3ctx.mkInt(1), z3ctx.mkInt(0)).asInstanceOf[IntExpr]

   //    val boolVars = encodings.flatMap(_._2).toSeq
   //    z3ctx.mkAdd(boolVars.map(boolToInt): _*)
   //  }
   //
   //  solver.MkMinimize(numSelection)

    /** New metric: maximize permissiveness */
    val permissivenessObjective = makePermissivenessObjective(disambiguationTraces, encodings, z3ctx)
    solver.MkMaximize(permissivenessObjective)

    val batchSize = 50
    var solutions = List.empty[Representation]
    var found = 0

    while (found < maxSolutions && solver.Check() == com.microsoft.z3.Status.SATISFIABLE) {
      var batch = List.empty[Representation]
      var batchCount = 0

      // Collect a batch of candidate models
      while (batchCount < batchSize && solver.Check() == com.microsoft.z3.Status.SATISFIABLE) {
        val model = solver.getModel
        val selection = interpretModel(model)
        batch +:= selection

        // Block this model for next candidate
        val block = encodings.flatMap { case (_, boolVars) =>
          boolVars.map { v =>
            val value = model.eval(v, true)
            if (value.isTrue) z3ctx.mkNot(v) else v
          }
        }.toSeq
        solver.Add(z3ctx.mkOr(block: _*))

        batchCount += 1
      }

      // Validate and block common predicates in zero-score batch
      val (validSelections, blockClause) =
        validateAndBlockZeroScore(disambiguationTraces, batch, encodings, z3ctx)

      if (blockClause != null && !blockClause.isFalse) {
        solver.Add(blockClause)
      }

      // Add valid selections to solutions
      solutions ++= validSelections
      found += validSelections.size
    }

    if (solutions.isEmpty) {
      println(s"[synthesize] No solution found.")
    }

    /** single mode */

    // val (_, blockAlwaysFalsePredicates) = constantFalsePredicates(disambiguationTraces)
    // solver.Add(blockAlwaysFalsePredicates)

    // var solutions = List.empty[Representation]
    // var found = 0

    // while (found < maxSolutions && solver.Check() == com.microsoft.z3.Status.SATISFIABLE) {
    //   val model = solver.getModel
    //   val selection = interpretModel(model)

    //   // Add blocking clause to prevent finding the same model again
    //   val block = encodings.flatMap { case (_, boolVars) =>
    //     boolVars.map { v =>
    //       val value = model.eval(v, true)
    //       if (value.isTrue) z3ctx.mkNot(v) else v
    //     }
    //   }.toSeq
    //   solver.Add(z3ctx.mkOr(block: _*))

    //   val validated: Boolean = validate(disambiguationTraces, selection)
    //   if (validated) {
    //     println(s"Found ${found} solutions.")
    //     solutions = solutions :+ selection
    //     found += 1
    //   }
    // }
    solutions
  }

  private def makePermissivenessObjective(
                                           disambiguationTraces: Set[EvaluatedTrace],
                                           encodings: Map[Relation, List[BoolExpr]],
                                           z3ctx: Context
                                         ): IntExpr = {
    def boolToInt(b: BoolExpr): IntExpr = z3ctx.mkITE(b, z3ctx.mkInt(1), z3ctx.mkInt(0)).asInstanceOf[IntExpr]

    val disambigAcceptExprs: Seq[BoolExpr] = disambiguationTraces.toSeq.map { trace =>
      val evalResults = evaluatePredicates(trace)
      val txAccepts = evalResults.map { case (tx, boolList) =>
        val boolVars = encodings(tx.relation)
        val assertions = boolList.zipWithIndex.map { case (b, i) =>
          val premise = boolVars(i)
          val conclusion = z3ctx.mkBool(b)
          z3ctx.mkImplies(premise, conclusion)
        }
        z3ctx.mkAnd(assertions: _*)
      }
      z3ctx.mkAnd(txAccepts: _*)
    }

    val permissiveness: Seq[IntExpr] = disambigAcceptExprs.map(boolToInt)

    // z3ctx.mkAdd(permissiveness: _*).asInstanceOf[IntExpr]
    val permissivenessSum = z3ctx.mkAdd(permissiveness: _*).asInstanceOf[IntExpr]

    // Penalize by number of binding literals in each predicate
    val allBoolVars = encodings.values.flatten.toSeq
    // val numSelectedPredicates = z3ctx.mkAdd(allBoolVars.map(boolToInt): _*).asInstanceOf[IntExpr]
    // val penalty = z3ctx.mkMul(z3ctx.mkInt(1), numSelectedPredicates).asInstanceOf[IntExpr]
    val penaltyTerms = allBoolVars.map { b =>
      val boolExprToPredicate: Map[BoolExpr, Predicate] = encodings.flatMap { case (rel, boolVars) =>
        val preds = predicates(rel).toList
        boolVars.zip(preds)
      }.toMap
      val predicate = boolExprToPredicate(b)
      val bindingCount = predicate.context.bindingLiterals.size
      z3ctx.mkMul(z3ctx.mkInt(bindingCount), boolToInt(b)).asInstanceOf[IntExpr]
    }
    val penalty = z3ctx.mkAdd(penaltyTerms: _*).asInstanceOf[IntExpr]


    // Objective: maximize permissiveness - penalty
    z3ctx.mkSub(permissivenessSum, penalty).asInstanceOf[IntExpr]
  }


  private def makeProgram(sketch: Program, repr: Representation): Program = {
    // For each rule in the sketch, if it is a transaction rule, replace it with a rule
    // that includes the selected predicates' binding literals in the body and predicate functors
    // in the rule's functors set. Non-transaction rules are kept as-is.

    val newRules: Set[Rule] = sketch.rules.map { r =>
      // Check if this rule is a transaction rule by finding its transaction literal (if any)
      val txLiteralOpt = try {
        Some(PredicateEnumerator.extractTxLiteral(r))
      } catch {
        case _: Throwable => None
      }

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
    sketch.copy(rules = newRules)
  }

  private def makeRule(sketchRule: Rule, predicates: Set[Predicate]): Rule = {

    def _resolveCollision(_preds: Set[Predicate]): Set[Predicate] = {
      val groups = _preds.groupBy(_.context.bindingLiterals)
      if (groups.size == 1) return _preds

      var renamedPredicates = Set[Predicate]()
      // using one idx per group
      for ((group, idx) <- groups.zipWithIndex) {
        val (bindingLits, predGroup) = group
        val renameMapping = bindingLits.map(lit => {
          val (_, valueParam) = interpreter.extractKeyValueVar(lit)
          val newName: String = s"${valueParam.name}_$idx"
          val newParameter = valueParam match {
            case _: Constant => throw new Exception(s"Expected variable at binding literal: $lit")
            case v: Variable => v.copy(name = newName)
          }
          valueParam -> newParameter
        }).toMap

        val renamedGroup = predGroup.map(_.rename(renameMapping))
        renamedPredicates ++= renamedGroup
      }
      renamedPredicates
    }

    val renamedPredicates = _resolveCollision(predicates)
    val bindingLits = renamedPredicates.flatMap(p => p.context.bindingLiterals)

    // Collect all predicate functors
    val predicateFunctors: Set[datalog.Functor] = renamedPredicates.map(_.functor)

    // New body: original body plus binding literals (avoid duplicates)
    val newBody: Set[datalog.Literal] = sketchRule.body ++ bindingLits

    // New functors: original functors plus selected predicate functors
    val newFunctors: Set[datalog.Functor] = sketchRule.functors ++ predicateFunctors

    // if predicate refer to variable in the context literals,
    // add those literal to the rule as well.
    val addMsgSender: Set[datalog.Literal] = if (renamedPredicates.exists(_.referredMsgSender())) Set(synthesis.Context.msgSender) else Set.empty
    val addMsgValue: Set[datalog.Literal] = if (renamedPredicates.exists(_.referredMsgValue())) Set(synthesis.Context.msgValue) else Set.empty

    // Combine bodies: original body + binding literals + possible implicit context literals
    val finalBody: Set[datalog.Literal] = newBody ++ addMsgSender ++ addMsgValue

    // Keep aggregators unchanged
    val newAggregators = sketchRule.aggregators

    Rule(sketchRule.head, finalBody, newFunctors, newAggregators)
  }

  /** Validate the synthesis results. */
  def validate(disambiguationTrace: Set[EvaluatedTrace], selection: Representation): Boolean = {
    val score = permissiveness(disambiguationTrace, selection)
    score > 0
  }

  def validateAndBlockZeroScore(
    disambiguationTraces: Set[EvaluatedTrace],
    selectionBatch: List[Representation],
    encodings: Map[Relation, List[BoolExpr]],
    z3ctx: Context
  ): (List[Representation], BoolExpr) = {
    // Compute scores
    val scores = selectionBatch.map(sel => sel -> permissiveness(disambiguationTraces, sel)).toMap
    val nonZeroSelections = scores.filter(_._2 > 0).keys
    val zeroSelections = scores.filter(_._2 == 0).keys

    println(s"${zeroSelections.size} 0 permissive program found.")

    // For each zero-permissive selection, find minimal blocking subset
    val minimalBlocks = zeroSelections.toList.flatMap { sel =>
      val allSelected: Set[(Relation, Predicate)] = encodings.keys.flatMap { rel =>
        sel.getPredicates(rel).map(p => (rel, p))
      }.toSet

      // Greedy minimization: try removing each predicate and check permissiveness
      def isStillZero(subset: Set[(Relation, Predicate)]): Boolean = {
        val testSel = new Representation(encodings.keys.map { rel =>
          val preds = subset.collect { case (`rel`, p) => p }
          rel -> preds
        }.toMap)
        permissiveness(disambiguationTraces, testSel) == 0
      }

      // Try to minimize the subset
      var minimal = allSelected
      for ((rel, p) <- allSelected) {
        val candidate = minimal - ((rel, p))
        if (candidate.nonEmpty && isStillZero(candidate)) {
          minimal = candidate
        }
      }
      if (minimal.nonEmpty) Some(minimal) else None
    }

    // Create blocking clauses for each minimal subset
    val blockClauses = minimalBlocks.map { subset =>
      val vars = subset.flatMap { case (rel, p) =>
        val idx = predicates(rel).toList.indexOf(p)
        if (idx >= 0) Some(encodings(rel)(idx)) else None
      }
      // At least one must be false
      // z3ctx.mkOr(vars.map(z3ctx.mkNot).toSeq: _*)
      z3ctx.mkOr(vars.map(z3ctx.mkNot).toSeq: _*)
    }

    println(s"Block ${blockClauses.size} claues.")

    val block = if (blockClauses.nonEmpty) z3ctx.mkAnd(blockClauses: _*) else z3ctx.mkFalse()

    (nonZeroSelections.toList, block)
  }

  def constantFalsePredicates(disambiguationTrace: Set[EvaluatedTrace]): (Set[Predicate], BoolExpr) = {
    // For each relation, check which predicates are always false
    val alwaysFalsePredicates = predicates.flatMap { case (rel, preds) =>
      preds.filter { p =>
        disambiguationTrace.forall { trace =>
          // For each step, check if predicate is always false
          trace.iterateTxAndStateBefore.forall { case (state, tx) =>
            if (tx.relation == rel) !interpreter.evaluate(state, tx, p) else true
          }
        }
      }.map(p => (rel, p))
    }.toSet

    // Get the corresponding BoolExpr variables for these predicates
    val falseVars = alwaysFalsePredicates.flatMap { case (rel, p) =>
      val idx = predicates(rel).toList.indexOf(p)
      if (idx >= 0) Some(encodings(rel)(idx)) else None
    }

    val alwaysFalsePairsPerRelation: Map[Relation, Set[(Predicate, Predicate)]] = predicates.map { case (rel, preds) =>
      val predList = preds.toList
      val pairs = (for {
        i <- predList.indices
        j <- (i + 1) until predList.size
        p1 = predList(i)
        p2 = predList(j)
        if disambiguationTrace.forall { trace =>
          trace.iterateTxAndStateBefore.forall { case (state, tx) =>
            tx.relation != rel || !(interpreter.evaluate(state, tx, p1) && interpreter.evaluate(state, tx, p2))
          }
        }
      } yield (p1, p2)).toSet
      rel -> pairs
    }

    val alwaysFalsePairs: Set[(Relation, Predicate, Predicate)] =
      alwaysFalsePairsPerRelation.flatMap { case (rel, pairs) =>
        pairs.map { case (p1, p2) => (rel, p1, p2) }
      }.toSet

    // Get corresponding BoolExpr variables for these pairs
    val falsePairVars = alwaysFalsePairs.flatMap { case (rel, p1, p2) =>
      val idx1 = predicates(rel).toList.indexOf(p1)
      val idx2 = predicates(rel).toList.indexOf(p2)
      if (idx1 >= 0 && idx2 >= 0) Some((encodings(rel)(idx1), encodings(rel)(idx2))) else None
    }

    // Block clause: all these must be false
    val blockSingle: BoolExpr =
      if (falseVars.nonEmpty) z3ctx.mkAnd(falseVars.map(z3ctx.mkNot).toSeq: _*)
      else z3ctx.mkTrue()

    // Block clause: for each pair, at least one must be false
    val blockParis: BoolExpr =
      if (falsePairVars.nonEmpty)
        z3ctx.mkAnd(falsePairVars.map { case (v1, v2) => z3ctx.mkOr(z3ctx.mkNot(v1), z3ctx.mkNot(v2)) }.toSeq: _*)
      else z3ctx.mkTrue()

    val block = z3ctx.mkAnd(blockSingle,blockParis)


    println(s"Block always false predicates: ${alwaysFalsePredicates.mkString("\n")}")
    println(s"Block always false predicates pairs: ${alwaysFalsePairs.size}")

    // Return the set of predicates and the blocking clause
    (alwaysFalsePredicates.map(_._2), block)
  }


}