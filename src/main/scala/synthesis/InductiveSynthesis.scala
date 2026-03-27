package synthesis

import com.microsoft.z3.{BoolExpr, BoolSort, Context, Expr, IntExpr, Model}
import datalog.{Arithmetic, Constant, Literal, Parameter, Program, Relation, ReservedRelation, Rule, SimpleRelation, SingletonRelation, Variable}
import synthesis.EvaluatedTrace.shiftTrace
import imp.SolidityTranslator.transactionRelationPrefix
import scala.collection.mutable
import synthesis.InductiveSynthesis.{RelationSolveStatus, SynthesisRunResult}

/** Given an EvaluatedTrace object, a set of predicates, return
 * a mapping, each transaction type to a bit vector encoding,
 * indicating which predicate is selected at each transaction's
 * condition guard. */
case class InductiveSynthesis(
  predicatesPerRule: Map[Rule,Set[Predicate]],
  interpreterContext: InterpreterContext,
  forbiddenSpec: ForbiddenSpec = ForbiddenSpec.empty,
  preselectedPredicates: Map[Relation, Set[Predicate]] = Map.empty
) {

  case class Representation(map: Map[Relation, Set[Predicate]]) {
    def getPredicates(rel: Relation): Set[Predicate] = map.getOrElse(rel, Set())

    override def toString: String =
      map.map { case (rel, preds) => s"${rel.name}: ${preds.mkString("\n")}" }.mkString("\n")
  }

  val interpreter: Interpreter = Interpreter(interpreterContext)

  private def dbgEnabledForRelation(rel: Relation): Boolean =
    rel.name == "recv_transfer" || rel.name == "recv_transferFrom"

  private def dbgJsonString(s: String): String =
    "\"" + Option(s).getOrElse("").flatMap {
      case '\\' => "\\\\"
      case '"' => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c => c.toString
    } + "\""

  private def dbgJsonArray(items: Seq[String]): String =
    items.map(dbgJsonString).mkString("[", ",", "]")

  private def bindingSlotKey(literal: Literal): String = {
    try {
      val (keyParams, _) = interpreter.extractKeyValueVar(literal)
      s"${literal.relation.name}(${keyParams.mkString(",")})"
    } catch {
      case _: Throwable => literal.toString
    }
  }

  private def effectivePredicates(rel: Relation, repr: Representation): Set[Predicate] =
    preselectedPredicates.getOrElse(rel, Set.empty) ++ repr.getPredicates(rel)

  private val synthesisCache: mutable.Map[List[EvaluatedTrace], List[Representation]] = mutable.Map.empty

  /** Reorganize and make the predicate lookup by relation efficient. */
  val predicates: Map[Relation, Set[Predicate]] = {
    predicatesPerRule.map { case (rule, preds) =>
      val txLiteral = PredicateEnumerator.extractTxLiteral(rule)
      txLiteral.relation -> preds
    }
  }

  val orderedPredicates: Map[Relation, List[Predicate]] = predicates.map { case (rel, preds) =>
    rel -> preds.toList.sortBy(_.canonicalString)
  }

  // Initialize Z3 context as a member
  val z3ctx: Context = new Context()

  // Initialize encoding as a member using makeEncoding
  val encodings: Map[Relation, List[BoolExpr]] = makeEncoding(z3ctx)

  private val predicateVars: Map[(Relation, Predicate), BoolExpr] = orderedPredicates.flatMap { case (rel, preds) =>
    preds.zip(encodings(rel)).map { case (pred, boolVar) =>
      (rel, pred) -> boolVar
    }
  }.toMap

  private val predicateVarsByKey: Map[Relation, Map[PredicateKey, BoolExpr]] = orderedPredicates.map { case (rel, preds) =>
    rel -> preds.zip(encodings(rel)).map { case (pred, boolVar) =>
      pred.stableKey -> boolVar
    }.toMap
  }

  private val preselectedKeysByRelation: Map[String, Set[PredicateKey]] = preselectedPredicates.map {
    case (rel, preds) => rel.name -> preds.map(_.stableKey)
  }

  /** For each Rule, create a list of Z3 Bool variables, one for each predicate.
   * The length of the list matches the number of predicates for that Rule. */
  def makeEncoding(z3ctx: Context): Map[Relation, List[BoolExpr]] = {
    orderedPredicates.map { case (rel, preds) =>
      val boolVars = preds.zipWithIndex.map { case (_, i) =>
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
      val preds = orderedPredicates(tx.relation)
      val results = preds.map(p => interpreter.evaluate(state, tx, p))
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
      val preds = orderedPredicates(rel)
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
      if (dbgEnabledForRelation(rel)) {
        // #region agent log
        DebugLogger.log(
          "InductiveSynthesis.scala:124",
          "interpretModel selected predicates",
          s"""{"relation":${dbgJsonString(rel.name)},"selectedPredicates":${dbgJsonArray(selectedPreds.toSeq.map(_.canonicalString).sorted)}}""",
          "erc1155-debug-pre",
          "H2"
        )
        // #endregion
      }
      rel -> selectedPreds.toSet
    }
    Representation(mapping)
  }

  private def relationPredicates(rel: Relation): List[Predicate] =
    orderedPredicates.getOrElse(rel, Nil)

  private def predicateVar(rel: Relation, predicate: Predicate): Option[BoolExpr] =
    predicateVars.get((rel, predicate))

  private def forbiddenClauses(targetRelation: Option[Relation]): Seq[(String, BoolExpr)] = {
    val relations = targetRelation.map(Set(_)).getOrElse(encodings.keySet)

    relations.toSeq.sortBy(_.name).flatMap { rel =>
      val relationName = rel.name
      val keyToVar = predicateVarsByKey.getOrElse(rel, Map.empty)
      val preselectedKeys = preselectedKeysByRelation.getOrElse(relationName, Set.empty)

      val singleClauses = forbiddenSpec.singleKeysFor(relationName).toSeq.sortBy(_.canonicalString).flatMap { key =>
        keyToVar.get(key).map { boolVar =>
          s"[forbid] Enforcing single predicate block for $relationName: ${key.canonicalString}" ->
            z3ctx.mkNot(boolVar)
        }
      }

      val pairClauses = forbiddenSpec.pairKeysFor(relationName).toSeq.sortBy(_.canonicalString).flatMap { pair =>
        val normalizedPair = pair.normalized
        val leftVar = keyToVar.get(normalizedPair.left)
        val rightVar = keyToVar.get(normalizedPair.right)
        val leftPreselected = preselectedKeys.contains(normalizedPair.left)
        val rightPreselected = preselectedKeys.contains(normalizedPair.right)

        (leftVar, rightVar, leftPreselected, rightPreselected) match {
          case (Some(v1), Some(v2), _, _) =>
            Some(
              s"[forbid] Enforcing predicate pair block for $relationName: ${normalizedPair.canonicalString}" ->
                z3ctx.mkOr(z3ctx.mkNot(v1), z3ctx.mkNot(v2))
            )
          case (Some(v1), None, _, true) =>
            Some(
              s"[forbid] Enforcing seeded/candidate pair block for $relationName by blocking: ${normalizedPair.left.canonicalString}" ->
                z3ctx.mkNot(v1)
            )
          case (None, Some(v2), true, _) =>
            Some(
              s"[forbid] Enforcing seeded/candidate pair block for $relationName by blocking: ${normalizedPair.right.canonicalString}" ->
                z3ctx.mkNot(v2)
            )
          case _ => None
        }
      }

      singleClauses ++ pairClauses
    }
  }

  private def addForbiddenClauses(solver: com.microsoft.z3.Optimize, targetRelation: Option[Relation]): Unit = {
    val clauses = forbiddenClauses(targetRelation)
    clauses.foreach { case (message, clause) =>
      println(message)
      solver.Add(clause)
    }
  }

  /** Rename relation in trace with the recv_ prefix */
  private def renameTxRelationInTrace(old: EvaluatedTrace): EvaluatedTrace = {

    def toTxTriggerRelation(relation: Relation): Relation = {
      // If BMC already returned a recv_* relation (the interface relation itself),
      // no renaming is needed. Only rename bare semantic relations (e.g. stake, unstake).
      if (relation.name.startsWith(transactionRelationPrefix)) relation
      else relation match {
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
                 evaluatedTraces: List[EvaluatedTrace],
                 maxSolutions: Int,
                 disambiguationTraces: Set[EvaluatedTrace]): Program = {
    synthesizeWithStatus(sketch, evaluatedTraces, maxSolutions, disambiguationTraces).program
  }

  def synthesizeWithStatus(sketch: Program,
                           evaluatedTraces: List[EvaluatedTrace],
                           maxSolutions: Int,
                           disambiguationTraces: Set[EvaluatedTrace]): SynthesisRunResult = {
    // Remove the first constructor transaction from each trace if present
    def stripConstructor(trace: EvaluatedTrace): EvaluatedTrace = {
      val steps = trace.steps
      if (steps.nonEmpty && steps.head._1.relation.name == "constructor") {
        val initState = steps.head._2
        trace.copy(initialState = initState, steps = steps.tail)
      } else
        trace
    }

    val renamedDisambiguationTrace = {
      val strippedDisambiguationTraces = disambiguationTraces.map(stripConstructor)
      strippedDisambiguationTraces.map(renameTxRelationInTrace)
    }
    val renamedSafetyTrace = {
      val strippedSafetyTraces = evaluatedTraces.map(stripConstructor)
      strippedSafetyTraces.map(renameTxRelationInTrace)
    }

    // val renamedDisambiguationTrace = disambiguationTraces.map(renameTxRelationInTrace)
    // val renamedSafetyTrace = evaluatedTraces.map(renameTxRelationInTrace)

    // val candidates = synthesizeMultiSolution(renamedSafetyTrace, maxSolutions, renamedDisambiguationTrace)
    // val selection = disambiguate(renamedDisambiguationTrace, candidates)
    val (selection, relationStatuses) = synthesizePerRelation(renamedSafetyTrace, maxSolutions,
      renamedDisambiguationTrace)
    SynthesisRunResult(makeProgram(sketch, selection), relationStatuses)
  }

  // debug info
  private def filterAndRankCandidates(
                                       evaluatedTraces: List[EvaluatedTrace],
                                       disambiguationTraces: Set[EvaluatedTrace]
                                     ): Map[Rule, List[(Predicate, Boolean, Int)]] = {

    predicatesPerRule.map { case (rule, preds) =>
      // Safely extract the transaction relation for the rule; if not present, treat as no occurrences.
      val txRelationOpt = try {
        Some(PredicateEnumerator.extractTxLiteral(rule).relation)
      } catch {
        case _: Throwable => None
      }

      val infos = preds.toList.sortBy(_.canonicalString).flatMap { p =>
        val (total, trueCount) = txRelationOpt match {
          case Some(txRel) =>
            val evalResults: Seq[Boolean] = evaluatedTraces.toSeq.flatMap { trace =>
              trace.iterateTxAndStateBefore.collect {
                case (st, tx) if tx.relation == txRel => interpreter.evaluate(st, tx, p) }
            }
            (evalResults.size, evalResults.count(identity))
          case None =>
            (0, 0)
        }

        // Predicate blocks at least one occurrence iff it's observed and not always true
        val blocksEvaluatedTrace = total > 0 && trueCount != total

        val permissiveness = txRelationOpt match {
          case Some(txRel) =>
            disambiguationTraces.count { trace =>
              val occ = trace.iterateTxAndStateBefore.collect {
                case (st, tx) if tx.relation == txRel => (st, tx)
              }
              occ.nonEmpty && occ.forall {
                case (st, tx) => interpreter.evaluate(st, tx, p)
              }
            }
          case None => 0
        }
        Some((p, blocksEvaluatedTrace, permissiveness))
      }.sortBy { case (_, _, perm) => -perm }

      rule -> infos
    }
  }


  /** Synthesize predicates for each relation independently, then combine best selections. */
  private def synthesizePerRelation(evaluatedTraces: List[EvaluatedTrace],
                                       maxSolutions: Int,
                                       disambiguationTraces: Set[EvaluatedTrace]
                                     ): (Representation, Map[Relation, RelationSolveStatus]) = {
    // Group traces by last transaction relation
    val grouped: Map[Relation, List[EvaluatedTrace]] =
      evaluatedTraces.groupBy(_.steps.last._1.relation)

    // For each relation, synthesize and disambiguate
    val allSolutionsWithStatus: Map[Relation, (List[Representation], RelationSolveStatus)] = grouped.map { case (rel, traces) =>
      synthesisCache.get(traces) match {
        case Some(cached) =>
          println(s"[synthesizePerRelation] Using cached synthesis for relation ${rel.name}")
          // Cache contains only SAT-produced models.
          rel -> (cached, RelationSolveStatus.SatFound)
        case None => {
          val solver = z3ctx.mkOptimize()
          addForbiddenClauses(solver, Some(rel))
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
          var satFound = false
          var checkStatus = solver.Check()
          while (found < maxSolutions && checkStatus == com.microsoft.z3.Status.SATISFIABLE) {
            satFound = true
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
            if (found < maxSolutions) {
              checkStatus = solver.Check()
            }
          }
          synthesisCache.update(traces,selections)
          val solveStatus =
            if (satFound) RelationSolveStatus.SatFound
            else checkStatus match {
              case com.microsoft.z3.Status.UNSATISFIABLE => RelationSolveStatus.Unsat
              case _ => RelationSolveStatus.Unknown
            }
          println(s"[synthesizePerRelation] Solver status for relation ${rel.name}: ${solveStatus.label}")
          rel -> (selections, solveStatus)
        }
      }
    }

    // Disambiguate for each relation after collecting all solutions
    val bestSelections: Map[Relation, Set[Predicate]] = allSolutionsWithStatus.map { case (rel, (candidates, _)) =>
      if (candidates.isEmpty) {
        println(s"[synthesizePerRelation] No Z3 solution found for relation ${rel.name}. Predicate pool size: ${orderedPredicates.getOrElse(rel, Nil).size}. Skipping.")
      }
      val best = disambiguate(disambiguationTraces, candidates)
      rel -> best.getPredicates(rel)
    }

    // Combine best selections for all relations, defaulting to empty set for missing keys
    val allRelations = encodings.keySet
    val completeSelections: Map[Relation, Set[Predicate]] = allRelations.map { rel =>
      rel -> bestSelections.getOrElse(rel, Set.empty[Predicate])
    }.toMap

    /** Turn on for debugging. */
    val debugInfo = filterAndRankCandidates(evaluatedTraces, disambiguationTraces)
    // println(debugInfo)


    val relationStatuses = allSolutionsWithStatus.map { case (rel, (_, status)) => rel -> status }
    (Representation(completeSelections), relationStatuses)
  }

  private def permissiveness(disambiguationTraces: Set[EvaluatedTrace], repr: Representation): Int = {

    def accept(trace: EvaluatedTrace, repr: Representation): Boolean = {
      trace.iterateTxAndStateBefore.forall {
        case (state, tx) =>
          val predicates = effectivePredicates(tx.relation, repr)
          val accepts = predicates.map(p => p -> interpreter.evaluate(state, tx, p)).toMap
          predicates.forall(accepts)
      }
    }

    // val renamedTrace = disambiguationTraces.map(renameTxRelationInTrace)
    disambiguationTraces.count(t => accept(t, repr))
  }

  private def disambiguate(disambiguationTraces: Set[EvaluatedTrace],
                           candidates: List[Representation]): Representation = {

    if (candidates.isEmpty) {
      println("[disambiguate] Warning: no candidate solutions found for this relation. Returning empty representation.")
      return Representation(Map.empty)
    }

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
    val debugRelations = best.map.keySet.filter(dbgEnabledForRelation)
    if (debugRelations.nonEmpty) {
      val relPayload = debugRelations.toSeq.sortBy(_.name).map { rel =>
        s"""{"relation":${dbgJsonString(rel.name)},"predicates":${dbgJsonArray(best.getPredicates(rel).toSeq.map(_.canonicalString).sorted)}}"""
      }.mkString("[", ",", "]")
      // #region agent log
      DebugLogger.log(
        "InductiveSynthesis.scala:420",
        "disambiguate best representation",
        s"""{"maxScore":$maxScore,"traceCount":${disambiguationTraces.size},"minPredCount":$minPredCount,"relations":$relPayload}""",
        "erc1155-debug-pre",
        "H3"
      )
      // #endregion
    }

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
    addForbiddenClauses(solver, None)
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
    addForbiddenClauses(solver, None)
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
      val txAccepts = shiftTrace(trace).map { case (state, tx) =>
        val preds = orderedPredicates(tx.relation)
        val boolList = preds.map(p => interpreter.evaluate(state, tx, p))
        val boolVars = encodings(tx.relation)
        val assertions = boolList.zipWithIndex.map { case (b, i) =>
          val premise = boolVars(i)
          val conclusion = z3ctx.mkBool(b)
          z3ctx.mkImplies(premise, conclusion)
        }
        val seededAccept =
          preselectedPredicates.getOrElse(tx.relation, Set.empty).forall(p => interpreter.evaluate(state, tx, p))
        val clauses = z3ctx.mkBool(seededAccept) +: assertions.toSeq
        z3ctx.mkAnd(clauses: _*)
      }
      z3ctx.mkAnd(txAccepts: _*)
    }

    val permissiveness: Seq[IntExpr] = disambigAcceptExprs.map(boolToInt)

    val permissivenessSum =
      if (permissiveness.nonEmpty) z3ctx.mkAdd(permissiveness: _*).asInstanceOf[IntExpr]
      else z3ctx.mkInt(0)

    // Penalize by number of binding literals in each predicate
    val allBoolVars = encodings.values.flatten.toSeq
    val penaltyTerms = allBoolVars.map { b =>
      val boolExprToPredicate: Map[BoolExpr, Predicate] = encodings.flatMap { case (rel, boolVars) =>
        val preds = relationPredicates(rel)
        boolVars.zip(preds)
      }.toMap
      val predicate = boolExprToPredicate(b)
      val bindingCount = predicate.context.bindingLiterals.size
      z3ctx.mkMul(z3ctx.mkInt(bindingCount), boolToInt(b)).asInstanceOf[IntExpr]
    }
    val penalty =
      if (penaltyTerms.nonEmpty) z3ctx.mkAdd(penaltyTerms: _*).asInstanceOf[IntExpr]
      else z3ctx.mkInt(0)


    // Objective: maximize permissiveness - penalty
    z3ctx.mkSub(permissivenessSum, penalty).asInstanceOf[IntExpr]
  }


  private def makeProgram(sketch: Program, repr: Representation): Program = {
    // For each rule in the sketch, if it is a transaction rule, replace it with a rule
    // that includes the selected predicates' binding literals in the body and predicate functors
    // in the rule's functors set. Non-transaction rules are kept as-is.

    val newRules: Set[Rule] = sketch.rules.diff(sketch.violationRules).map { r =>
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
          // println(s"[makeProgram] selected predicates: $selectedPreds")
          println(s"[makeProgram] new rule: $newRule")
          newRule
        }
        case None => r
      }
    }

    // Reuse program metadata from sketch
    // datalog.Program(newRules, sketch.interfaces, sketch.relationIndices, sketch.functions, sketch.violations, sketch.name)
    sketch.copy(rules = newRules++sketch.violationRules)
  }

  def augmentSketchWithPredicates(sketch: Program,
                                  candidates: Map[Rule, Set[Predicate]],
                                 ): Program = {
    val newRules: Set[Rule] = sketch.rules.diff(sketch.violationRules).map { r =>
      val selectedPreds = candidates.getOrElse(r, Set.empty)
      if (selectedPreds.nonEmpty) {
        val newRule = makeRule(r, selectedPreds)
        println(s"[makeProgram] new rule: $newRule")
        newRule
      } else {
        r
      }
    }
    sketch.copy(rules = newRules ++ sketch.violationRules)
  }

  private def makeRule(sketchRule: Rule, predicates: Set[Predicate]): Rule = {
    val targetTxLiteralOpt = try {
      Some(PredicateEnumerator.extractTxLiteral(sketchRule))
    } catch {
      case _: Throwable => None
    }

    def predicateReferencedParams(predicate: Predicate): Set[Parameter] = {
      val bindingParams = predicate.context.bindingLiterals.flatMap(_.fields)
      val functorParamsSet = PredicateEnumerator.functorParams(predicate.functor)
      val helperParams = predicate.helperFunctors.flatMap(assign => Arithmetic.extractParameters(assign.b))
      (bindingParams ++ functorParamsSet ++ helperParams).toSet
    }

    def adaptPredicateToRule(predicate: Predicate): Option[Predicate] = {
      targetTxLiteralOpt match {
        case None => Some(predicate)
        case Some(targetTxLiteral) =>
          val sourceTxLiteral = predicate.context.tx
          if (sourceTxLiteral.relation != targetTxLiteral.relation) return None
          val referencedParams = predicateReferencedParams(predicate)
          val compatible = sourceTxLiteral.fields.zip(targetTxLiteral.fields).forall {
            case (sourceVar: Variable, targetVar: Variable)
              if sourceVar.name != "_" && referencedParams.contains(sourceVar) =>
              targetVar.name != "_"
            case _ => true
          }
          if (!compatible) None
          else {
            val renameMap: Map[Parameter, Parameter] = sourceTxLiteral.fields.zip(targetTxLiteral.fields).collect {
              case (from: Variable, to: Variable)
                if from.name != "_" && to.name != "_" && from != to =>
                from -> to
            }.toMap
            val renamed = if (renameMap.nonEmpty) predicate.rename(renameMap) else predicate
            Some(renamed)
          }
      }
    }

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

    val debugTxRelationOpt = targetTxLiteralOpt.map(_.relation).filter(dbgEnabledForRelation)
    if (debugTxRelationOpt.nonEmpty) {
      val sketchBindingSlots = sketchRule.body.map(bindingSlotKey).toSeq.sorted
      val selectedBindings = predicates.toSeq.flatMap(_.context.bindingLiterals.map(bindingSlotKey)).sorted
      // #region agent log
      DebugLogger.log(
        "InductiveSynthesis.scala:731",
        "makeRule before collision resolution",
        s"""{"relation":${dbgJsonString(debugTxRelationOpt.get.name)},"head":${dbgJsonString(sketchRule.head.relation.name)},"sketchBodySlots":${dbgJsonArray(sketchBindingSlots)},"sketchFunctors":${dbgJsonArray(sketchRule.functors.toSeq.map(_.toString).sorted)},"selectedPredicates":${dbgJsonArray(predicates.toSeq.map(_.canonicalString).sorted)},"selectedBindingSlots":${dbgJsonArray(selectedBindings)}}""",
        "erc1155-debug-pre",
        "H1"
      )
      // #endregion
    }

    val compatiblePredicates = predicates.flatMap(adaptPredicateToRule)
    val renamedPredicates = _resolveCollision(compatiblePredicates)
    val bindingLits = renamedPredicates.flatMap(p => p.context.bindingLiterals)
    val helperAssigns: Set[datalog.Assign] = renamedPredicates.flatMap(_.helperFunctors)

    // Collect all predicate functors, but filter out any that directly contradict a functor
    // already present in the sketch rule.  This prevents the CEGIS Z3 solver from adding a
    // guard whose negation was already seeded from the violation-rule properties
    // (e.g. adding `amt<=0` when `amt>0` is already in the sketch body), which would produce
    // a rule that never fires.
    val allCandidateFunctors: Set[datalog.Functor] = renamedPredicates.map(_.functor)
    // Step 1: filter out candidates that contradict any functor already in the sketch.
    val filteredBySketch: Set[datalog.Functor] = allCandidateFunctors.filterNot { f =>
      sketchRule.functors.exists(existing =>
        try { datalog.Functor.contradicts(existing, f) }
        catch { case _: Throwable => false }
      )
    }
    // Step 2: filter out intra-candidate contradictions (e.g., both n<0 and 0==n
    // selected in the same iteration).  Keep the first of any contradicting pair.
    val predicateFunctors: Set[datalog.Functor] = filteredBySketch.foldLeft(Set.empty[datalog.Functor]) {
      (acc, f) =>
        val contradicted = acc.exists(existing =>
          try { datalog.Functor.contradicts(existing, f) } catch { case _: Throwable => false }
        )
        if (contradicted) acc else acc + f
    }

    if (debugTxRelationOpt.nonEmpty) {
      val sameSlotConflicts = renamedPredicates.toSeq.flatMap { pred =>
        pred.context.bindingLiterals.toSeq.flatMap { lit =>
          val slot = bindingSlotKey(lit)
          val sketchSlotMatch = sketchRule.body.exists(bodyLit => bindingSlotKey(bodyLit) == slot)
          if (sketchSlotMatch) {
            Some(s"$slot :: ${pred.functor}")
          } else None
        }
      }.sorted
      // #region agent log
      DebugLogger.log(
        "InductiveSynthesis.scala:759",
        "makeRule after contradiction filtering",
        s"""{"relation":${dbgJsonString(debugTxRelationOpt.get.name)},"renamedPredicates":${dbgJsonArray(renamedPredicates.toSeq.map(_.canonicalString).sorted)},"allCandidateFunctors":${dbgJsonArray(allCandidateFunctors.toSeq.map(_.toString).sorted)},"filteredBySketch":${dbgJsonArray(filteredBySketch.toSeq.map(_.toString).sorted)},"finalPredicateFunctors":${dbgJsonArray(predicateFunctors.toSeq.map(_.toString).sorted)},"sameSlotConflictsWithSketch":${dbgJsonArray(sameSlotConflicts)}}""",
        "erc1155-debug-pre",
        "H2"
      )
      // #endregion
    }

    // New body: original body plus binding literals (avoid duplicates)
    val newBody: Set[datalog.Literal] = sketchRule.body ++ bindingLits

    // New functors: original functors plus selected predicate functors
    val newFunctors: Set[datalog.Functor] = sketchRule.functors ++ predicateFunctors ++ helperAssigns

    // if predicate refer to variable in the context literals,
    // add those literal to the rule as well.
    val addMsgSender: Set[datalog.Literal] = if (renamedPredicates.exists(_.referredMsgSender())) Set(synthesis.Context.msgSender) else Set.empty
    val addMsgValue: Set[datalog.Literal] = if (renamedPredicates.exists(_.referredMsgValue())) Set(synthesis.Context.msgValue) else Set.empty

    // Combine bodies: original body + binding literals + possible implicit context literals
    val finalBody: Set[datalog.Literal] = newBody ++ addMsgSender ++ addMsgValue

    // Keep aggregators unchanged
    val newAggregators = sketchRule.aggregators

    if (debugTxRelationOpt.nonEmpty) {
      // #region agent log
      DebugLogger.log(
        "InductiveSynthesis.scala:776",
        "makeRule final rule",
        s"""{"relation":${dbgJsonString(debugTxRelationOpt.get.name)},"finalBody":${dbgJsonArray(finalBody.toSeq.map(_.toString).sorted)},"finalFunctors":${dbgJsonArray((sketchRule.functors ++ predicateFunctors ++ helperAssigns).toSeq.map(_.toString).sorted)}}""",
        "erc1155-debug-pre",
        "H1"
      )
      // #endregion
    }

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
        predicateVar(rel, p)
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
      predicateVar(rel, p)
    }

    val alwaysFalsePairsPerRelation: Map[Relation, Set[(Predicate, Predicate)]] = predicates.map { case (rel, preds) =>
      val predList = preds.toList.sortBy(_.canonicalString)
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
      predicateVar(rel, p1).flatMap { v1 =>
        predicateVar(rel, p2).map(v2 => (v1, v2))
      }
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

object InductiveSynthesis {
  sealed trait RelationSolveStatus {
    def label: String
  }
  object RelationSolveStatus {
    case object SatFound extends RelationSolveStatus { val label: String = "SAT" }
    case object Unsat extends RelationSolveStatus { val label: String = "UNSAT" }
    case object Unknown extends RelationSolveStatus { val label: String = "UNKNOWN" }
  }

  case class SynthesisRunResult(program: Program, relationStatuses: Map[Relation, RelationSolveStatus])
}