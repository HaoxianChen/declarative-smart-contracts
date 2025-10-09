package synthesis

import com.microsoft.z3.{BoolExpr, BoolSort, Context, Expr, IntExpr, Model}
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

    override def toString: String =
      map.map { case (rel, preds) => s"${rel.name}: ${preds.mkString("\n")}" }.mkString("\n")
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
    val candidates = synthesizeMultiSolution(evaluatedTraces, maxSolutions, disambiguationTraces)
    val selection = disambiguate(disambiguationTraces, candidates)
    makeProgram(sketch, selection)
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

    val renamedTrace = disambiguationTraces.map(renameTxRelationInTrace)
    renamedTrace.count(t => accept(t, repr))
  }

  private def disambiguate(disambiguationTraces: Set[EvaluatedTrace],
                           candidates: List[Representation]): Representation = {

    val permissivenessScores: Map[Representation, Int] = {
      candidates.map(c => c -> permissiveness(disambiguationTraces, c)).toMap
    }
    val best = candidates.maxBy(permissivenessScores)
    val bestScore = permissivenessScores(best)
    if (bestScore == 0) {
      println(s"[Warning]")
    }
    println(s"Selected $best with permissive score: $bestScore / ${disambiguationTraces.size}.")
    best
  }

  /** Perform the synthesis given EvaluatedTraces and predicates, returning up to maxSolutions programs. */
  def synthesizeMultiSolution(evaluatedTraces: Set[EvaluatedTrace], maxSolutions: Int, disambiguationTraces: Set[EvaluatedTrace]): List[Representation] = {
    // rename relations in Evaluated Trace to ones with recv_ prefix
    val renamedTraces = evaluatedTraces.map(renameTxRelationInTrace)
    val traceConstraints = renamedTraces.map(t => {
      val evalResults = evaluatePredicates(t)
      makeConstraints(evalResults).asInstanceOf[Expr[BoolSort]]
    })
    val constraint = z3ctx.mkAnd(traceConstraints.toSeq: _*)
    // val solver = z3ctx.mkSolver()
    val solver = z3ctx.mkOptimize()
    solver.Add(constraint)

    /** Metric: minimize the number of selected predicats */
    val numSelection = {
      def boolToInt(b: BoolExpr): IntExpr = z3ctx.mkITE(b, z3ctx.mkInt(1), z3ctx.mkInt(0)).asInstanceOf[IntExpr]

      val boolVars = encodings.flatMap(_._2).toSeq
      z3ctx.mkAdd(boolVars.map(boolToInt): _*)
    }

    solver.MkMinimize(numSelection)

    // val batchSize = 50
    // var solutions = List.empty[Representation]
    // var found = 0

    // while (found < maxSolutions && solver.Check() == com.microsoft.z3.Status.SATISFIABLE) {
    //   var batch = List.empty[Representation]
    //   var batchCount = 0

    //   // Collect a batch of candidate models
    //   while (batchCount < batchSize && solver.Check() == com.microsoft.z3.Status.SATISFIABLE) {
    //     val model = solver.getModel
    //     val selection = interpretModel(model)
    //     batch +:= selection

    //     // Block this model for next candidate
    //     val block = encodings.flatMap { case (_, boolVars) =>
    //       boolVars.map { v =>
    //         val value = model.eval(v, true)
    //         if (value.isTrue) z3ctx.mkNot(v) else v
    //       }
    //     }.toSeq
    //     solver.Add(z3ctx.mkOr(block: _*))

    //     batchCount += 1
    //   }

    //   // Validate and block common predicates in zero-score batch
    //   val (validSelections, blockClause) =
    //     validateAndBlockZeroScore(disambiguationTraces, batch, encodings, z3ctx)

    //   if (blockClause != null && !blockClause.isFalse) {
    //     solver.Add(blockClause)
    //   }

    //   // Add valid selections to solutions
    //   solutions ++= validSelections
    //   found += validSelections.size
    // }

    // if (solutions.isEmpty) {
    //   println(s"[synthesize] No solution found.")
    // }

    val (_, blockAlwaysFalsePredicates) = constantFalsePredicates(disambiguationTraces)
    solver.Add(blockAlwaysFalsePredicates)

    var solutions = List.empty[Representation]
    var found = 0

    while (found < maxSolutions && solver.Check() == com.microsoft.z3.Status.SATISFIABLE) {
      val model = solver.getModel
      val selection = interpretModel(model)

      // Add blocking clause to prevent finding the same model again
      val block = encodings.flatMap { case (_, boolVars) =>
        boolVars.map { v =>
          val value = model.eval(v, true)
          if (value.isTrue) z3ctx.mkNot(v) else v
        }
      }.toSeq
      solver.Add(z3ctx.mkOr(block: _*))

      val validated: Boolean = validate(disambiguationTraces, selection)
      if (validated) {
        println(s"Found ${found} solutions.")
        solutions = solutions :+ selection
        found += 1
      }
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

    // Find common true variables among zero-score selections
    val trueVarsPerSelection = zeroSelections.map { sel =>
      encodings.flatMap { case (rel, boolVars) =>
        val selectedPreds = sel.getPredicates(rel)
        val preds = predicates(rel).toList
        boolVars.zip(preds).collect {
          case (v, p) if selectedPreds.contains(p) => v
        }
      }.toSet
    }

    val commonTrueVars =
      if (trueVarsPerSelection.nonEmpty) trueVarsPerSelection.reduce(_ intersect _)
      else Set.empty[BoolExpr]

    // read the common predicates here
    val commonPredicates: Set[Predicate] = commonTrueVars.flatMap { v =>
      encodings.collectFirst {
        case (rel, boolVars) if boolVars.contains(v) =>
          val idx = boolVars.indexOf(v)
          predicates(rel).toList.lift(idx)
      }.flatten
    }

    println(s"Blocking the common predicate ${commonPredicates} in next iteration.")

    // Blocking clause: at least one of these must be false
    val block: BoolExpr =
      if (commonTrueVars.nonEmpty) z3ctx.mkOr(commonTrueVars.map(z3ctx.mkNot).toSeq: _*)
      else z3ctx.mkFalse()

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