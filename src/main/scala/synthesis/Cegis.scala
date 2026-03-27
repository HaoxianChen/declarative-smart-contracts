package synthesis

import datalog.{AnyType, BooleanType, CompoundType, NumberType, Program, Relation, ReservedRelation, Rule, SimpleRelation, SingletonRelation, SymbolType, UnitType}
import imp.SolidityStatement
import imp.{ImperativeTranslator, Inliner}
import imp.SolidityTranslator.transactionRelationPrefix
import synthesis.InductiveSynthesis.RelationSolveStatus

case class SynthesisStat(
  synthesisTimeMs: Long,
  cegisIterations: Int,
  bmcTimeMs: Long,
  bmcBound: Int
)

case class Cegis(sketch: Program,
                 udfSolPath: String = "",
                 forbiddenSpec: ForbiddenSpec = ForbiddenSpec.empty,
                 verboseSink: VerboseLogSink = VerboseLogSink.NoOp,
                 benchmarkName: String = "") {
  private val txDefs: Map[String, SolidityStatement] = extractTransactionDefinition(sketch)
  val interpreter = SolidityInterpreter()
  // val disambiguationTraces: Set[EvaluatedTrace] = makeDisambiguationTraces(sketch, interpreter,
  //   numTraces = 1000, txsPerTrace = 5)
//   val disambiguationTraces: Set[EvaluatedTrace] = makeDisambiguationTracesHeuristic(sketch, interpreter,
//     numTraces = 1000, txsPerTrace = 5)

  val disambiguationTraces: Set[EvaluatedTrace] = {
    val disambiguator = Disambiguator(sketch, interpreter, txDefs)
    disambiguator.makeTracesHeuristic(500)
  }

  private def vlog(msg: String): Unit = {
    val scoped = if (benchmarkName.nonEmpty) s"[${benchmarkName}] $msg" else msg
    verboseSink.log(scoped)
  }

  /**  This is a composed object that :
   *
   *  - First use the sketch to run against the BMC to get a counterexample Trace
   *  - Run the sketch over the trace using SolidityInterpreter to get an EvaluatedTrace
   *  - Run the inductive synthesizer that generate new program that blocks such EvaluatedTrace
   *  - iterate until no counter example is found by the BMC.
  *  */
  def run(maxBound: Int = 5, maxIters: Int = 120, maxSolutionsPerStep: Int = 20): (Program, SynthesisStat) = {
    val startTime = System.currentTimeMillis()
    var bmcTime: Long = 0
    var program: Program = sketch
    var traces: List[EvaluatedTrace] = List()

    // Build predicate candidates and the interpreter context for synthesis
    val interpreterContext = InterpreterContext.makeContext(program)
    val enumerator = PredicateEnumerator(interpreterContext)
    val restrictedCandidates = enumerator.enumeratePredicatesRestricted(program, fallbackToAllIfEmpty = false)
    val fullCandidates = enumerator.enumeratePredicatesFull(program)
    val txRuleByRelation: Map[Relation, Rule] = restrictedCandidates.keys.map { rule =>
      PredicateEnumerator.extractTxLiteral(rule).relation -> rule
    }.toMap

    def promoteCandidateRules(base: Map[Rule, Set[Predicate]],
                              full: Map[Rule, Set[Predicate]],
                              promoted: Set[Rule]): Map[Rule, Set[Predicate]] = {
      base.map { case (rule, preds) =>
        val effectivePreds =
          if (promoted.contains(rule)) full.getOrElse(rule, preds)
          else preds
        rule -> effectivePreds
      }
    }

    var promotedRules: Set[Rule] = Set.empty

    var rawCandidates = promoteCandidateRules(restrictedCandidates, fullCandidates, promotedRules)
    var candidates = forbiddenSpec.filterCandidateMap(rawCandidates)
    val rawPredicatesFromTxRules = enumerator.extractPredicateFromTxProperties(program)
    val predicatesFromTxRules = forbiddenSpec.filterSeededPredicateMap(rawPredicatesFromTxRules)
    val seededPredicatesByRelation: Map[Relation, Set[Predicate]] = predicatesFromTxRules.map {
      case (rule, preds) => PredicateEnumerator.extractTxLiteral(rule).relation -> preds
    }
    var synthesizer = InductiveSynthesis(candidates, interpreterContext, forbiddenSpec, seededPredicatesByRelation)

    val augmented = synthesizer.augmentSketchWithPredicates(program, predicatesFromTxRules)
    println(predicatesFromTxRules)
    vlog(s"BenchmarkStart: program=${sketch.name}, maxBound=$maxBound, maxIters=$maxIters, maxSolutionsPerStep=$maxSolutionsPerStep")
    vlog(s"SeededPredicatesFromTxProperties: ${predicatesFromTxRules}")
    vlog(s"InitialTransactionRules:\n${augmented.transactionRules().mkString("\n")}")
    program = augmented

    var iter = 0
    var finished = false
    var reason = ""

    while (iter < maxIters && !finished) {
      val bmcStart = System.currentTimeMillis()
      val bmc = BoundedModelChecker(udfSolPath = udfSolPath, verboseSink = verboseSink)
      println(s"[CEGIS] Iteration: $iter (BMC bound = $maxBound)")
      vlog(s"Iteration#$iter ProgramBeforeBMC:\n${program.transactionRules().mkString("\n")}")
      vlog(s"Iteration#$iter BMCInput: bound=$maxBound, violationRules=${program.violationRules.toSeq.map(_.head.relation.name).sorted.mkString(",")}")

      val (sat, optTrace) = bmc.check(program, program.violationRules, maxBound)
      bmcTime += (System.currentTimeMillis() - bmcStart)
      if (sat) {
        println("[CEGIS] No counterexample found by BMC. Finished.")
        vlog(s"Iteration#$iter BMCResult: no-counterexample")
        finished = true
        reason = "sat"
      } else {
        optTrace match {
          case None =>
            println("[CEGIS] BMC reported violation but did not return a trace. Aborting.")
            vlog(s"Iteration#$iter BMCResult: violation-without-trace")
            finished = true
            reason = "abort"
          case Some(trace) =>
            println(s"[CEGIS] Counterexample trace found: $trace")
            vlog(s"Iteration#$iter Counterexample:\n$trace")
            val evaluatedTrace = interpreter.interpret(txDefs, trace)
            val blockedRelation = evaluatedTrace.steps.last._1.relation

            traces :+= evaluatedTrace
            println("[CEGIS] Running inductive synthesis to block the counterexample...")
            val restrictedRun = synthesizer.synthesizeWithStatus(augmented, traces, maxSolutionsPerStep, disambiguationTraces)
            val blockedStatus = restrictedRun.relationStatuses.get(blockedRelation)
            blockedStatus.foreach(status =>
              println(s"[CEGIS] Restricted solver status for relation ${blockedRelation.name}: ${status.label}")
            )
            blockedStatus.foreach(status => vlog(s"Iteration#$iter RestrictedSolveStatus: relation=${blockedRelation.name}, status=${status.label}"))

            val shouldFallbackForStatus = blockedStatus.exists {
              case RelationSolveStatus.Unsat | RelationSolveStatus.Unknown => true
              case RelationSolveStatus.SatFound => false
            }
            val fallbackRules = txRuleByRelation.get(blockedRelation).filterNot(promotedRules.contains).filter(_ => shouldFallbackForStatus).toSet

            val effectiveProgram =
              if (fallbackRules.nonEmpty) {
                val promotedNames = fallbackRules.toList.map(_.head.relation.name).sorted.mkString(", ")
                val statusLabel = blockedStatus.map(_.label).getOrElse("UNKNOWN")
                println(s"[CEGIS] Restricted status=$statusLabel. Falling back to full predicate space for: $promotedNames")
                vlog(s"Iteration#$iter FallbackToFull: reason=$statusLabel, relations=$promotedNames")
                promotedRules ++= fallbackRules
                rawCandidates = promoteCandidateRules(restrictedCandidates, fullCandidates, promotedRules)
                candidates = forbiddenSpec.filterCandidateMap(rawCandidates)
                synthesizer = InductiveSynthesis(candidates, interpreterContext, forbiddenSpec, seededPredicatesByRelation)
                val retryRun = synthesizer.synthesizeWithStatus(augmented, traces, maxSolutionsPerStep, disambiguationTraces)
                vlog(s"Iteration#$iter ProgramAfterFallbackSynthesis:\n${retryRun.program.transactionRules().mkString("\n")}")
                retryRun.program
              } else {
                vlog(s"Iteration#$iter ProgramAfterRestrictedSynthesis:\n${restrictedRun.program.transactionRules().mkString("\n")}")
                restrictedRun.program
              }

            if (effectiveProgram == program) {
              if (fallbackRules.nonEmpty) {
                println("[CEGIS] Synthesizer produced no change after status-driven full-space fallback. Stopping.")
                vlog(s"Iteration#$iter SynthesisResult: nochange-after-fallback")
              } else {
                println("[CEGIS] Synthesizer produced no change. Stopping.")
                vlog(s"Iteration#$iter SynthesisResult: nochange")
              }
              finished = true
              reason = "nochange"
            } else {
              if (fallbackRules.nonEmpty) {
                println("[CEGIS] Program updated by status-driven full-space fallback. Continuing next iteration.")
                vlog(s"Iteration#$iter SynthesisResult: updated-by-fallback")
              } else {
                println("[CEGIS] Program updated by synthesizer. Continuing next iteration.")
                vlog(s"Iteration#$iter SynthesisResult: updated")
              }
              program = effectiveProgram
              iter += 1
            }
        }
      }
    }
    if (iter >= maxIters) {
      println(s"[CEGIS] Reached maximum iterations ($maxIters). Returning current program.")
      reason = "maxiters"
    }
    val totalTime = System.currentTimeMillis() - startTime
    vlog(s"BenchmarkEnd: reason=$reason, totalTimeMs=$totalTime, bmcTimeMs=$bmcTime, cegisIterations=$iter, finalRules=\n${program.transactionRules().mkString("\n")}")
    (program, SynthesisStat(totalTime, iter, bmcTime, maxBound))
  }

  private def extractTransactionDefinition(program: Program): Map[String, SolidityStatement] = {
    // Follow the same pipeline used in `Main.run`:
    // 1) Translate datalog Program -> ImperativeAbstractProgram
    // 2) Translate ImperativeAbstractProgram -> Solidity AST
    // 3) Inline functions using Inliner
    // 4) Collect DeclFunction definitions and map interface relations to the matching function

    val materializedRelations: Set[datalog.Relation] = Set()

    // Step 1: Imperative translation
    val impTranslator = new ImperativeTranslator(program, materializedRelations, isInstrument = false,
      monitorViolations = false, arithmeticOptimization = true, enableProjection = true)
    val imperative = impTranslator.translate()

    // Step 2: Solidity translation
    val solidityAst = imp.SolidityTranslator(imperative, program.interfaces, program.violations,
      materializedRelations, isInstrument = false, monitorViolation = false, enableProjection = true).translate()

    // Step 3: Inlining
    // Inliner expects `Set[Relation]` for interfaces (it only needs the relation names);
    // extract relations from Interface objects.
    val interfaceRelations: Set[datalog.Relation] = program.interfaces.map(_.relation)
    val inliner = Inliner(solidityAst, interfaceRelations)
    val inlinedSolidity = inliner.run()

    // Step 4: map interfaces to function bodies
    val funcDefs: Map[String, imp.DeclFunction] = inliner.collectFunctionDefs(inlinedSolidity)

    val mappings = scala.collection.mutable.Map.empty[String, SolidityStatement]

    for (iface <- program.interfaces) {
      val funcName = if (iface.relation.name.startsWith(transactionRelationPrefix)) {
        iface.relation.name.stripPrefix(transactionRelationPrefix)
      } else {
        s"get${iface.relation.name.capitalize}"
      }
      funcDefs.get(funcName) match {
        case Some(df) => mappings += (funcName -> df)
        case None => println(s"[CEGIS] Warning: function definition for interface ${iface.relation.name} (expected '$funcName') not found")
      }
    }

    val constructorKey = "constructor"
    mappings += (constructorKey->funcDefs(constructorKey))

    mappings.toMap
  }

  private def makeDisambiguationTracesHeuristic(sketch: Program, solInterpreter: SolidityInterpreter,
                                       numTraces: Int = 20, txsPerTrace: Int = 3): Set[EvaluatedTrace] = {
    import scala.util.Random

    val interfaceRelations = sketch.interfaces.map(_.relation).
      filter(_.name.startsWith(transactionRelationPrefix)).toList
    val txRelations = interfaceRelations.map {
      case sr: SimpleRelation => sr.copy(name=sr.name.stripPrefix(transactionRelationPrefix))
      case SingletonRelation(name, sig, memberNames) => ???
      case relation: ReservedRelation => ???
    }

    // small address universe as strings
    val addresses = List("1", "2", "3")

    // helper to build a random Transaction for a given relation
    def randomTransaction(rel: datalog.Relation): Transaction = {
      val params = rel.sig.zipWithIndex.map { case (t, idx) =>
        t match {
          case SymbolType(_) =>
            // pick an address from the small universe
            datalog.Constant(t, addresses(Random.nextInt(addresses.length)))
          case _:NumberType =>
            // small numeric values
            datalog.Constant(t, (Random.nextInt(10) + 1).toString)
          case _ =>
            // fallback: small domain values as strings
            datalog.Constant(t, Random.nextInt(3).toString)
        }
      }
      // also make Implicit parameters here.
      Transaction(rel, params, ImplicitParameters())
    }

    // For each trace: add per-address setup (mint-like) transactions when possible,
    // then append random transactions up to txsPerTrace.
    val traces: Set[Trace] = (1 to numTraces).map { _ =>
      val setupTxs: List[Transaction] = txRelations.flatMap { rel =>
        // Heuristic: if relation name contains "mint" (case-insensitive), produce per-address setup
        if (rel.name.toLowerCase.contains("mint")) {
          addresses.map { addr =>
            val params = rel.sig.zipWithIndex.map { case (t, idx) =>
              t match {
                case SymbolType(_) =>
                  // first param assumed to be address
                  datalog.Constant(t, addr)
                case _:NumberType  =>
                  datalog.Constant(t, (Random.nextInt(10) + 1).toString)
                case _ =>
                  datalog.Constant(t, Random.nextInt(3).toString)
              }
            }
            Transaction(rel, params, ImplicitParameters())
          }
        } else Nil
      }

      // fill remaining transactions with random transactions
      val remaining = math.max(0, txsPerTrace - setupTxs.length)
      val randomTxs = (1 to remaining).map { _ =>
        val rel = txRelations(Random.nextInt(txRelations.length))
        randomTransaction(rel)
      }

      Trace((setupTxs ++ randomTxs).toList)
    }.toSet

    // interpret traces into EvaluatedTrace using the solidity interpreter
    traces.map(t => solInterpreter.interpret(txDefs, t))
  }
  // private def makeDisambiguationTracesHeuristic(sketch: Program, solInterpreter: SolidityInterpreter,
  //                                      numTraces: Int = 20, txsPerTrace: Int = 3): Set[EvaluatedTrace] = {
  //   /** 1. make a small address universe, say 1,2,3. */

  //   /** 2. set up the state by having mint(p,n) where p each adress in the universe,
  //    *   n is random number between 1-10.
  //    *  */

  //   /** 3. Randomly generate rest of [txPerTrace], enumerating each parameter combination for each
  //    *   transaction, each parameter in 1,2,3.
  //    *   */
  //  ???
  //

  // }

  private def makeDisambiguationTraces(sketch: Program, solInterpreter: SolidityInterpreter,
                                       numTraces: Int = 20, txsPerTrace: Int = 3): Set[EvaluatedTrace] = {
    import scala.util.Random
    val interfaceRelations = sketch.interfaces.map(_.relation).
      filter(_.name.startsWith(transactionRelationPrefix)).toList
    val txRelations = interfaceRelations.map {
      case sr: SimpleRelation => sr.copy(name=sr.name.stripPrefix(transactionRelationPrefix))
      case SingletonRelation(name, sig, memberNames) => ???
      case relation: ReservedRelation => ???
    }

    // Helper to generate a random transaction for a relation
    def randomTransaction(rel: datalog.Relation): Transaction = {
      val params = rel.sig.zipWithIndex.map { case (t, i) =>
        // Use random integer as string for each parameter
        val bound = t match {
          case SymbolType(_) => 3
          case _ => 3
        }
        datalog.Constant(t, Random.nextInt(bound).toString)
      }
      Transaction(rel, params, ImplicitParameters())
    }

    // Generate random traces
    val randomTraces: Set[Trace] = (1 to numTraces).map { _ =>
      val txs = (1 to txsPerTrace).map { _ =>
        val rel = txRelations(Random.nextInt(interfaceRelations.size))
        randomTransaction(rel)
      }
      Trace(txs.toList)
    }.toSet

    // Convert traces to EvaluatedTrace
    randomTraces.map(t => solInterpreter.interpret(txDefs, t))
  }
}