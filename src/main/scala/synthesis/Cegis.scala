package synthesis

import datalog.{AnyType, BooleanType, CompoundType, NumberType, Program, Relation, ReservedRelation, SimpleRelation, SingletonRelation, SymbolType, UnitType}
import imp.SolidityStatement
import imp.{ImperativeTranslator, Inliner}
import imp.SolidityTranslator.transactionRelationPrefix

case class Cegis(sketch: Program) {

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

  /**  This is a composed object that :
   *
   *  - First use the sketch to run against the BMC to get a counterexample Trace
   *  - Run the sketch over the trace using SolidityInterpreter to get an EvaluatedTrace
   *  - Run the inductive synthesizer that generate new program that blocks such EvaluatedTrace
   *  - iterate until no counter example is found by the BMC.
  *  */
  def run(maxBound: Int = 4, maxIters: Int = 50, maxSolutionsPerStep: Int = 20): Program = {
    // Assumptions made:
    // 1) We try to use the SolidityInterpreter whenever possible. We construct a
    //    minimal `ReadValueFromMap` statement that performs a read using constant
    //    keys so the interpreter's implementation path executes without requiring
    //    a full program-to-solidity translation.
    // 2) If no suitable SimpleRelation exists in the program, we fall back to the
    //    conservative conversion (pair transactions with empty State snapshots).
    // 3) The synthesis loop is bounded by `maxIters` and uses the provided
    //    `maxBound` when invoking the BoundedModelChecker.

    var program: Program = sketch
    var traces: Set[EvaluatedTrace] = Set()
    val bmc = BoundedModelChecker()

    // Build predicate candidates and the interpreter context for synthesis
    val interpreterContext = InterpreterContext.makeContext(program)
    val enumerator = PredicateEnumerator(interpreterContext)
    val candidates = enumerator.enumeratePredicates(program)

    val synthesizer = InductiveSynthesis(candidates, interpreterContext)

    var iter = 0
    while (iter < maxIters) {
      println(s"[CEGIS] Iteration: $iter (BMC bound = $maxBound)")

      val (sat, optTrace) = bmc.check(program, program.violationRules, maxBound)
      if (sat) {
        println("[CEGIS] No counterexample found by BMC. Finished.")
        return program
      }

      val trace = optTrace.getOrElse {
        println("[CEGIS] BMC reported violation but did not return a trace. Aborting.")
        return program
      }

      println(s"[CEGIS] Counterexample trace found: $trace")

      val evaluatedTrace = interpreter.interpret(txDefs, trace)

      traces += evaluatedTrace
      println("[CEGIS] Running inductive synthesis to block the counterexample...")
      val newProgram = synthesizer.synthesize(sketch, traces, maxSolutionsPerStep, disambiguationTraces)

      if (newProgram == program) {
        println("[CEGIS] Synthesizer produced no change. Stopping.")
        return program
      }

      println("[CEGIS] Program updated by synthesizer. Continuing next iteration.")
      program = newProgram
      iter += 1
    }

    println(s"[CEGIS] Reached maximum iterations ($maxIters). Returning current program.")
    program
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