package synthesis

import datalog.{Program, Relation}
import imp.{SolidityStatement}
import imp.{ImperativeTranslator, Inliner}
import imp.SolidityTranslator.transactionRelationPrefix

case class Cegis(sketch: Program) {

  private val txDefs: Map[String, SolidityStatement] = extractTransactionDefinition(sketch)

  /**  This is a composed object that :
   *
   *  - First use the sketch to run against the BMC to get a counterexample Trace
   *  - Run the sketch over the trace using SolidityInterpreter to get an EvaluatedTrace
   *  - Run the inductive synthesizer that generate new program that blocks such EvaluatedTrace
   *  - iterate until no counter example is found by the BMC.
  *  */
  def run(maxBound: Int = 10, maxIters: Int = 10): Program = {
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
    val interpreter = SolidityInterpreter()

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

      // Build predicate candidates and the interpreter context for synthesis
      val interpreterContext = InterpreterContext.makeContext(program)
      val enumerator = PredicateEnumerator(interpreterContext)
      val candidates = enumerator.enumeratePredicates(program)

      val synthesizer = InductiveSynthesis(candidates, interpreterContext)

      traces += evaluatedTrace
      println("[CEGIS] Running inductive synthesis to block the counterexample...")
      // todo: need to extend synthesize to take multiple traces
      ???
      val newProgram = synthesizer.synthesize(program, evaluatedTrace)

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
}