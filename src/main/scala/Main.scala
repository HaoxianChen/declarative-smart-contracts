import datalog.{Parser, Program, Relation, TypeChecker}
import imp.{ImperativeTranslator, ImperativeTranslatorWithUpdateFusion, Inliner, SolidityTranslator, Translator}
import synthesis.{BoundedModelChecker, Cegis, EvaluatedTrace, InductiveSynthesis, Interpreter, Predicate}
import util.Misc
import util.Misc.{createDirectory, fileToString, isFileExists, parseProgram, readMaterializedRelationNames, combineSplitFilesToFile, parseProgramFromSplitDir, parseAllProgramsFromSplitParent}
import verification.{Prove, TransitionSystem, Verifier}
import java.nio.file.Paths
import scala.sys.exit

object Main extends App {
  val outDir = "solidity/dsc"
  val outDirWithInstrumentations = "solidity/dsc-instrument"
  val benchmarkDir = "benchmarks"
  val allBenchmarks = List(
    "crowFunding.dl",
    "erc20.dl",
    "nft.dl",
    "wallet.dl",
    "vestingWallet.dl",
    "paymentSplitter.dl",
    "erc777.dl",
    "erc1155.dl",
    "controllable.dl",
    "tokenPartition.dl",
    "tether.dl",
    "bnb.dl",
    "matic.dl",
    "ltcSwapAsset.dl",
    "theta.dl",
    "wbtc.dl",
    "shib.dl",
    "linktoken.dl",
    "voting.dl",
    "brickBlockToken.dl",
    "auction.dl"
    )

    val invariantGenerationBenchmarks = List(
      "crowFunding.dl",
      "voting.dl",
      "brickBlockToken.dl",
      "auction.dl"
    )

  // List of split-directory names (subdirectories of `synthesis-benchmark`) to run in split-mode.
  // If empty -> run on all subdirectories (default). Modify this list to control which directories run.
  val synthesisSplitDirs: List[String] = List(
    "wallet",
    "erc20",
    "matic",
    "controllable",
    "cappedCrowdSale",
    "bnb",
    "crowFunding",
    "tether",
    "brickBlockToken",
    "shib",
    "tokenPartition",
    "wbtc",
    "linktoken",
    "finalizableCrowdSale",
    "ltcSwapAsset",
  )

  def getMaterializedRelations(dl: Program, filepath: String): Set[Relation] = {
    if (isFileExists(filepath)) {
      val materializedRelationNames: Set[String] = {
        val allPlans = readMaterializedRelationNames(filepath)
        allPlans.minBy(_.length).toSet
      }
      val ret = materializedRelationNames.flatMap(n=>dl.relations.filter(_.name==n))
      require(ret.size == materializedRelationNames.size)
      ret
    }
    else {
      Set()
    }
  }

  def run(filepath: String, displayResult: Boolean, outDir: String, isInstrument: Boolean, monitorViolations: Boolean,
          consolidateUpdates: Boolean, materializePath: String = s"", enableProjection:Boolean,
          arithmeticOptimization: Boolean = true): Unit = {
    createDirectory(outDir)
    val filename = Misc.getFileNameFromPath(filepath)
    val dl = parseProgram(filepath)
    val materializedRelations: Set[Relation] = if (materializePath.nonEmpty) {
      getMaterializedRelations(dl, materializePath)
    }
    else {
      Set()
    }
    val impTranslator: ImperativeTranslator = if (consolidateUpdates) {
      ImperativeTranslatorWithUpdateFusion(dl, materializedRelations, isInstrument, monitorViolations,
        arithmeticOptimization=arithmeticOptimization, enableProjection=enableProjection)
    }
    else {
      new ImperativeTranslator(dl, materializedRelations, isInstrument, monitorViolations,
        arithmeticOptimization=arithmeticOptimization, enableProjection=enableProjection)
    }
    val imperative = impTranslator.translate()
    val solidity = SolidityTranslator(imperative, dl.interfaces,dl.violations,materializedRelations,
      isInstrument,monitorViolations, enableProjection).translate()
    val outfile = Paths.get(outDir, s"$filename.sol")
    Misc.writeToFile(solidity.toString, outfile.toString)
    if (displayResult) {
      println(dl)
      println(imperative)
      println(s"Solidity program:\n${solidity}")
    }
    println(s"${impTranslator.ruleSize} rules.")
  }

  val compileUsage: String = s"Usage: compile [--arg n] file-path\n" +
    s"--materialize <filename> materialize the set of relations specified in file\n" +
    s"--fuse turn on the option to consolidate updates into one function\n" +
    s"--no-arithmetic-optimization turn off arithmetic optimization\n" +
    s"--no-projection turn off projection optimization\n" +
    s"--out <directory> output directory\n"

  def nextArg(map: Map[String, Any], list: List[String]): Map[String, Any] = list match {
    case Nil => map
    case string :: Nil => nextArg(map ++ Map("filepath"->string), list.tail)
    case "--materialize" :: value :: tail => nextArg(map++ Map("materialize"->value), tail)
    case "--fuse" :: tail => nextArg(map ++ Map("fuse"->true), tail)
    case "--no-arithmetic-optimization" :: tail => nextArg(map++Map("arithmetic-optimization"->false), tail)
    case "--no-projection" :: tail => nextArg(map++Map("projection"->false), tail)
    case "--instrument" :: tail => nextArg(map++Map("instrument"->true), tail)
    case "--monitor" :: tail => nextArg(map++Map("monitor"->true), tail)
    case "--out" :: value :: tail => nextArg(map++ Map("out"->value), tail)
    case unknown :: _ =>
      println(s"Unknown option: $unknown")
      exit(1)
  }

  def runVerification(p: String): Unit = {
    println(p)
    val filepath = Paths.get(benchmarkDir, p).toString
    val dl = parseProgram(filepath)
    val materializedRelations: Set[Relation] = Set()
    val impTranslator = new ImperativeTranslator(dl, materializedRelations, isInstrument=true,
      enableProjection = true, monitorViolations = false, arithmeticOptimization = true)
    val imperative = impTranslator.translate()
    val verifier = new Verifier(dl, imperative)
    verifier.check()
  }

  if (args(0) == "compile") {
    val options: Map[String, Any] = if (args.length <= 1) {
      println(compileUsage)
      exit(1)
    }
    else {
      nextArg(Map(), args.tail.toList)
    }
    // val filepath = args(1)
    // val isInstrument = args(2).toBoolean
    // val _outDir = if(isInstrument) outDirWithInstrumentations else outDir
    val filepath = options("filepath").toString
    run(filepath, displayResult = true, outDir=options("out").toString,
      isInstrument = options.getOrElse("instrument",false).toString.toBoolean,
      monitorViolations = options.getOrElse("monitor",false).toString.toBoolean,
      consolidateUpdates = options.getOrElse("fuse",false).toString.toBoolean,
      materializePath = options.getOrElse("materialize","").toString,
      arithmeticOptimization = options.getOrElse("arithmetic-optimization",true).toString.toBoolean,
      enableProjection = options.getOrElse("projection", true).toString.toBoolean)
  }
  else if (args(0) == "compile-all") {
    val options: Map[String, Any] = if (args.length <= 1) {
      println(compileUsage)
      exit(1)
    }
    else {
      nextArg(Map(), args.tail.toList)
    }
    for (p <- allBenchmarks) {
      println(p)
      val filepath = Paths.get(benchmarkDir, p).toString
      run(filepath, displayResult = false, outDir=options("out").toString,
        isInstrument = options.getOrElse("instrument",false).toString.toBoolean,
        monitorViolations = options.getOrElse("monitor",false).toString.toBoolean,
        consolidateUpdates = options.getOrElse("fuse",false).toString.toBoolean,
        materializePath = options.getOrElse("materialize","").toString,
        arithmeticOptimization = options.getOrElse("arithmetic-optimization",true).toString.toBoolean,
        enableProjection = options.getOrElse("projection", true).toString.toBoolean)
    }
  }
  else if (args(0) == "test") {
    for (p <- allBenchmarks) {
      println(p)
      val filepath = Paths.get(benchmarkDir, p).toString
      run(filepath, displayResult = false, outDir=outDir, isInstrument = false, monitorViolations = false,
        consolidateUpdates = false, enableProjection = true)
      run(filepath, displayResult = false, outDir="solidity/fuse", isInstrument = false, monitorViolations = false,
        consolidateUpdates = true, enableProjection = true)
    }
  }
  else if (args(0) == "compile-all-versions") {
    val filepath = args(1)

    /** 1. The basic compilation. */
    run(filepath, displayResult = false, outDir=outDir, isInstrument = false, monitorViolations = false,
      consolidateUpdates = false, enableProjection = true)

    /** 2. Fuse update operations into one function. */
    val _fusedOutDir = "solidity/fuse"
    run(filepath, displayResult = false, outDir=_fusedOutDir, isInstrument = false, monitorViolations = false,
      consolidateUpdates = true, enableProjection = true)
  }
  else if (args(0) == "test-instrument") {
    val _outDir = outDirWithInstrumentations
    for (p <- allBenchmarks) {
      println(p)
      val filepath = Paths.get(benchmarkDir, p).toString
      run(filepath, displayResult = false, outDir=_outDir, isInstrument = true, monitorViolations = true,
        consolidateUpdates = true, enableProjection = true)
    }
  }

  else if (args(0) == "verify") {
    val filepath = args(1)

    val dl = parseProgram(filepath)
    val materializedRelations: Set[Relation] = Set()
    val impTranslator = new ImperativeTranslator(dl, materializedRelations, isInstrument=true, enableProjection=true,
      monitorViolations = false, arithmeticOptimization = true)
    val imperative = impTranslator.translate()
    // println(imperative)
    val verifier = new Verifier(dl, imperative)
    verifier.check()

  }

  else if (args(0) == "test-verification") {
    for (p <- allBenchmarks) {
      runVerification(p)
    }
  }

  else if (args(0) == "synthesis") {
    /** Input:
     *    - Datalog: a smart contract in Datalog, without transaction validation rules
     *    - Temporal properties
     *
     *  Output:
     *    - Fill in the transaction validation rules for the input Datalog file,
     *      such that it is consistent with the temporal properties.
     *  */
    val datalog_filepath = args(1)
    val program = parseProgram(datalog_filepath)
    val interpreterContext = synthesis.InterpreterContext.makeContext(program)
    val enumerator = synthesis.PredicateEnumerator(interpreterContext)
    val candidates = enumerator.enumeratePredicates(program)
    println(s"[synthesis] program: ${program.name}")
    println(s"[synthesis] candidate predicates: ${candidates.size}")

    /** Synthesize by adding validation condition */
    val synthesizer = InductiveSynthesis(candidates, interpreterContext)
    val testTrace = EvaluatedTrace.testTrace1(program)
    val synthesisOutput = synthesizer.synthesize(program, List(testTrace),
      maxSolutions = 1, disambiguationTraces = Set())
    // println(synthesisOutput)
  }

  else if (args(0) == "cegis") {
    val synthesisBenchmarks: List[String] = List(
      // "wallet.dl",
      // "erc20.dl",
      // "matic.dl",
      // "controllable.dl",
      "cappedCrowdSale.dl",
      // "bnb.dl",
      // "crowFunding.dl",
      // "tether.dl",
      // "brickBlockToken.dl",
      // "shib.dl",
      // "tokenPartition.dl",
      // "wbtc.dl",
      // "linktoken.dl",
      // "finalizableCrowdSale.dl",
      // "ltcSwapAsset.dl",
      //////////////////////
      // "voting.dl"
      // "auction.dl"
    )
    val test = true
    val synthesisBenchmarkDir = "synthesis-benchmark"
    val datalogOutDir = "synthesis-output"
    val statsFile = Paths.get(datalogOutDir, "synthesis_stats.csv").toString
    createDirectory(datalogOutDir)
    if (!isFileExists(statsFile)) {
      Misc.writeToFile("benchmark,relations,interfaces,rules_minus_interface_and_violation,violation_rules,synthesis_time_s,bmc_time_s,cegis_iterations,bmc_bound\n", statsFile) // Updated CSV header and stat order
    }
    for (p <- synthesisBenchmarks) {
        println(s"$p")
        val filenameNoExt = p.stripSuffix(".dl")
        val datalogOutfile = Paths.get(datalogOutDir, s"${filenameNoExt}.dl").toString
        if (!isFileExists(datalogOutfile) || test) {
          val datalog_filepath = Paths.get(synthesisBenchmarkDir, p).toString
          val sketch = parseProgram(datalog_filepath)

          val interfaceCount = sketch.interfaces.size
          val violationRules = sketch.violationRules.size
          val relationCount = sketch.relations.size - interfaceCount - sketch.violations.size
          val rulesMinusInterfaceAndViolation = sketch.rules.size - interfaceCount - violationRules

          val cegis = Cegis(sketch)
          val (program, stat) = cegis.run() // Capture both result and stats

          println(s"Synthesis output:\n${program}")

          createDirectory(datalogOutDir)
          Misc.writeToFile(program.toString, datalogOutfile)

          // Write associated Solidity file to disk
          val impTranslator = new ImperativeTranslator(
            program, Set(), isInstrument = false, monitorViolations = false, arithmeticOptimization = true,
            enableProjection = true
          )
          val imperative = impTranslator.translate()
          val solidity = SolidityTranslator(imperative, program.interfaces, program.violations,
            Set(), isInstrument = false, monitorViolation = false, enableProjection = true
          ).translate()
          val solidityOutfile = Paths.get(datalogOutDir, s"${filenameNoExt}.sol").toString
          if (!test) Misc.writeToFile(solidity.toString, solidityOutfile)

          // Record stats using SynthesisStat, convert ms to seconds
          val synthesisTimeS = stat.synthesisTimeMs / 1000.0
          val bmcTimeS = stat.bmcTimeMs / 1000.0
          val statsLine = s"$p,$relationCount,$interfaceCount,$rulesMinusInterfaceAndViolation,$violationRules,$synthesisTimeS,$bmcTimeS,${stat.cegisIterations},${stat.bmcBound}\n"
          if (!test) Misc.appendToFile(statsLine, statsFile)
        } else {
          println(s"Output for $p exists, skipping.")
        }
    }
  }

  // New: run CEGIS over split-program directories (schema/rules/properties parsed in-memory)
  else if (args(0) == "cegis-split") {
    val test = true
    val synthesisBenchmarkDir = "synthesis-benchmark"
    val datalogOutDir = "synthesis-output"
    val statsFile = Paths.get(datalogOutDir, "synthesis_stats.csv").toString
    createDirectory(datalogOutDir)
    if (!isFileExists(statsFile)) {
      Misc.writeToFile("benchmark,relations,interfaces,rules_minus_interface_and_violation,violation_rules,synthesis_time_s,bmc_time_s,cegis_iterations,bmc_bound\n", statsFile)
    }

    // parse all split-program subdirectories under parent into (name, Program)
    val programsByName: Seq[(String, Program)] = if (synthesisSplitDirs.nonEmpty) {
      // Build (name, Program) for each requested split-dir (skip missing)
      synthesisSplitDirs.flatMap { name =>
        val dir = Paths.get(synthesisBenchmarkDir, name).toString
        val f = new java.io.File(dir)
        if (f.exists() && f.isDirectory) {
          Some((name, parseProgramFromSplitDir(dir)))
        } else {
          println(s"Skipping missing split-dir: ${dir}")
          None
        }
      }
    } else {
      parseAllProgramsFromSplitParent(synthesisBenchmarkDir)
    }

    for ((name, sketch) <- programsByName) {
      val displayName = if (name.endsWith(".dl")) name else s"${name}.dl"
      println(displayName)
      val filenameNoExt = if (name.endsWith(".dl")) name.stripSuffix(".dl") else name
      val datalogOutfile = Paths.get(datalogOutDir, s"${filenameNoExt}.dl").toString
      if (!isFileExists(datalogOutfile) || test) {
        val interfaceCount = sketch.interfaces.size
        val violationRules = sketch.violationRules.size
        val relationCount = sketch.relations.size - interfaceCount - sketch.violations.size
        val rulesMinusInterfaceAndViolation = sketch.rules.size - interfaceCount - violationRules

        val cegis = Cegis(sketch)
        val (program, stat) = cegis.run()

        println(s"Synthesis output:\n${program}")

        createDirectory(datalogOutDir)
        Misc.writeToFile(program.toString, datalogOutfile)

        val impTranslator = new ImperativeTranslator(
          program, Set(), isInstrument = false, monitorViolations = false, arithmeticOptimization = true,
          enableProjection = true
        )
        val imperative = impTranslator.translate()
        val solidity = SolidityTranslator(imperative, program.interfaces, program.violations,
          Set(), isInstrument = false, monitorViolation = false, enableProjection = true
        ).translate()
        val solidityOutfile = Paths.get(datalogOutDir, s"${filenameNoExt}.sol").toString
        if (!test) Misc.writeToFile(solidity.toString, solidityOutfile)

        val synthesisTimeS = stat.synthesisTimeMs / 1000.0
        val bmcTimeS = stat.bmcTimeMs / 1000.0
        val statsLine = s"${displayName},${relationCount},${interfaceCount},${rulesMinusInterfaceAndViolation},${violationRules},${synthesisTimeS},${bmcTimeS},${stat.cegisIterations},${stat.bmcBound}\n"
        if (!test) Misc.appendToFile(statsLine, statsFile)
      } else {
        println(s"Output for ${displayName} exists, skipping.")
      }
    }
  }

  // New: run synthesis-all over split-program directories
  else if (args(0) == "synthesis-all-split") {
    val synthesisBenchmarkDir = "synthesis-benchmark"
    val allParsed: Seq[(String, Program)] = parseAllProgramsFromSplitParent(synthesisBenchmarkDir)
    val programsToRun: Seq[(String, Program)] = if (synthesisSplitDirs.nonEmpty) {
      synthesisSplitDirs.flatMap { name =>
        val dir = Paths.get(synthesisBenchmarkDir, name).toString
        val f = new java.io.File(dir)
        if (f.exists() && f.isDirectory) Some((name, parseProgramFromSplitDir(dir)))
        else { println(s"Skipping missing split-dir: ${dir}"); None }
      }
    } else allParsed

    for ((name, sketch) <- programsToRun) {
      println(s"Running synthesis on (split): ${name}")
      val cegis = Cegis(sketch)
      cegis.run()
    }
  }

  else if (args(0) == "test-interpreter") {
    val datalog_filepath = args(1)
    val program = parseProgram(datalog_filepath)
    val interpreterContext = synthesis.InterpreterContext.makeContext(program)
    val enumerator = synthesis.PredicateEnumerator(interpreterContext)
    val candidates = enumerator.enumeratePredicates(program)
    for ((rule, preds) <- candidates) {
      Interpreter.test2(interpreterContext, rule, preds)
    }
  }

  /** Test the bounded model checker. */
  else if (args(0) == "bmc") {
    val filepath = args(1)
    val bound = if (args.length > 2) args(2).toInt else 10 // default bound
    val dl = parseProgram(filepath)
    val bmc = BoundedModelChecker()
    bmc.check(dl, dl.violationRules, bound)
  }

  else if (args(0) == "dump-expression") {
    val filepath = args(1)
    val dl = parseProgram(filepath)
    val materializedRelations: Set[Relation] = Set()
    val impTranslator = new ImperativeTranslator(dl, materializedRelations, isInstrument=true, enableProjection=true,
      monitorViolations = false, arithmeticOptimization = true)
    val imperative = impTranslator.translate()
    // println(imperative)
    val verifier = new Verifier(dl, imperative)
    verifier.traverseExpression()
  }

  else if (args(0) == "test-invariant-generator") {
    for (p<-invariantGenerationBenchmarks) {
      runVerification(p)
    }
  }

  else if (args(0) == "dependency-graph") {
    for (p <- allBenchmarks) {
      val filepath = Paths.get(benchmarkDir, p).toString
      val dl = parseProgram(filepath)
      val materializedRelations: Set[Relation] = Set()
      val impTranslator = new ImperativeTranslator(dl, materializedRelations, isInstrument=true,
        enableProjection = true, monitorViolations = false, arithmeticOptimization = true)
      val relationDependencies = impTranslator.getRelationDependencies()
      // write to files
      val outfile = s"view-materialization/relation-dependencies/${dl.name}.csv"
      val preamble = s"#body,head,ruleId,isAgg,isTx\n"
      val sortedEdges = relationDependencies.toList.sortBy(_._3)
      val edgeStr = sortedEdges.map(t=>s"${t._1.name},${t._2.name},${t._3},${t._4},${t._5}")
      val outStr = preamble+edgeStr.mkString("\n")
      Misc.writeToFile(outStr, outfile)
    }
  }

  else if (args(0) == "testz3") {
    TransitionSystem.testTS()
    // Prove.testZ3()
    // Prove.testTuple()
  }

  else if (args(0) == "test-sol-interpreter") {
    // Read datalog file path from args(1)
    val filepath = args(1)
    // Parse the datalog program
    val dl = parseProgram(filepath)
    // No materialized relations for this test
    val materializedRelations: Set[Relation] = Set()
    // Translate to imperative
    val impTranslator = new ImperativeTranslator(
      dl,
      materializedRelations,
      isInstrument = false,
      monitorViolations = false,
      arithmeticOptimization = true,
      enableProjection = true
    )
    val imperative = impTranslator.translate()
    // Translate to Solidity
    val solidity = SolidityTranslator(
      imperative,
      dl.interfaces,
      dl.violations,
      materializedRelations,
      isInstrument = false,
      monitorViolation = false,
      enableProjection = true
    ).translate()
    // Print results
    println(dl)
    println(imperative)
    println(s"Solidity program:\n${solidity}")
    println(s"${impTranslator.ruleSize} rules.")

    val inliner = Inliner(solidity, dl.interfaces.map(_.relation))
    val inlinedSol = inliner.run()
    println(s"inlined Solidity:\n${inlinedSol}")
  }

  else if (args(0) == "test-inline") {
    for (p <- allBenchmarks) {
      println(p)
      // Read datalog file path from args(1)
      val filepath = Paths.get(benchmarkDir, p).toString

      // Parse the datalog program
      val dl = parseProgram(filepath)
      // No materialized relations for this test
      val materializedRelations: Set[Relation] = Set()
      // Translate to imperative
      val impTranslator = new ImperativeTranslator(
        dl,
        materializedRelations,
        isInstrument = false,
        monitorViolations = false,
        arithmeticOptimization = true,
        enableProjection = true
      )
      val imperative = impTranslator.translate()
      // Translate to Solidity
      val solidity = SolidityTranslator(
        imperative,
        dl.interfaces,
        dl.violations,
        materializedRelations,
        isInstrument = false,
        monitorViolation = false,
        enableProjection = true
      ).translate()

      val inliner = Inliner(solidity, dl.interfaces.map(_.relation))
      val inlinedSol = inliner.run()

      val outDir = "solidity/inline"
      createDirectory(outDir)
      val filename = Misc.getFileNameFromPath(filepath)
      val outfile = Paths.get(outDir, s"$filename.sol")
      Misc.writeToFile(inlinedSol.toString, outfile.toString)
    }

  }

  else {
    println(s"Unrecognized command: ${args(0)}")
  }
}
