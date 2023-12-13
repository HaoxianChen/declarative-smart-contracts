//import datalog.{Balance, MsgSender, MsgValue, Now, Parser, Program, Relation, This, TypeChecker}
import datalog._
import imp.{ImperativeTranslator, ImperativeTranslatorWithUpdateFusion, SolidityTranslator, Translator}
import util.Misc
import verification.{Prove, TransitionSystem, Verifier}
import util.Misc.{createDirectory, fileToString, isFileExists, parseProgram, readMaterializedRelationNames}

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
     //"erc1155.dl", // Lan: broke when run dependency-graph
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
    "auction.dl")

  def getMaterializedRelations(dl: Program, filepath: String): List[(Set[Relation],Set[Relation])] = {
    if (isFileExists(filepath)) {
      val allPlans = readMaterializedRelationNames(filepath)
      var rowLists: List[(Set[Relation],Set[Relation])] = List()
      for(plan <- allPlans){
        val emptyIndex = plan.indexOf("")
        val (minList, funcList) = plan.splitAt(emptyIndex)
        val minListSet = minList.toSet
        val funcListSet = funcList.tail.toSet  // skip the "" entry
        val minSet = minListSet.flatMap(n => dl.relations.filter(_.name == n))
        require(minSet.size == minListSet.size)
        val funcSet = funcListSet.flatMap(n => dl.relations.filter(_.name == n))
        require(funcSet.size == funcListSet.size)
        rowLists = ((minSet, funcSet)) :: rowLists
      }
      rowLists
    }
    else {
      List((Set.empty[Relation],Set.empty[Relation]))
    }
  }

  def run(filepath: String, displayResult: Boolean, outDir: String, isInstrument: Boolean, monitorViolations: Boolean,
          consolidateUpdates: Boolean, materializePath: String = s"", enableProjection:Boolean,
          arithmeticOptimization: Boolean = true): Unit = {
    createDirectory(outDir)
    val filename = Misc.getFileNameFromPath(filepath)
    val dl_org = parseProgram(filepath)  // the program object => original dl version
    val rowLists = getMaterializedRelations(dl_org, materializePath)
    println(rowLists.getClass.getName)
    println(rowLists.mkString(","))
    var count = 0
    for(row <- rowLists){
      count += 1
      println(row.getClass.getName)
      val (materializedRelations: Set[Relation], functionalRelations: Set[Relation]) = row
      val dl = dl_org.addFunctions(functionalRelations)
      val impTranslator: ImperativeTranslator = if (consolidateUpdates) {
        ImperativeTranslatorWithUpdateFusion(dl, materializedRelations, isInstrument, monitorViolations,
          arithmeticOptimization = arithmeticOptimization, enableProjection = enableProjection)
      }
      else {
        new ImperativeTranslator(dl, materializedRelations, isInstrument, monitorViolations,
          arithmeticOptimization = arithmeticOptimization, enableProjection = enableProjection)
      }
      val imperative = impTranslator.translate()
//      val solidity = SolidityTranslator(imperative, dl.interfaces, dl.violations, materializedRelations,
//        isInstrument, monitorViolations, enableProjection).translate()
      val solidity = SolidityTranslator(imperative, dl, dl.interfaces, dl.violations, materializedRelations,
        isInstrument, monitorViolations, enableProjection).translate()
      val outfile = Paths.get(outDir, s"$filename/$filename$count.sol")
      Misc.writeToFile(solidity.toString, outfile.toString)
      if (displayResult) {
        println(dl)
        println(imperative)
        println(s"Solidity program:\n${solidity}")
      }
      println(s"${impTranslator.ruleSize} rules.")
    }

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
    var outDirectory = "solidity/default"
    if (options.contains("out")) {
    	outDirectory = options("out").toString
    }
    run(filepath, displayResult = true, outDir=outDirectory,
    //run(filepath, displayResult = true, outDir=options("out").toString,
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
  }

  else if (args(0) == "dependency-graph") {
    for (p <- allBenchmarks) {
      val filepath = Paths.get(benchmarkDir, p).toString
      println(filepath)
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

  // adding new features for simplification
  else if (args(0) == "simplification-check") {
    for (p <- allBenchmarks) {
      val filepath = Paths.get(benchmarkDir, p).toString
      val dl = parseProgram(filepath)
      var noSimplificationSet: Set[String] = Set()

      // originally, relations that can not be skipped:
      // => Original noSkippedSet: public relations, txns, direct body relations for txns, manually defined functions
      // 1. write in scala: complex; 2. write in python: separate code

      // complete set of relations that can not be skipped
      // => Original noSkippedSet (as above) + AnalysisBases noSkippedSet (discussed as follows)
      dl.rules.foreach { element =>
//        val head = element.head.toString.split("\\(")
//        val headFields = head(1).split("\\)")(0).split(",")
        val head = element.head.relation.name
        val headFields = element.head.relation.paramList.map(p => p.name)
        val bodies = element.body.toArray
        val formulas = element.functors.toArray

        // 1. if the body relation use exactly the same field as the head field
        // => differential is constant, no judgement operation

        // 2. if some field(s) from the body/head relation form a formula relationship
        // => differential may not be constant: s = m*n (however, we do not find a case up to now)
        // => may contain judgement operations: <, <=, >, >=, !=, == (we have to manually list out all cases)
        // => otherwise, differential is constant, no judgement operation: =, :=, +, -

        // Question: if the rule contains a judgement, all relations in the rule can not be skipped?
        // Or only relations whose fields are in the rule can not be skipped?
        // version 1:
        for (formula <- formulas) {
          println("current formula:", formula)
          val judgementPattern = """[<>!=]=|[<>](?![=<>])""".r
          val judgementMatches = judgementPattern.findAllIn(formula.toString).toList

          if (judgementMatches.nonEmpty) {
            println("contain judgement operator")
            // for every body relation whose field is in this judgement, this body relation can not be skipped

            val separatorPattern = """(?![<>=!:])=(?![<>=!:])|[<>=!:]=|[<>](?![=<>])|[\-+()]""".r
            val formulaFields = separatorPattern.split(formula.toString).map(_.trim).filter(_.nonEmpty)
            println("formula fields: ", formulaFields.mkString(","))

            // find all the body/head relations containing fields appearing in judgement operations
            for (formulaField <- formulaFields) {
              for (body <- bodies) {
                body.fields.foreach { bodyField =>
                  if (bodyField.toString == formulaField) { // this body relation can not be skipped
                    // Lan: to do
                  }
                }
              }
              for (headField <- headFields) {
                if (headField == formulaField) { // this head relation can not be skipped
                  // Lan: to do
                }
              }
            }
          }
        }
        for (body <- bodies) { // if body itself contains a judgement operation (true, false), it can not be skipped
          // "closed(true)"  equals to   "close(p), p==true"
          body.fields.foreach { bodyField =>
            if (bodyField.toString == "true" || bodyField.toString == "false") { // this body relation can not be skipped
              // Lan: to do
            }
          }
        }
        // version 2:
        // if the judgement turn from false to true, then we will have to update the head based on all other bodies
        // if it contains a judgement in the rule, all body relations should not be skipped
        var judgementFlag = false
        for (formula <- formulas) {
          println("current formula:", formula)
          val judgementPattern = """[<>!=]=|[<>](?![=<>])""".r
          val judgementMatches = judgementPattern.findAllIn(formula.toString).toList
          if (judgementMatches.nonEmpty) {
            println("contain judgement operator")
            judgementFlag = true
          }
        }
        for (body <- bodies) { // if body itself contains a judgement operation (true, false), it can not be skipped
          // "closed(true)"  equals to   "close(p), p==true"
          body.fields.foreach { bodyField =>
            if (bodyField.toString == "true" || bodyField.toString == "false") { // this body relation can not be skipped
              judgementFlag = true
            }
          }
        }
        if (judgementFlag) { // all body relations can not be skipped
          for(body <- bodies){
            noSimplificationSet += body.relation.name
          }
        }

        // 3. if some field(s) from the body relation have a aggregated formula relationship with this head field
        // Now we only have one operator: sum
        // => differential is constant, no judgement operation
        println("relations that can not be simplified: ", noSimplificationSet.mkString(","))
        println("finish a rule\n")
      }

      val outfile = s"view-materialization/cannot-simplified/${dl.name}.csv"
      val outStr = noSimplificationSet.mkString(",") + "\n"
      Misc.writeToFile(outStr, outfile)
      println("finish a contract\n")
    }
  }

  else if (args(0) == "function-check") {
    for (p <- allBenchmarks) {
//      if (p == "ltcSwapAsset.dl") {
      val filepath = Paths.get(benchmarkDir, p).toString
      val dl = parseProgram(filepath)
      var noMaterializationSet: Set[String] = Set()
      dl.rules.foreach { element =>
        val head_rel = element.head.relation
        val headFields = element.head.relation.paramList.map(p => p.name)
        val bodies = element.body.toArray
        val formulas = element.functors.toArray

        var afterConstructor = false
        var isTxn = false
        bodies.foreach { body =>
          if (body.relation.name == "constructor") afterConstructor = true
          if (body.relation.name.startsWith("recv_")) isTxn = true
        }
        // type 1: body contains now (block.timestamp)
        if (!afterConstructor && !isTxn){
          bodies.foreach { body =>
            if (body.relation.name == "now") {
              noMaterializationSet += head_rel.name
            }
          }
        }
        // type 2: without primary keys: boolean function, txn, constructor, singleton
        // after selection => add all boolean functions, together with some txn
        val primaryKeyIndices: Map[Relation, List[Int]] = dl.relations.map {
          case rel: SimpleRelation => rel -> dl.relationIndices.getOrElse(rel, List())
          case rel: SingletonRelation => rel -> List()
          case rel: ReservedRelation => rel -> List()
        }.toMap
        head_rel match {
          case _:SingletonRelation | _:ReservedRelation => {}
          case rel: SimpleRelation => {
            if(dl.relationIndices.getOrElse(rel, List()).isEmpty) {
              if (!isTxn) noMaterializationSet += rel.name
            }
          }
        }

      }
      val outfile = s"view-materialization/cannot-materialized/${dl.name}.csv"
      val outStr = noMaterializationSet.mkString(",") + "\n"
      Misc.writeToFile(outStr, outfile)
      println(dl.name)
      println("finish a contract\n", outStr)

    }
  }
  else if (args(0) == "testz3") {
    TransitionSystem.testTS()
    // Prove.testZ3()
    // Prove.testTuple()
  }

  else {
    println(s"Unrecognized command: ${args(0)}")
  }
}
