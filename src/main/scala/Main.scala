import Main.outDir
import datalog.{Parser, TypeChecker}
import imp.{ImperativeTranslator, SolidityTranslator}
import util.Misc
import verification.{Prove, TransitionSystem, Verifier}
import util.Misc.createDirectory

import java.nio.file.Paths

object Main extends App {
  val outDir = "solidity/dsc"
  val outDirWithInstrumentations = "solidity/dsc-instrument"
  val benchmarkDir = "benchmarks"
  val allBenchmarks = List("crowFunding.dl", "erc20.dl", "nft.dl", "wallet.dl", "vestingWallet.dl", "auction.dl")

  def run(filepath: String, displayResult: Boolean, outDir: String, isInstrument: Boolean): Unit = {
    createDirectory(outDir)
    val filename = Misc.getFileNameFromPath(filepath)
    val dl = {
      val parser = new Parser()
      val inputStr = Misc.fileToString(filepath)
      val raw = parser.parseAll(parser.program, inputStr).get
      val typeChecker = TypeChecker()
      typeChecker.updateTypes(raw).setName(filename.capitalize)
    }
    val impTranslator =ImperativeTranslator(dl, isInstrument)
    val imperative = impTranslator.translate()
    val solidity = SolidityTranslator(imperative, dl.interfaces,dl.violations,isInstrument).translate()
    val outfile = Paths.get(outDir, s"$filename.sol")
    Misc.writeToFile(solidity.toString, outfile.toString)
    if (displayResult) {
      println(dl)
      println(imperative)
      println(s"Solidity program:\n${solidity}")
    }
    println(s"${impTranslator.ruleSize} rules.")
  }

  if (args(0) == "compile") {
    val filepath = args(1)
    val isInstrument = args(2).toBoolean
    val _outDir = if(isInstrument) outDirWithInstrumentations else outDir
    run(filepath, displayResult = true, outDir=_outDir, isInstrument = isInstrument)
  }
  if (args(0) == "test") {
    val _outDir = outDir
    for (p <- allBenchmarks) {
      println(p)
      val filepath = Paths.get(benchmarkDir, p).toString
      run(filepath, displayResult = false, outDir=_outDir, isInstrument = false)
    }
  }
  if (args(0) == "test-instrument") {
    val _outDir = outDirWithInstrumentations
    for (p <- allBenchmarks) {
      println(p)
      val filepath = Paths.get(benchmarkDir, p).toString
      run(filepath, displayResult = false, outDir=_outDir, isInstrument = true)
    }
  }

  if (args(0) == "verify") {
    val filepath = args(1)

    val filename = Misc.getFileNameFromPath(filepath)
    val dl = {
      val parser = new Parser()
      val inputStr = Misc.fileToString(filepath)
      val raw = parser.parseAll(parser.program, inputStr).get
      val typeChecker = TypeChecker()
      typeChecker.updateTypes(raw).setName(filename.capitalize)
    }
    val impTranslator =ImperativeTranslator(dl, isInstrument=true)
    val imperative = impTranslator.translate()
    println(imperative)
    val verifier = new Verifier(dl, imperative)
    verifier.check()

  }

  if (args(0) == "test-verification") {
    for (p <- List("crowFunding.dl", "erc20.dl",
      "nft.dl",
      "wallet.dl",
      "vestingWallet.dl", "auction.dl")) {
      println(p)
      val filepath = Paths.get(benchmarkDir, p).toString
      val filename = Misc.getFileNameFromPath(filepath)
      val dl = {
        val parser = new Parser()
        val inputStr = Misc.fileToString(filepath)
        val raw = parser.parseAll(parser.program, inputStr).get
        val typeChecker = TypeChecker()
        typeChecker.updateTypes(raw).setName(filename.capitalize)
      }
      val impTranslator =ImperativeTranslator(dl, isInstrument=true)
      val imperative = impTranslator.translate()
      val verifier = new Verifier(dl, imperative)
      verifier.check()
    }
  }

  if (args(0) == "testz3") {
    TransitionSystem.testTS()
    // Prove.testZ3()
    // Prove.testTuple()
  }

}
