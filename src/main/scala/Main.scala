import datalog.{Parser, TypeChecker}
import imp.{ImperativeTranslator, SolidityTranslator}
import util.Misc

import java.nio.file.Paths

object Main extends App {
  val outDir = "/Users/hxc/projects/smart-contracts/datalog/dsc"
  val benchmarkDir = "/Users/hxc/projects/declarative-smart-contract/benchmarks"
  val allBenchmarks = List("auction.dl", "crowFunding.dl", "erc20.dl", "nft.dl", "wallet.dl")

  def run(filepath: String, displayResult: Boolean): Unit = {
    val filename = Misc.getFileNameFromPath(filepath)
    val dl = {
      val parser = new Parser()
      val inputStr = Misc.fileToString(filepath)
      val raw = parser.parseAll(parser.program, inputStr).get
      val typeChecker = TypeChecker()
      typeChecker.updateTypes(raw).setName(filename.capitalize)
    }
    val imperative = ImperativeTranslator(dl).translate()
    val solidity = SolidityTranslator(imperative, dl.interfaces,dl.violations).translate()
    val outfile = Paths.get(outDir, s"$filename.sol")
    Misc.writeToFile(solidity.toString, outfile.toString)
    if (displayResult) {
      println(dl)
      println(imperative)
      println(s"Solidity program:\n${solidity}")
    }
  }

  if (args(0) == "parse") {
    val filepath = args(1)
    run(filepath, displayResult = true)
  }
  if (args(0) == "regression-test") {
    for (p <- allBenchmarks) {
      println(p)
      val filepath = Paths.get(benchmarkDir, p).toString
      run(filepath, displayResult = false)
    }

  }

}
