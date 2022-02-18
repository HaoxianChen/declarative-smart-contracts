import datalog.{Parser, TypeChecker}
import imp.{ImperativeTranslator, SolidityTranslator}
import util.Misc

import java.nio.file.Paths

object Main extends App {
  val outDir = "/Users/hxc/projects/smart-contracts/datalog/dsc"
  if (args(0) == "parse") {
    val filepath = args(1)
    val filename = Misc.getFileNameFromPath(filepath)
    val dl = {
        val parser = new Parser()
        val inputStr = Misc.fileToString(filepath)
        val raw = parser.parseAll(parser.program, inputStr).get
        val typeChecker = TypeChecker()
        typeChecker.updateTypes(raw).setName(filename.capitalize)
    }
    println(dl)
    val imperative = ImperativeTranslator(dl).translate()
    println(imperative)
    val solidity = SolidityTranslator(imperative, dl.interfaces).translate()
    println(s"Solidity program:\n${solidity}")
    val outfile = Paths.get(outDir, s"$filename.sol")
    Misc.writeToFile(solidity.toString, outfile.toString)
  }
}
