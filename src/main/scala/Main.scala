import datalog.{Parser, TypeChecker}
import imp.{ImperativeTranslator, SolidityTranslator}
import util.Misc

object Main extends App {
  if (args(0) == "parse") {
    val filename = args(1)
    val dl = {
        val parser = new Parser()
        val inputStr = Misc.fileToString(filename)
        val raw = parser.parseAll(parser.program, inputStr).get
        val typeChecker = TypeChecker()
        typeChecker.updateTypes(raw)
    }
    println(dl)
    val imperative = ImperativeTranslator().translate(dl)
    println(imperative)
    val solidity = SolidityTranslator(imperative, dl.interfaces).translate()
    println(s"Solidity program:\n${solidity}")
  }
}
