object Main extends App {
  if (args(0) == "parse") {
    val filename = args(1)
    val parser = new Parser()
    val inputStr = Misc.fileToString(filename)
    val program = parser.parseAll(parser.program, inputStr).get
    println(program)
  }
}
