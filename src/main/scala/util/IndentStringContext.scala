package util

class IndentStringContext(sc: StringContext) {
  def e(args: Any*):String = {
    val sb = new StringBuilder()
    for ((s, a) <- sc.parts zip args) {
      sb append s

      val ind = getindent(s)
      if (ind.size > 0) {
        sb append a.toString().replaceAll("\n", "\n" + ind)
      } else {
        sb append a.toString()
      }
    }
    if (sc.parts.size > args.size)
      sb append sc.parts.last

    sb.toString()
  }

  // get white indent after the last new line, if any
  def getindent(str: String): String = {
    val lastnl = str.lastIndexOf("\n")
    if (lastnl == -1) ""
    else {
      val ind = str.substring(lastnl + 1)
      if (ind.trim.isEmpty) ind  // ind is all whitespace. Use this
      else ""
    }
  }
}

object Indenter {
  // top level implicit defs allowed only in 2.10 and above
  implicit  def toISC(sc: StringContext) = new IndentStringContext(sc)
}