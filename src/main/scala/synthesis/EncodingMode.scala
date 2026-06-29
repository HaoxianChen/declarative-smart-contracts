package synthesis

sealed trait EncodingMode {
  def label: String
}

object EncodingMode {
  case object Concrete extends EncodingMode {
    override val label: String = "concrete"
  }

  case object SymbolicCexBlocking extends EncodingMode {
    override val label: String = "symbolic-fixed-cex-blocking"
  }

  def fromString(raw: String): EncodingMode = raw.toLowerCase match {
    case "concrete" => Concrete
    case "symbolic" | "symbolic-cex-blocking" | "symbolic-fixed-cex-blocking" |
         "symboliccexblocking" | "symbolicfixedcexblocking" =>
      SymbolicCexBlocking
    case other =>
      throw new IllegalArgumentException(
        s"Unknown encoding mode '$other'. Expected concrete or symbolic.")
  }
}
