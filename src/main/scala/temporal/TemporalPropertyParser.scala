package temporal

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

/**
 * Parser for temporal properties with uppercase keywords.
 * 
 * Syntax:
 *   ALWAYS (expr)
 *   ONCE (expr)
 *   NOT (expr)
 *   expr AND expr
 *   expr OR expr
 *   expr IMPLY expr
 *   expr == expr, expr != expr, expr < expr, expr <= expr, expr > expr, expr >= expr
 *   identifier(args...)
 *   identifier
 *   numeric literals
 */
object TemporalPropertyParser extends RegexParsers {
  
  override def skipWhitespace = true
  
  // Lexical elements
  def identifier: Parser[String] = """[a-zA-Z_][a-zA-Z0-9_]*""".r
  def number: Parser[BigInt] = """\d+""".r ^^ { s => BigInt(s) }
  
  // Primary expressions
  def primaryExpr: Parser[TemporalExpr] = 
    boolLiteral | numericLiteral | functionCall | identifierExpr | parenExpr
  
  def boolLiteral: Parser[TemporalExpr] = 
    ("true" | "TRUE") ^^ { _ => TemporalExpr.BoolLiteral(true) } |
    ("false" | "FALSE") ^^ { _ => TemporalExpr.BoolLiteral(false) }
  
  def numericLiteral: Parser[TemporalExpr] = 
    number ^^ { n => TemporalExpr.NumericLiteral(n) }
  
  def identifierExpr: Parser[TemporalExpr] = 
    identifier ^^ { name => TemporalExpr.Identifier(name) }
  
  def functionCall: Parser[TemporalExpr] = 
    identifier ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ {
      case name ~ args => TemporalExpr.FunctionCall(name, args)
    }
  
  def parenExpr: Parser[TemporalExpr] = 
    "(" ~> expr <~ ")"
  
  // Temporal operators (highest precedence)
  def temporalExpr: Parser[TemporalExpr] = 
    ("ONCE" ~> "(" ~> expr <~ ")") ^^ { e => TemporalExpr.Once(e) } |
    ("ALWAYS" ~> "(" ~> expr <~ ")") ^^ { e => TemporalExpr.Always(e) } |
    ("NOT" ~> "(" ~> expr <~ ")") ^^ { e => TemporalExpr.Not(e) } |
    ("NOT" ~> primaryExpr) ^^ { e => TemporalExpr.Not(e) } |
    primaryExpr
  
  // Comparison operators
  def comparisonExpr: Parser[TemporalExpr] = 
    temporalExpr ~ opt(compOp ~ temporalExpr) ^^ {
      case left ~ None => left
      case left ~ Some(op ~ right) => op(left, right)
    }
  
  def compOp: Parser[(TemporalExpr, TemporalExpr) => TemporalExpr] = 
    ("==" | "=") ^^ { _ => (l: TemporalExpr, r: TemporalExpr) => TemporalExpr.Eq(l, r) } |
    "!=" ^^ { _ => (l: TemporalExpr, r: TemporalExpr) => TemporalExpr.Neq(l, r) } |
    "<=" ^^ { _ => (l: TemporalExpr, r: TemporalExpr) => TemporalExpr.Le(l, r) } |
    ">=" ^^ { _ => (l: TemporalExpr, r: TemporalExpr) => TemporalExpr.Ge(l, r) } |
    "<" ^^ { _ => (l: TemporalExpr, r: TemporalExpr) => TemporalExpr.Lt(l, r) } |
    ">" ^^ { _ => (l: TemporalExpr, r: TemporalExpr) => TemporalExpr.Gt(l, r) }
  
  // Logical operators (left-associative)
  def andExpr: Parser[TemporalExpr] = 
    comparisonExpr ~ rep("AND" ~> comparisonExpr) ^^ {
      case first ~ rest => rest.foldLeft(first) { case (acc, e) => TemporalExpr.And(acc, e) }
    }
  
  def orExpr: Parser[TemporalExpr] = 
    andExpr ~ rep("OR" ~> andExpr) ^^ {
      case first ~ rest => rest.foldLeft(first) { case (acc, e) => TemporalExpr.Or(acc, e) }
    }
  
  def implyExpr: Parser[TemporalExpr] = 
    orExpr ~ opt(("IMPLY" | "IMPLIES") ~> expr) ^^ {
      case left ~ None => left
      case left ~ Some(right) => TemporalExpr.Imply(left, right)
    }
  
  // Top-level expression
  def expr: Parser[TemporalExpr] = implyExpr
  
  /**
   * Parse a single temporal property formula.
   */
  def parseFormula(input: String): Either[String, TemporalExpr] = {
    parseAll(expr, input.trim) match {
      case Success(result, _) => Right(result)
      case NoSuccess(msg, next) => 
        Left(s"Parse error at position ${next.pos}: $msg")
    }
  }
  
  /**
   * Parse a temporal properties file.
   * 
   * Format:
   *   - Lines starting with "//" are comments
   *   - Empty lines are ignored
   *   - Each non-comment line is a temporal formula
   */
  def parseFile(filepath: String): (List[TemporalProperty], List[(Int, String)]) = {
    val lines = Source.fromFile(filepath).getLines().toList
    val properties = scala.collection.mutable.ListBuffer[TemporalProperty]()
    val errors = scala.collection.mutable.ListBuffer[(Int, String)]()
    
    var currentComment: Option[String] = None
    
    lines.zipWithIndex.foreach { case (line, idx) =>
      val lineNum = idx + 1
      val trimmed = line.trim
      
      if (trimmed.isEmpty) {
        currentComment = None // Reset comment on empty line
      } else if (trimmed.startsWith("//")) {
        // Extract comment text
        val commentText = trimmed.substring(2).trim
        currentComment = Some(commentText)
      } else {
        // Try to parse as temporal property
        parseFormula(trimmed) match {
          case Right(expr) =>
            properties += TemporalProperty(lineNum, expr, trimmed, currentComment)
            currentComment = None
          case Left(errMsg) =>
            errors += ((lineNum, errMsg))
            currentComment = None
        }
      }
    }
    
    (properties.toList, errors.toList)
  }
  
  /**
   * Parse file and throw exception on errors.
   */
  def parseFileOrThrow(filepath: String): List[TemporalProperty] = {
    val (properties, errors) = parseFile(filepath)
    if (errors.nonEmpty) {
      val errorMsg = errors.map { case (line, msg) => 
        s"Line $line: $msg" 
      }.mkString("\n")
      throw new Exception(s"Failed to parse temporal properties:\n$errorMsg")
    }
    properties
  }
}


