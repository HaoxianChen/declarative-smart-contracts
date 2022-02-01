import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

case class ParsingContext(relations: Set[Relation], rules: Set[Rule]) {
  def addRelation(name: String, schema: List[String]): ParsingContext = {
    val types = schema.map(s => Type(s))
    val relation = Relation(name, types)
    this.copy(relations=relations+relation)
  }
  def addRule(rule: Rule) = this.copy(rules=rules+rule)
  def getLiteral(relName: String, fieldNames: List[String]): Literal = {
    val relation = {
      val f1 = relations.filter(_.name==relName)
      require(f1.size == 1, s"Unrecognized relation name $relName")
      f1.head
    }
    require(fieldNames.size == relation.sig.size,
      s"${relName}${fieldNames}: expected ${relation.sig.size} variables, found ${fieldNames.size}")
    val fields: List[Parameter] = relation.sig.zip(fieldNames).map {
        case (t, name) => Variable(t, name)
      }
    Literal(relation, fields)
  }
}
object ParsingContext {
  def apply(): ParsingContext = ParsingContext(Set(), Set())
}

class Parser extends JavaTokenParsers{
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comments and identifiers
  // All valid Java identifiers may be used: See documentation for JavaTokenParsers.ident
  // Ignore C and C++-style comments. See: https://stackoverflow.com/a/5954831
  protected override val whiteSpace: Regex = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def fieldDecl: Parser[String] = ident ~> ":" ~> ident
  def fieldDeclList: Parser[List[String]] = repsep(fieldDecl, ",")
  def relationDecl: Parser[ParsingContext => ParsingContext] =
    (".decl" ~> ident ) ~ ("(" ~> fieldDeclList <~ ")") ^^ {
      case name ~ schema => {
        pc => pc.addRelation(name, schema)
      }
    }

  def literalList: Parser[ParsingContext => List[Literal]] = repsep(literal, ",") ^^ {
    case fs => {
      pc => fs.map(f=>f(pc))
    }
  }
  def literal: Parser[ParsingContext => Literal] =
    (ident ~ ("(" ~> repsep(ident,",") <~ ")")) ^^ {
      case rel ~ fields => {
        pc => pc.getLiteral(rel, fields)
      }
    }
  def ruleDecl: Parser[ParsingContext => ParsingContext] =
    ((literal <~ ":-") ~ (literalList <~ ".") ) ^^ {
      case head ~ body => {
        pc => {
          val rule = Rule(head(pc), body(pc).toSet)
          pc.addRule(rule)
        }
      }
    }
  def program: Parser[Program] = (relationDecl | ruleDecl ).* ^^ {
    fs => {
      val parsingContext = fs.foldLeft(ParsingContext()) {case (pc, f) => f(pc)}
      Program(parsingContext.rules)
    }
  }
}
