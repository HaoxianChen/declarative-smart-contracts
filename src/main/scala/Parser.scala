import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

case class ParsingContext(relations: Set[Relation], rules: Set[Rule], interfaces: Set[Interface]) {
  val relsByName: Map[String,Relation] = relations.map(rel => rel.name -> rel).toMap
  def getProgram(): Program = Program(rules, interfaces)
  def addRelation(name: String, schema: List[String]): ParsingContext = {
    val types = schema.map(s => Type(s))
    val relation = SimpleRelation(name, types)
    this.copy(relations=relations+relation)
  }
  def addSingletonRelation(name: String, schema: List[String]): ParsingContext = {
    val types = schema.map(s => Type(s))
    val relation = SingletonRelation(name, types)
    this.copy(relations=relations+relation)
  }
  def addInterface(name: String, optRetIndexStr: Option[String]): ParsingContext = {
    val relation = relsByName(name)
    val sig = relation.sig
    val (inputTypes,optRetType): (List[Type],Option[Type]) = optRetIndexStr match {
      case Some(idxstr) => {
        val idx = idxstr.toInt
        val _inputTypes = sig.take(idx) ++ sig.takeRight(sig.size-idx-1)
        assert(_inputTypes.size + 1 == sig.size, s"sig: ${sig}, idx: $idx")
        (_inputTypes, Some(sig(idx)))
      }
      case None => (sig, None)
    }
    val interface = Interface(name, inputTypes, optRetType)
    this.copy(interfaces=interfaces+interface)
  }
  def addRule(rule: Rule) = this.copy(rules=rules+rule)
  def getLiteral(relName: String, fieldNames: List[String]): Literal = {
    val relation = relsByName(relName)
    require(fieldNames.size == relation.sig.size,
      s"${relName}${fieldNames}: expected ${relation.sig.size} variables, found ${fieldNames.size}")
    val fields: List[Parameter] = relation.sig.zip(fieldNames).map {
        case (t, name) => Variable(t, name)
      }
    Literal(relation, fields)
  }
}
object ParsingContext {
  def apply(): ParsingContext = ParsingContext(Set(), Set(), Set())
}

class Parser extends JavaTokenParsers{
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comments and identifiers
  // All valid Java identifiers may be used: See documentation for JavaTokenParsers.ident
  // Ignore C and C++-style comments. See: https://stackoverflow.com/a/5954831
  protected override val whiteSpace: Regex = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def fieldDecl: Parser[String] = ident ~> ":" ~> ident
  def fieldDeclList: Parser[List[String]] = repsep(fieldDecl, ",")

  def singletonRelationDecl: Parser[ParsingContext => ParsingContext] =
    (".decl" ~> "*" ~> ident ) ~ ("(" ~> fieldDeclList <~ ")") ^^ {
      case name ~ schema => {
        pc => pc.addSingletonRelation(name, schema)
      }
    }
  def relationDecl: Parser[ParsingContext => ParsingContext] =
    (".decl" ~> ident ) ~ ("(" ~> fieldDeclList <~ ")") ^^ {
      case name ~ schema => {
        pc => pc.addRelation(name, schema)
      }
    }
  def interfaceDecl: Parser[ParsingContext => ParsingContext] =
    (".interface" ~> ident) ~ opt("("~> wholeNumber <~")") ^^ {
      case name ~ optOutIndex => {
        pc => pc.addInterface(name, optOutIndex)
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
  def program: Parser[Program] = (relationDecl | singletonRelationDecl | interfaceDecl | ruleDecl ).* ^^ {
    fs => {
      val parsingContext = fs.foldLeft(ParsingContext()) {case (pc, f) => f(pc)}
      parsingContext.getProgram()
    }
  }
}
