package datalog

import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

case class ParsingContext(relations: Set[Relation], rules: Set[Rule], interfaces: Set[Interface],
                         /** The index of column on which the table is indexed by.
                          *  Assume each row has a unique index value.
                          *  */
                          relationIndices: Map[Relation, Int]
                         ) {
  val relsByName: Map[String,Relation] = relations.map(rel => rel.name -> rel).toMap
  def getProgram(): Program = Program(rules, interfaces, relationIndices)
  def addRelation(name: String, schema: List[String], optIndexStr: Option[String]): ParsingContext = {
    val types = schema.map(s => Type(s))
    val relation = SimpleRelation(name, types)
    optIndexStr match {
      case Some(s) => {
        val index = s.toInt
        require(!relationIndices.contains(relation))
        this.copy(relations=relations+relation, relationIndices=relationIndices+(relation->index))
      }
      case None => this.copy(relations=relations+relation)
    }
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
    val interface = Interface(relation, inputTypes, optRetType)
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
  def getAggregator(opName: String, aggResult: Variable, aggParam: Variable, literal: Literal): Aggregator = {
    val aggParamTyped: Variable = {
      val idx = literal.fields.map(_.name).indexOf(aggParam.name)
      val _type = literal.relation.sig(idx)
      aggParam.copy(_type=_type)
    }
    val aggResultTyped: Variable = {
      val _type = aggParamTyped._type.name match {
        case "uint" => Type.uintType
        case "int" => Type.integerType
        case _ => ???
      }
      aggResult.copy(_type=_type)
    }
    opName match {
      case "sum" => Sum(literal, aggParamTyped, aggResultTyped)
      case _ => ???
    }
  }
}
object ParsingContext {
  def apply(): ParsingContext = ParsingContext(relations = Relation.reservedRelations, Set(), Set(), Map())
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
    (".decl" ~> ident ) ~ ("(" ~> fieldDeclList <~ ")") ~ opt("[" ~> wholeNumber <~ "]") ^^ {
      case name ~ schema ~ optIndex => {
        pc => pc.addRelation(name, schema, optIndex)
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

  def variable: Parser[Variable] = ident ^^ {x => Variable(Type.any, x)}
  def constant: Parser[Constant] = wholeNumber ^^ {x => Constant(Type.integerType, x)}
  def parameter: Parser[Parameter] = variable | constant
  def functor: Parser[ParsingContext => Functor] = (parameter) ~ (">="|"<="|">"|"<") ~ parameter ^^ {
    case a ~ op ~ b => _ => op match {
      case ">=" => Geq(a,b)
      case "<=" => Leq(a,b)
      case ">" => Greater(a,b)
      case "<" => Lesser(a,b)
    }
  }
  def aggregator: Parser[ParsingContext => Aggregator] = (variable <~ "=") ~ "sum" ~ (variable <~ ":") ~ literal ^^ {
    case s ~ op ~ n ~ fLit => pc => {
      val lit: Literal = fLit(pc)
      pc.getAggregator(op,s,n,lit)
    }
  }

  def ruleDecl: Parser[ParsingContext => ParsingContext] =
    ((literal <~ ":-") ~ (repsep(literal|functor|aggregator, ",") <~ ".") ) ^^ {
      case head ~ fs =>
        pc => {
          var body: Set[Literal] = Set()
          var functors: Set[Functor] = Set()
          var aggregators: Set[Aggregator] = Set()
          for (f <- fs) f(pc) match {
              case l: Literal => body += l
              case f: Functor => functors += f
              case a: Aggregator => aggregators += a
            }
          val rule = Rule(head(pc), body, functors, aggregators)
          pc.addRule(rule)
        }
      }
  def program: Parser[Program] = (relationDecl | singletonRelationDecl | interfaceDecl | ruleDecl ).* ^^ {
    fs => {
      val parsingContext = fs.foldLeft(ParsingContext()) {case (pc, f) => f(pc)}
      parsingContext.getProgram()
    }
  }
}
