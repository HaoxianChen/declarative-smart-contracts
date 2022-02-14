package datalog

import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

case class ParsingContext(relations: Set[Relation], rules: Set[Rule], interfaces: Set[Interface],
                         /** The index of column on which the table is indexed by.
                          *  Assume each row has a unique index value.
                          *  */
                          relationIndices: Map[SimpleRelation, Int]
                         ) {
  val relsByName: Map[String,Relation] = relations.map(rel => rel.name -> rel).toMap
  def getProgram(): Program = Program(rules, interfaces, relationIndices)
  private def getTypes(schema: List[(String, String)]) :(List[String], List[Type]) = {
    val memberNames = schema.map(_._1)
    val types = schema.map ( s => s._2 match {
        case "bool" => BooleanType()
        case "uint" => Type.uintType
        case "int" => Type.integerType
        case _ => Type(s._2)
      }
    )
    (memberNames, types)
  }
  def addRelation(name: String, schema: List[(String,String)], optIndexStr: Option[String]): ParsingContext = {
    val (memberNames, types) = getTypes(schema)
    val relation = SimpleRelation(name, types, memberNames)
    optIndexStr match {
      case Some(s) => {
        val index = s.toInt
        require(!relationIndices.contains(relation))
        this.copy(relations=relations+relation, relationIndices=relationIndices+(relation->index))
      }
      case None => this.copy(relations=relations+relation)
    }
  }
  def addSingletonRelation(name: String, schema: List[(String,String)]): ParsingContext = {
    val (memberNames, types) = getTypes(schema)
    val relation = SingletonRelation(name, types, memberNames)
    this.copy(relations=relations+relation)
  }
  def addInterface(name: String, optRetIndexStr: Option[String]): ParsingContext = {
    val relation = relsByName(name)
    val sig = relation.sig
    val (inputIndices, optOutIndex): (List[Int], Option[Int]) = optRetIndexStr match {
      case Some(idxstr) => {
        val idx = idxstr.toInt
        val _inputIndices = sig.indices.take(idx) ++ sig.indices.takeRight(sig.size-idx-1)
        assert(_inputIndices.size + 1 == sig.size, s"sig: ${sig}, idx: $idx")
        (_inputIndices.toList, Some(idx))
      }
      case None => (sig.indices.toList, None)
    }
    val interface = Interface(relation, inputIndices, optOutIndex)
    this.copy(interfaces=interfaces+interface)
  }
  def addRule(rule: Rule) = this.copy(rules=rules+rule)
  def getLiteral(relName: String, fieldNames: List[String]): Literal = {
    val relation = relsByName(relName)
    require(fieldNames.size == relation.sig.size,
      s"${relName}${fieldNames}: expected ${relation.sig.size} variables, found ${fieldNames.size}")
    val fields: List[Parameter] = relation.sig.zip(fieldNames).map {
        case (t, name) => t match {
            case _:UnitType|_:AnyType|_:CompoundType => throw new Exception(s"Unsupported type ${t}")
            case _:SymbolType => Variable(t, name)
            case _:NumberType => if (name.forall(_.isDigit)) Constant(t,name) else  Variable(t,name)
            case BooleanType() => name match {
              case "true"|"false" => Constant(t, name)
              case _ => Variable(t, name)
            }
          }
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
        case _ => throw new Exception(s"Unsupported aggregate type $aggParamTyped")
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

class ArithmeticParser extends JavaTokenParsers {
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Comments and identifiers
  // All valid Java identifiers may be used: See documentation for JavaTokenParsers.ident
  // Ignore C and C++-style comments. See: https://stackoverflow.com/a/5954831
  protected override val whiteSpace: Regex = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  private def variable: Parser[Variable] = ident ^^ {x => Variable(AnyType(), x)}
  private def constant: Parser[Constant] = wholeNumber ^^ {x => Constant(Type.integerType, x)}
  private def parameter: Parser[Param] = (variable | constant) ^^ { p => Param(p)}

  private def term : Parser[Arithmetic] = "(" ~> expr <~ ")" | parameter

  def expr: Parser[Arithmetic] = term ~ rep("[+-]".r ~ term) ^^ {
    case t ~ ts => ts.foldLeft(t) {
      case (t1, "+" ~ t2) => Add(t1, t2)
      case (t1, "-" ~ t2) => Sub(t1, t2)
    }
  }

  def assignment: Parser[Functor] = parameter ~ ":=" ~ expr ^^ {
    case p ~ op ~ e => Assign(p,e)
  }

  def comparison: Parser[Functor] = (expr) ~ (">="|"<="|">"|"<") ~ expr ^^ {
    case a ~ op ~ b => op match {
      case ">=" => Geq(a,b)
      case "<=" => Leq(a,b)
      case ">" => Greater(a,b)
      case "<" => Lesser(a,b)
    }
  }

  def functor: Parser[Functor] = comparison | assignment

}

class Parser extends ArithmeticParser {
  def fieldDecl: Parser[(String, String)] = ident ~  ":" ~ ident ^^ {case name ~ _ ~ t => (name,t)}
  def fieldDeclList: Parser[List[(String, String)]] = repsep(fieldDecl, ",")

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
    (ident ~ ("(" ~> repsep(ident|wholeNumber,",") <~ ")")) ^^ {
      case rel ~ fields => {
        pc => pc.getLiteral(rel, fields)
      }
    }

  def variable: Parser[Variable] = ident ^^ {x => Variable(AnyType(), x)}
  def constant: Parser[Constant] = wholeNumber ^^ {x => Constant(Type.integerType, x)}
  def parameter: Parser[Parameter] = variable | constant
  def functorFromPc: Parser[ParsingContext => Functor] = functor ^^ {f => _:ParsingContext => f}
  def aggregator: Parser[ParsingContext => Aggregator] = (variable <~ "=") ~ "sum" ~ (variable <~ ":") ~ literal ^^ {
    case s ~ op ~ n ~ fLit => pc => {
      val lit: Literal = fLit(pc)
      pc.getAggregator(op,s,n,lit)
    }
  }

  def ruleDecl: Parser[ParsingContext => ParsingContext] =
    ((literal <~ ":-") ~ (repsep(literal|functorFromPc|aggregator, ",") <~ ".") ) ^^ {
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
