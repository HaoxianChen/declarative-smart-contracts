package util

import datalog.{Program, Relation, Type}

import scala.sys.process._
import scala.util.parsing.json.JSON

/**
  * Minimal Solidity AST checker based on `solc --ast-compact-json`.
  *
  * We do NOT use regex. We parse the AST JSON and verify that every `.udf` relation has
  * a matching Solidity function definition in `udf.sol`.
  *
  * UDF convention:
  * - A `.udf` relation declares (in1, in2, ..., out)
  * - The Solidity function has inputs (in1..inN) and returns (out) (exactly 1 return)
  */
object SolcAst {

  case class SolFunctionSig(
    name: String,
    inputs: List[String],
    outputs: List[String],
    stateMutability: String,
    visibility: String
  )

  private def normalizeSolType(typeString: String): String = {
    if (typeString == null) return ""
    val t = typeString.trim
    if (t.startsWith("uint")) "uint"
    else if (t.startsWith("int")) "int"
    else if (t.startsWith("bool")) "bool"
    else if (t.startsWith("address")) "address"
    else t
  }

  private def normalizeDatalogType(t: Type): String = t.name match {
    case "uint" => "uint"
    case "int" => "int"
    case "bool" => "bool"
    case "address" => "address"
    case other => other
  }

  private def runSolcAstJson(udfSolPath: String): String = {
    val cmd = Seq("solc", "--ast-compact-json", udfSolPath)
    val out = cmd.!!
    // `solc` may print warnings before the JSON AST. Be robust by extracting the JSON object.
    val start = out.indexOf('{')
    val end = out.lastIndexOf('}')
    if (start >= 0 && end > start) out.substring(start, end + 1) else out
  }

  private def parseJson(json: String): Any = {
    JSON.parseFull(json).getOrElse(throw new Exception("Failed to parse solc AST JSON"))
  }

  private def asMap(x: Any): Map[String, Any] = x match {
    case m: Map[_, _] => m.asInstanceOf[Map[String, Any]]
    case _ => Map.empty
  }

  private def asList(x: Any): List[Any] = x match {
    case l: List[_] => l.asInstanceOf[List[Any]]
    case _ => Nil
  }

  private def findAllNodes(root: Any, pred: Map[String, Any] => Boolean): List[Map[String, Any]] = {
    def rec(x: Any): List[Map[String, Any]] = x match {
      case m: Map[_, _] =>
        val mm = m.asInstanceOf[Map[String, Any]]
        val here = if (pred(mm)) List(mm) else Nil
        val children = mm.values.toList.flatMap(rec)
        here ++ children
      case l: List[_] =>
        l.toList.flatMap(rec)
      case _ => Nil
    }
    rec(root)
  }

  private def extractFunctionSig(fnNode: Map[String, Any]): SolFunctionSig = {
    val name = fnNode.getOrElse("name", "").toString
    val paramsNode = asMap(fnNode.getOrElse("parameters", Map.empty))
    val retsNode = asMap(fnNode.getOrElse("returnParameters", Map.empty))
    val stateMutability = fnNode.getOrElse("stateMutability", "").toString
    val visibility = fnNode.getOrElse("visibility", "").toString

    def extractParamTypes(ps: Map[String, Any]): List[String] = {
      val params = asList(ps.getOrElse("parameters", Nil))
      params.map { p =>
        val pm = asMap(p)
        val td = asMap(pm.getOrElse("typeDescriptions", Map.empty))
        normalizeSolType(td.getOrElse("typeString", "").toString)
      }
    }

    SolFunctionSig(
      name = name,
      inputs = extractParamTypes(paramsNode),
      outputs = extractParamTypes(retsNode),
      stateMutability = stateMutability,
      visibility = visibility
    )
  }

  def extractSingleContractAndFunctions(udfSolPath: String): (String, List[SolFunctionSig]) = {
    val json = runSolcAstJson(udfSolPath)
    val root = parseJson(json)

    val contractNodes = findAllNodes(root, n => n.get("nodeType").contains("ContractDefinition"))
    if (contractNodes.isEmpty) throw new Exception(s"No ContractDefinition found in $udfSolPath")
    val contractName = contractNodes.head.getOrElse("name", "").toString

    val functionNodes = findAllNodes(root, n => n.get("nodeType").contains("FunctionDefinition"))
      // filter out constructors/fallback/receive (name may be empty)
      .filter(n => n.getOrElse("name", "").toString.nonEmpty)

    val funcs = functionNodes.map(extractFunctionSig)
    (contractName, funcs)
  }

  def checkUdfsAgainstUdfSol(program: Program, udfSolPath: String): (String, List[String]) = {
    val (contractName, funcs) = extractSingleContractAndFunctions(udfSolPath)
    val funcIndex: Map[String, List[SolFunctionSig]] = funcs.groupBy(_.name)

    def relationToExpectedSig(rel: Relation): (String, List[String], String) = {
      val name = rel.name
      val types = rel.sig.map(normalizeDatalogType)
      if (types.isEmpty) throw new Exception(s"UDF relation must have at least 1 column: $rel")
      val inTypes = types.dropRight(1)
      val outType = types.last
      (name, inTypes, outType)
    }

    val errors = program.udfs.toList.sorted(Ordering.by[Relation, String](_.name)).flatMap { rel =>
      val (fname, inTypes, outType) = relationToExpectedSig(rel)
      funcIndex.get(fname) match {
        case None =>
          List(s"udf.sol missing function '$fname(${inTypes.mkString(",")}) returns (${outType})'")
        case Some(cands) =>
          val inputMatched = cands.filter(_.inputs == inTypes)
          if (inputMatched.isEmpty) {
            val got = cands.map(s =>
              s"${s.name}(${s.inputs.mkString(",")}) returns (${s.outputs.mkString(",")}) [${s.stateMutability}]"
            ).mkString("; ")
            List(
              s"udf.sol function '$fname' input mismatch. expected (${inTypes.mkString(",")}); got: $got"
            )
          } else {
            val outputMatched = inputMatched.filter(_.outputs == List(outType))
            if (outputMatched.isEmpty) {
              val got = inputMatched.map(s =>
                s"${s.name}(${s.inputs.mkString(",")}) returns (${s.outputs.mkString(",")}) [${s.stateMutability}]"
              ).mkString("; ")
              List(
                s"udf.sol function '$fname' output mismatch (or return arity != 1). expected returns ($outType); got: $got"
              )
            } else {
              val mutabilityMatched = outputMatched.filter(s =>
                s.stateMutability == "pure" || s.stateMutability == "view")
              if (mutabilityMatched.nonEmpty) Nil
              else {
                val gotMutability = outputMatched.map(_.stateMutability).distinct.mkString(",")
                List(
                  s"udf.sol function '$fname' must be pure/view for verifier assumptions; got: $gotMutability"
                )
              }
            }
          }
      }
    }

    (contractName, errors)
  }
}

