package datalog

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import temporal.{TemporalPropertyParser, TemporalExpr, TemporalProperty}

class TemporalPropertyParserSpec extends AnyFlatSpec with Matchers {

  "TemporalPropertyParser" should "parse ALWAYS with comparison" in {
    val input = "ALWAYS (withdrawCount(p, c) IMPLY c <= 1)"
    val result = TemporalPropertyParser.parseFormula(input)
    
    result shouldBe a[Right[_, _]]
    result.right.get shouldBe a[TemporalExpr.Always]
  }

  it should "parse ONCE with function call" in {
    val input = "ONCE withdraw()"
    val result = TemporalPropertyParser.parseFormula(input)
    
    result shouldBe a[Right[_, _]]
    result.right.get shouldBe TemporalExpr.Once(
      TemporalExpr.FunctionCall("withdraw", List())
    )
  }

  it should "parse NOT with AND" in {
    val input = "NOT (ONCE withdraw() AND ONCE refund())"
    val result = TemporalPropertyParser.parseFormula(input)
    
    result shouldBe a[Right[_, _]]
    val expr = result.right.get
    expr shouldBe a[TemporalExpr.Not]
  }

  it should "parse IMPLY correctly" in {
    val input = "closed(true) IMPLY balance(p, n)"
    val result = TemporalPropertyParser.parseFormula(input)
    
    result shouldBe a[Right[_, _]]
    result.right.get shouldBe a[TemporalExpr.Imply]
  }

  it should "parse comparison operators" in {
    val cases = List(
      ("x == 0", classOf[TemporalExpr.Eq]),
      ("x != 0", classOf[TemporalExpr.Neq]),
      ("x < 10", classOf[TemporalExpr.Lt]),
      ("x <= 10", classOf[TemporalExpr.Le]),
      ("x > 0", classOf[TemporalExpr.Gt]),
      ("x >= 0", classOf[TemporalExpr.Ge])
    )
    
    cases.foreach { case (input, expectedClass) =>
      val result = TemporalPropertyParser.parseFormula(input)
      result shouldBe a[Right[_, _]]
      result.right.get should have (
        Symbol("getClass")(expectedClass)
      )
    }
  }

  it should "parse nested temporal operators" in {
    val input = "ALWAYS (ONCE initialize() IMPLY balance(owner) > 0)"
    val result = TemporalPropertyParser.parseFormula(input)
    
    result shouldBe a[Right[_, _]]
    val expr = result.right.get
    expr shouldBe TemporalExpr.Always(
      TemporalExpr.Imply(
        TemporalExpr.Once(TemporalExpr.FunctionCall("initialize", List())),
        TemporalExpr.Gt(
          TemporalExpr.FunctionCall("balance", List(TemporalExpr.Identifier("owner"))),
          TemporalExpr.NumericLiteral(BigInt(0))
        )
      )
    )
  }

  it should "parse file with comments" in {
    val tempFile = java.io.File.createTempFile("test_temporal", ".txt")
    tempFile.deleteOnExit()
    
    val content = """// This is a comment
                    |ALWAYS (x > 0)
                    |
                    |// Another comment
                    |ONCE event()
                    |""".stripMargin
    
    val writer = new java.io.PrintWriter(tempFile)
    writer.write(content)
    writer.close()
    
    val (properties, errors) = TemporalPropertyParser.parseFile(tempFile.getPath)
    
    errors shouldBe empty
    properties should have length 2
    properties(0).comment shouldBe Some("This is a comment")
    properties(1).comment shouldBe Some("Another comment")
  }

  it should "report parse errors with line numbers" in {
    val tempFile = java.io.File.createTempFile("test_temporal_error", ".txt")
    tempFile.deleteOnExit()
    
    val content = """ALWAYS (x > 0)
                    |INVALID SYNTAX HERE
                    |ONCE event()
                    |""".stripMargin
    
    val writer = new java.io.PrintWriter(tempFile)
    writer.write(content)
    writer.close()
    
    val (properties, errors) = TemporalPropertyParser.parseFile(tempFile.getPath)
    
    errors should not be empty
    errors.head._1 shouldBe 2 // Line 2 has error
    properties should have length 2 // Lines 1 and 3 parsed successfully
  }

  it should "handle IMPLIES (alternative syntax)" in {
    val input = "x > 0 IMPLIES y > 0"
    val result = TemporalPropertyParser.parseFormula(input)
    
    result shouldBe a[Right[_, _]]
    result.right.get shouldBe a[TemporalExpr.Imply]
  }

  it should "parse complex nested expression" in {
    val input = "ALWAYS (totalBalance(m) AND raised(r) AND closed(b) AND b == false IMPLY m == r)"
    val result = TemporalPropertyParser.parseFormula(input)
    
    result shouldBe a[Right[_, _]]
    result.right.get shouldBe a[TemporalExpr.Always]
  }
}

