package com.example.rule.parser.compiler

import com.example.rule.parser.parser._
import com.example.rule.util.config.BetterEither
import org.scalatest.matchers.must.Matchers

class ExpressionCompilerFullSyntaxTest extends org.scalatest.FunSuite with Matchers with BetterEither {
  test("full rule") {
    val rule1 =
      """|name: rule_identity
         |given: col1, col2, col3
         |case:
         | when: col1 =>
         |  then: col1
         | when: col2 =>
         |  then: col2
         | otherwise: col3
         |get: col1, rule_identity
         |""".stripMargin
    val ast = ExpressionCompiler(rule1)
    val expect = Rule(
      name = MyColumn("rule_identity"),
      caseClause = Choice(Seq(
        WhenThen(MyColumn("col1"), MyColumn("col1")),
        WhenThen(MyColumn("col2"), MyColumn("col2")),
        OtherwiseThen(MyColumn("col3"))
      )),
      rules = End,
      givenClause = AndThen(MyColumn("col1"), COMMAColumn(AndThen(MyColumn("col2"), COMMAColumn(MyColumn("col3"))))),
      getClause = AndThen(MyColumn("col1"), COMMAColumn(MyColumn("rule_identity"))
      )
    )
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }
  test("full rule2") {
    val rule1 =
      """|name: rule_identity
         |given: col1, col2, col3
         |case:
         | when: col1 =>
         |  then: col1
         | when: col2 =>
         |  then: col2
         | otherwise: col3
         |get: col1, rule_identity
         |""".stripMargin
    val ast = ExpressionCompiler(rule1)
    val expect = Rule(
      name = MyColumn("rule_identity"),
      caseClause = Choice(Seq(
        WhenThen(MyColumn("col1"), MyColumn("col1")),
        WhenThen(MyColumn("col2"), MyColumn("col2")),
        OtherwiseThen(MyColumn("col3"))
      )),
      rules = End,
      givenClause = AndThen(MyColumn("col1"), COMMAColumn(AndThen(MyColumn("col2"), COMMAColumn(MyColumn("col3"))))),
      getClause = AndThen(MyColumn("col1"), COMMAColumn(MyColumn("rule_identity"))
      )
    )
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }
  test("nested rule") {
    val rule1 =
      """|name: rule_identity
         |given: col1, col2, col3
         |case:
         | when: col1 =>
         |  then: col1
         | when: col2 =>
         |  then: col2
         | otherwise: col3
         |rules:
         |  name: rule_id2
         |  case:
         |    otherwise: col4
         |get: col1, rule_identity
         |""".stripMargin
    val ast = ExpressionCompiler(rule1)
    val expect = Rule(
      name = MyColumn("rule_identity"),
      caseClause = Choice(Seq(
        WhenThen(MyColumn("col1"), MyColumn("col1")),
        WhenThen(MyColumn("col2"), MyColumn("col2")),
        OtherwiseThen(MyColumn("col3"))
      )),
      rules = AndThen(Rule(
        name = MyColumn("rule_id2"),
        caseClause = Choice(List(OtherwiseThen(MyColumn("col4")))),
        rules = End,
        givenClause = End,
        getClause = End
      ), End),
      givenClause = AndThen(MyColumn("col1"), COMMAColumn(AndThen(MyColumn("col2"), COMMAColumn(MyColumn("col3"))))),
      getClause = AndThen(MyColumn("col1"), COMMAColumn(MyColumn("rule_identity"))
      )
    )
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }
  test("optional keywords") {
    val rule1 =
      """|name: rule_identity
         |given: col1, col2, col3
         |case:
         | col1 =>
         |  col1
         | col2 =>
         |  col2
         | col3
         |get: col1, rule_identity
         |""".stripMargin
    val ast = ExpressionCompiler(rule1)
    val expect = Rule(
      name = MyColumn("rule_identity"),
      caseClause = Choice(Seq(
        WhenThen(MyColumn("col1"), MyColumn("col1")),
        WhenThen(MyColumn("col2"), MyColumn("col2")),
        OtherwiseThen(MyColumn("col3"))
      )),
      rules = End,
      givenClause = AndThen(MyColumn("col1"), COMMAColumn(AndThen(MyColumn("col2"), COMMAColumn(MyColumn("col3"))))),
      getClause = AndThen(MyColumn("col1"), COMMAColumn(MyColumn("rule_identity"))
      )
    )
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }
  test("optional keywords nested rule") {
    val rule1 =
      """|name: rule_identity
         |given: col1, col2, col3
         |case:
         | col1 =>
         |  col1
         | col2 =>
         |  col2
         | col3
         |rules:
         |  name: rule_id2
         |  case:
         |    col4
         |get: col1, rule_identity
         |""".stripMargin
    val ast = ExpressionCompiler(rule1)
    val expect = Rule(
      name = MyColumn("rule_identity"),
      caseClause = Choice(Seq(
        WhenThen(MyColumn("col1"), MyColumn("col1")),
        WhenThen(MyColumn("col2"), MyColumn("col2")),
        OtherwiseThen(MyColumn("col3"))
      )),
      rules = AndThen(Rule(
        name = MyColumn("rule_id2"),
        caseClause = Choice(List(OtherwiseThen(MyColumn("col4")))),
        rules = End,
        givenClause = End,
        getClause = End
      ), End),
      givenClause = AndThen(MyColumn("col1"), COMMAColumn(AndThen(MyColumn("col2"), COMMAColumn(MyColumn("col3"))))),
      getClause = AndThen(MyColumn("col1"), COMMAColumn(MyColumn("rule_identity"))
      )
    )
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }
}
