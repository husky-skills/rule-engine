package com.example.rule.parser.compiler

import com.example.rule.parser.parser._
import org.scalatest.matchers.must.Matchers

class ExpressionCompilerColumnArithmeticTest extends org.scalatest.FunSuite with Matchers {
  test("col1 + col2 - col3") {
    val testName = "col1 + col2 - col3"
    val ast = ExpressionCompiler(testName)
    val expect =
      AndThen(
        MyColumn("col1"),
        OPColumn("+",
          AndThen(MyColumn("col2"),
            OPColumn("-", MyColumn("col3")))
        ))
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }

  test("-col1 + col2") {
    val testName = "-col1 + col2"
    val ast = ExpressionCompiler(testName)
    val expect =
      AndThen(
        OPColumn("-", MyColumn("col1")),
        OPColumn("+", MyColumn("col2"))
      )
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }
  test("col1 + (col2 - col3)") {
    val testName = "col1 + (col2 - col3)"
    val ast = ExpressionCompiler(testName)
    val expect =
      AndThen(
        MyColumn("col1"),
        OPColumn("+",
          AndThen(MyColumn("col2"),
            OPColumn("-", MyColumn("col3")))
        ))
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }
  test("col1 * (col2 - col3)") {
    val testName = "col1 * (col2 - col3)"
    val ast = ExpressionCompiler(testName)
    val expect =
      AndThen(
        MyColumn("col1"),
        OPColumn("*",
          AndThen(MyColumn("col2"),
            OPColumn("-", MyColumn("col3")))
        ))
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }
  test("(col1 + col2) * col3") {
    val testName = "(col1 + col2) * col3"
    val ast = ExpressionCompiler(testName)
    val expect =
      AndThen(AndThen(MyColumn("col1"),
        OPColumn("+", MyColumn("col2"))),
        OPColumn("*", MyColumn("col3")))
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }
}
