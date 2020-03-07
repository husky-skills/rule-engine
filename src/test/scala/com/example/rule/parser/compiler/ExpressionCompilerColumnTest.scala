package com.example.rule.parser.compiler

class ExpressionCompilerColumnTest extends org.scalatest.FunSuite {
  //column testing testing
  test("col1 as alias") {
    val testName = "col1 as alias"
  }
  test("col1 cast boolean") {
    val testName = "col1 cast boolean"
  }
  test("let(100) as alias") {
    val testName = "let(100) as alias"
  }

  //arithmetic and logical operator testing
  test("col1 * col2 + col3") {
    val testName = "col1 * col2 + col3"
  }
  test("col1") {
    val testName = "col1"
  }
  test("col1 && col2") {
    val testName = "col1 && col2"
  }
  test("col1 between (10, 20)") {
    val testName = "col1 between (10, 20)"
  }
  test("col1 in (10, 20, 30)") {
    val testName = "col1 in (10, 20, 30)"
  }

}
