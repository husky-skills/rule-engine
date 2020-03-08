package com.example.rule.parser.compiler

import com.example.rule.parser.parser._
import org.scalatest.matchers.must.Matchers

class ExpressionCompilerColumnTest extends org.scalatest.FunSuite with Matchers {
  //column testing testing
  test("col1 as alias") {
    val testName = "col1 as alias"
    val ast = ExpressionCompiler(testName)
    val expect = AndThen(MyColumn("col1"), ASColumn("alias"))
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }
  test("col1 cast boolean") {
    val testName = "col1 cast boolean"
    val ast = ExpressionCompiler(testName)
    val expect = AndThen(MyColumn("col1"), CASTColumn("boolean"))
  }
  test("let 100 be alias") {
    val testName = "let 100 be alias"
  }

  test("alias be 100") {
    val testName = "alias be 100"
  }

  test("let(0100) as alias cast boolean") {
    val testName = "let(0100) as alias cast boolean"
  }

  //arithmetic and logical operator testing
  test("col1 * col2 + col3") {
    val testName = "col1 * col2 + col3"
  }
  test("col1") {
    val testName = "col1"
    val ast = ExpressionCompiler(testName)
    val expect = MyColumn(testName)
    assert(ast.toOption === Some(expect))
  }
  test("col1 && col2") {
    val testName = "col1 && col2"
    val ast = ExpressionCompiler(testName)
    val expect = AndThen(MyColumn("col1"), ANDColumn(MyColumn("col2")))
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }

  test("col1 || col2") {
    val testName = "col1 || col2"
    val ast = ExpressionCompiler(testName)
    val expect = AndThen(MyColumn("col1"), ORColumn(MyColumn("col2")))
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }
  test("col1 between (10, 20)") {
    val testName = "col1 between (10, 20)"
  }
  test("col1 in (10, 20, 30)") {
    val testName = "col1 in (10, 20, 30)"
  }
  //=============== Combination =================
  test("col1 && col2 || col3") {
    val testName = "col1 && col2 || col3"
    val ast = ExpressionCompiler(testName)
    val expect = AndThen(AndThen(MyColumn("col1"), ANDColumn(MyColumn("col2"))), ORColumn(MyColumn("col3")))
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }
  test("col1 && col2 && col3") {
    val testName = "col1 && col2 && col3"
    val ast = ExpressionCompiler(testName)
    val expect = AndThen(AndThen(MyColumn("col1"), ANDColumn(MyColumn("col2"))), ANDColumn(MyColumn("col3")))
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }
  test("col1 as alias cast boolean && col2") {
    val testName = "col1 as alias cast boolean && col2"
    val ast = ExpressionCompiler(testName)
    val expect = AndThen(AndThen(AndThen(
      MyColumn("col1"),
      ASColumn("alias")),
      CASTColumn("boolean")),
      ANDColumn(MyColumn("col2")))
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }

}
