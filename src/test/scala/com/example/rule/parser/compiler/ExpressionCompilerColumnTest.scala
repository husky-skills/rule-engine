package com.example.rule.parser.compiler

import com.example.rule.parser.parser.{ANDColumn, ASColumn, CASTColumn, MyColumn, ORColumn}
import org.scalatest.matchers.must.Matchers

class ExpressionCompilerColumnTest extends org.scalatest.FunSuite with Matchers {
  //column testing testing
  test("col1 as alias") {
    val testName = "col1 as alias"
    val ast = ExpressionCompiler(testName)
    val expect = ASColumn(MyColumn("col1"), "alias")
    assert(ast.right.get === expect)
  }
  test("col1 cast boolean") {
    val testName = "col1 cast boolean"
    val ast = ExpressionCompiler(testName)
    val expect = CASTColumn(MyColumn("col1"), "boolean")
  }
  test("let(100) as alias") {
    val testName = "let(100) as alias"
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
    assert(ast.right.get === expect)
  }
  test("col1 && col2") {
    val testName = "col1 && col2"
    val ast = ExpressionCompiler(testName)
    val expect = ANDColumn(MyColumn("col1"), MyColumn("col2"))
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }

  test("col1 || col2") {
    val testName = "col1 || col2"
    val ast = ExpressionCompiler(testName)
    val expect = ORColumn(MyColumn("col1"), MyColumn("col2"))
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
    val expect = ORColumn(ANDColumn(MyColumn("col1"), MyColumn("col2")), MyColumn("col3"))
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }
  test("col1 && col2 && col3") {
    val testName = "col1 && col2 && col3"
    val ast = ExpressionCompiler(testName)
    val expect = ANDColumn(ANDColumn(MyColumn("col1"), MyColumn("col2")), MyColumn("col3"))
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }
  test("col1 as alias cast boolean && col2") {
    val testName = "col1 as alias cast boolean && col2"
    val ast = ExpressionCompiler(testName)
    val expect = ORColumn(MyColumn("col1"), MyColumn("col2"))
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }

}
