package com.example.rule.parser.compiler

import java.sql.Timestamp
import java.text.SimpleDateFormat

import com.example.rule.parser.parser._
import org.scalatest.matchers.must.Matchers

class ExpressionCompilerColumnLetTest extends org.scalatest.FunSuite with Matchers {
  //ref:   https://henkelmann.eu/2011/01/an-introduction-to-scala-parser-combinators---part-2-parsing-literal-expressions/
  test("let col1 be 100") {
    val testName = "let col1 be 100"
    val ast = ExpressionCompiler(testName)
    val expect = LetMyColumn("col1", 100)
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }

  test("let col1 be col1 * col2") {
    val testName = "let col1 be col1 * col2"
    val ast = ExpressionCompiler(testName)
    val expect = LetMyColumn("col1",
      AndThen(MyColumn("col1"), OPColumn("*", MyColumn("col2")))
    )
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }

  test("let col1 be 20.20") {
    val testName = "let col1 be 20.20"
    val ast = ExpressionCompiler(testName)
    val expect = LetMyColumn("col1", 20.20)
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }
  test("""let col1 be "mahi"""") {
    val testName = """let col1 be "mahi""""
    val ast = ExpressionCompiler(testName)
    val expect = LetMyColumn("col1", "mahi")
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }
  test("""let col1 be 'mahi'""") {
    val testName = """let col1 be 'mahi'"""
    val ast = ExpressionCompiler(testName)
    val expect = LetMyColumn("col1", "mahi")
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }

  test("""let mahendra be  true """) {
    val testName = """let mahendra be  true """
    val ast = ExpressionCompiler(testName)
    val expect = LetMyColumn("mahendra", true)
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }
  test("""let kiran be  false """) {
    val testName = """let kiran be  false """
    val ast = ExpressionCompiler(testName)
    val expect = LetMyColumn("kiran", false)
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }
  test("""let col1 be  col2 """) {
    val testName = """let col1 be  col2 """
    val ast = ExpressionCompiler(testName)
    val expect = LetMyColumn("col1", MyColumn("col2"))
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }

  test("""let dateCol1 be 20/12/2020""") {
    val date = "20/12/2020"
    val testName = s"""let dateCol1 be ${date}"""
    val ast = ExpressionCompiler(testName)
    val dateFormat = new SimpleDateFormat("dd/MM/yyyy")
    val expect = LetMyColumn("dateCol1", new Timestamp(dateFormat.parse(date).getTime))
    if (ast.isLeft) println(ast.left.get.toString)
    assert(ast.toOption === Some(expect))
  }
}
