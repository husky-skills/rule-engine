package com.example.rule.parser

import com.example.rule.parser.compiler.{ExpressionCompiler, ExpressionParserError, Location}
import com.example.rule.parser.parser._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExpressionCompilerSpec extends AnyFlatSpec with Matchers {

  val validCode =
    """
      |read input name, country
      |switch:
      |  country == "PT" ->
      |    call service "A"
      |    exit
      |  otherwise ->
      |    call service "B"
      |    switch:
      |      name == "unknown" ->
      |        exit
      |      otherwise ->
      |        call service "C"
      |        exit
    """.stripMargin.trim

  val invalidCode =
    """
      |read input name, country
      |switch:
      |  country == PT ->
      |    call service "A"
      |    exit
      |  otherwise ->
      |    call service "B"
      |    switch:
      |      name == "unknown" ->
      |        exit
      |      otherwise ->
      |        call service "C"
      |        exit
    """.stripMargin.trim

  val successfulAST = AndThen(
    ReadInput(List("name", "country")),
    Choice(List(
      IfThen(Equals("country", "PT"), AndThen(CallService("A"), Exit)),
      OtherwiseThen(
        AndThen(
          CallService("B"),
          Choice(List(
            IfThen(Equals("name", "unknown"), Exit),
            OtherwiseThen(AndThen(CallService("C"), Exit))
          ))
        )
      )
    ))
  )

  val errorMsg = ExpressionParserError(Location(3, 14), "string literal expected")


  "Expression compiler" should "successfully parse a valid expression" in {
    ExpressionCompiler(validCode) shouldBe Right(successfulAST)
  }

  it should "return an error with an invalid expression" in {
    ExpressionCompiler(invalidCode) shouldBe Left(errorMsg)
  }

}
