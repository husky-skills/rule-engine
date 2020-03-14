package com.example.rule.parser

import com.example.rule.parser.compiler.{ExpressionCompiler, ExpressionParserError, Location}
import com.example.rule.parser.parser._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExpressionCompilerSpec extends AnyFlatSpec with Matchers {
  pending

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
      |       exit
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
    ChoiceOld(List(
      IfThenOld(EqualsOld("country", "PT"), AndThen(CallService("A"), Exit)),
      OtherwiseThenOld(
        AndThen(
          CallService("B"),
          ChoiceOld(List(
            IfThenOld(EqualsOld("name", "unknown"), Exit),
            OtherwiseThenOld(AndThen(CallService("C"), Exit))
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

  it should "simple column" in {
    ExpressionCompiler("deposit_col") shouldBe Right(MyColumn("deposit_col"))
  }


  it should "column as alias" in {
    ExpressionCompiler("deposit_col as alias_name") shouldBe
      Right(AndThen(MyColumn("deposit_col"), ASColumn("alias_name")))
  }
}
