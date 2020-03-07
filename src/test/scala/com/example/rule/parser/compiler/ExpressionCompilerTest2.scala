package com.example.rule.parser.compiler

class ExpressionCompilerTest2 extends org.scalatest.FunSuite {

  val when1 =
    """
      |col1
      |""".stripMargin
  val when2 =
    """
      |col1 cast boolean
      |""".stripMargin
  val when3 =
    """
      |col1 && col2
      |""".stripMargin
  val when4 =
    """
      |col1 between(10,20)
      |""".stripMargin
  val then1 =
    """
      |col1 * (col2 + col3) / col4
      |""".stripMargin
  val then2 =
    """
      |col1 && col2 < 100
      |""".stripMargin
}
