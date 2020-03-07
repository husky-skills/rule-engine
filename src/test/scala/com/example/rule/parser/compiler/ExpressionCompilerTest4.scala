package com.example.rule.parser.compiler

class ExpressionCompilerTest4 extends org.scalatest.FunSuite {
  val simpleFull =
    """
      |name:rule1
      |given:col1, col2, col3
      |when: col1 && col2
      |then: col3
      |rules:
      | name: rule2
      | then: col3 && col2 || col1
      | name: rule3
      | then: col1 && col3
      |name:rule4
      |then: let(100)
      |get: col1, col2, col3 as boolean, rule2, rule3
      |
      |""".stripMargin

}