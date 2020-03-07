package com.example.rule.parser.compiler

class ExpressionCompilerTest3 extends org.scalatest.FunSuite {
val full =
  """
    |name: rule1
    |given: col1 cast boolean, col2 cast int
    |match:
    | when: col1 && col2
    |   then: col1 * col2 + let(100)
    | when: col1 + col2
    |   then: col2
    | otherwise: col1 + let(100)
    |rules:
    | name:rule2
    | given:
    | match:
    |   when:
    |     then:
    |   otherwise:
    | name:rule3
    | given:
    | match:
    |   when:
    |     then:
    |   when:
    |     then:
    |   otherwise:
    | name:resultRule
    | then: rule1 * rule2
    |
    |""".stripMargin
}
