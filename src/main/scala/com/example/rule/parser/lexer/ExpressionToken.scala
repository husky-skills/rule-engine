package com.example.rule.parser.lexer

import scala.util.parsing.input.Positional

sealed trait ExpressionToken extends Positional

case class IDENTIFIER(str: String) extends ExpressionToken

case class LITERAL(str: String) extends ExpressionToken

case class INDENTATION(spaces: Int) extends ExpressionToken

case class EXIT() extends ExpressionToken

case class READINPUT() extends ExpressionToken

case class CALLSERVICE() extends ExpressionToken

case class SWITCH() extends ExpressionToken

case class OTHERWISE() extends ExpressionToken

case class COLON() extends ExpressionToken

case class ARROW() extends ExpressionToken

case class EQUALS() extends ExpressionToken

case class BETWEEN() extends ExpressionToken

case class LEFTPAR() extends ExpressionToken

case class RIGHTPAR() extends ExpressionToken

case class IN() extends ExpressionToken

case class COMMA() extends ExpressionToken

case class INDENT() extends ExpressionToken

case class DEDENT() extends ExpressionToken

case class AS() extends ExpressionToken

case class CAST() extends ExpressionToken

case class AND() extends ExpressionToken

case class OR() extends ExpressionToken

case class BINARY(operator: String) extends ExpressionToken

case class MINUS() extends ExpressionToken

case class NOTSYM(operator: String) extends ExpressionToken

case class NAME() extends ExpressionToken

case class GIVEN() extends ExpressionToken

case class GET() extends ExpressionToken

case class MATCH_CASE() extends ExpressionToken

case class RULES() extends ExpressionToken

case class WHEN() extends ExpressionToken

case class THEN() extends ExpressionToken
