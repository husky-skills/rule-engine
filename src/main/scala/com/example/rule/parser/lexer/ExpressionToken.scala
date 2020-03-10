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

case class UNARY(operator: String) extends ExpressionToken

