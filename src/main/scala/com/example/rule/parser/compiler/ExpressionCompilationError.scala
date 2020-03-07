package com.example.rule.parser.compiler

sealed trait ExpressionCompilationError

case class ExpressionLexerError(location: Location, msg: String) extends ExpressionCompilationError

case class ExpressionParserError(location: Location, msg: String) extends ExpressionCompilationError

case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}
