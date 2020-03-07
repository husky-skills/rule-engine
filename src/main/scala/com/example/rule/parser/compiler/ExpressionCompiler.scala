package com.example.rule.parser.compiler

import com.example.rule.parser.lexer.ExpressionLexer
import com.example.rule.parser.parser.{ExpressionAST, ExpressionParser}

object ExpressionCompiler {
  def apply(code: String): Either[ExpressionCompilationError, ExpressionAST] = {
    for {
      tokens <- ExpressionLexer(code).right
      ast <- ExpressionParser(tokens).right
    } yield ast
  }
}
