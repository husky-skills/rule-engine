package com.example.rule.util.config

import com.example.rule.parser.compiler.ExpressionCompilationError
import com.example.rule.parser.parser.ExpressionAST

trait BetterEither {

  case class BetterEitherImpl[A, B](arg: Either[A, B]) {
    implicit def toOption() = arg match {
      case Right(resp) => Some(resp)
      case _ => None
    }
  }

  implicit def toBetterEither(arg: Either[ExpressionCompilationError, ExpressionAST]): BetterEitherImpl[ExpressionCompilationError, ExpressionAST]
  = BetterEitherImpl(arg)
}