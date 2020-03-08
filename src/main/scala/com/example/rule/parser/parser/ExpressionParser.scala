package com.example.rule.parser.parser

import com.example.rule.parser.compiler.{ExpressionParserError, Location}
import com.example.rule.parser.lexer._
import com.example.rule.parser.parser

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object ExpressionParser extends Parsers {
  override type Elem = ExpressionToken

  class ExpressionTokenReader(tokens: Seq[ExpressionToken]) extends Reader[ExpressionToken] {
    override def first: ExpressionToken = tokens.head

    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)

    override def rest: Reader[ExpressionToken] = new ExpressionTokenReader(tokens.tail)
  }


  def apply(tokens: Seq[ExpressionToken]): Either[ExpressionParserError, ExpressionAST] = {
    println(s"#### received tokens #####${tokens}")
    val reader: ExpressionTokenReader = new ExpressionTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(ExpressionParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  def program: Parser[ExpressionAST] = positioned {
    phrase(expression)
//    phrase(block)
  }


  def expression: Parser[ExpressionAST] = positioned {
    rep1(or | and | as | cast | column) ^^ { case expList =>
      expList reduceLeft AndThen
    }
  }

  def block: Parser[ExpressionAST] = positioned {
    rep1(statement) ^^ { case stmtList: List[ExpressionAST] =>
      println(stmtList)
      stmtList reduceRight AndThen
    }
  }

  def statement: Parser[ExpressionAST] = positioned {
    val exit = EXIT() ^^ { case _ => Exit }
    val readInput = READINPUT() ~ rep(identifier ~ COMMA()) ~ identifier ^^ {
      case read ~ inputs ~ IDENTIFIER(lastInput) => ReadInput(inputs.map(_._1.str) ++ List(lastInput))
    }
    val callService = CALLSERVICE() ~ literal ^^ {
      case call ~ LITERAL(serviceName) => CallService(serviceName)
    }
    val switch = SWITCH() ~ COLON() ~ INDENT() ~ rep1(ifThen) ~ opt(otherwiseThen) ~ DEDENT() ^^ {
      case _ ~ _ ~ _ ~ ifs ~ otherwise ~ _ => Choice(ifs ++ otherwise)
    }

    or | and | as | column | exit | readInput | callService | switch

  }

  def as: Parser[ExpressionAST] = positioned {
    AS() ~ identifier ^^ {
      case _ ~ IDENTIFIER(name) => ASColumn(name)
    }
  }

  def cast: Parser[ExpressionAST] = positioned {
    CAST() ~ identifier ^^ {
      case _ ~ IDENTIFIER(name) => CASTColumn(name)
    }
  }

  def and: Parser[ExpressionAST] = positioned {
    AND() ~ column ^^ {
      case _ ~ right => ANDColumn(right)
    }
  }

  def or: Parser[ORColumn] = positioned {
    OR() ~ column ^^ {
      case _ ~ right => ORColumn(right)
    }
  }

  def ifThen: Parser[IfThen] = positioned {
    (condition ~ ARROW() ~ INDENT() ~ block ~ DEDENT()) ^^ {
      case cond ~ _ ~ _ ~ block ~ _ => parser.IfThen(cond, block)
    }
  }

  def column: Parser[ExpressionAST] = positioned {
    accept("identifier", {
      case IDENTIFIER(name) => MyColumn(name)
    })
  }

  def otherwiseThen: Parser[OtherwiseThen] = positioned {
    (OTHERWISE() ~ ARROW() ~ INDENT() ~ block ~ DEDENT()) ^^ {
      case _ ~ _ ~ _ ~ block ~ _ => OtherwiseThen(block)
    }
  }

  def condition: Parser[Equals] = positioned {
    (identifier ~ EQUALS() ~ literal) ^^ {
      case IDENTIFIER(id) ~ eq ~ LITERAL(lit) => Equals(id, lit)
    }
  }

  private def identifier: Parser[IDENTIFIER] = positioned {
    accept("identifier", { case id@IDENTIFIER(name) => id })
  }

  private def literal: Parser[LITERAL] = positioned {
    accept("string literal", { case lit@LITERAL(name) => lit })
  }
}
