package com.example.rule.parser.parser

import com.example.rule.parser.compiler.{ExpressionParserError, Location}
import com.example.rule.parser.lexer._

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

  def expression: Parser[ExpressionAST] = positioned(expr)

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

  private def as: Parser[ExpressionAST] = positioned {
    AS() ~ (literal | identifier) ^^ {
      case _ ~ IDENTIFIER(name) => ASColumn(name)
      case _ ~ LITERAL(name) => ASColumn(name)
    }
  }

  private def cast: Parser[ExpressionAST] = positioned {
    CAST() ~ identifier ^^ {
      case _ ~ IDENTIFIER(name) => CASTColumn(name)
    }
  }

  private def and: Parser[ExpressionAST] = positioned {
    AND() ~ expr ^^ {
      case _ ~ right => ANDColumn(right)
    }
  }

  private def or: Parser[ORColumn] = positioned {
    OR() ~ expr ^^ {
      case _ ~ right => ORColumn(right)
    }
  }

  private def in: Parser[INColumn] = positioned {
    (IN() ~ LEFTPAR() ~ rep1sep(column, COMMA())) ~ RIGHTPAR() ^^ {
      case _ ~ _ ~ list ~ _ => INColumn(list)
    }
  }

  private def between: Parser[BETWEENColumn] = positioned {
    (BETWEEN() ~ LEFTPAR() ~ column ~ COMMA() ~ column ~ RIGHTPAR()) ^^ {
      case _ ~ _ ~ start ~ _ ~ end ~ _ => BETWEENColumn(start, end)
    }
  }

  private def commaColumn: Parser[ExpressionAST] = positioned {
    COMMA() ~ expr ^^ {
      case _ ~ exp => COMMAColumn(exp)
    }
  }

  private def binary: Parser[ExpressionAST] = positioned {
    ((BINARY("&&") |
      BINARY("||") |
      BINARY("+") |
      BINARY("-") |
      BINARY("*") |
      BINARY("/") |
      BINARY("%")
      )
      ~ expr) ^^ {
      case BINARY(op) ~ ex => BINARYColumn(op, ex)
      case UNARY(op) ~ ex => BINARYColumn(op, ex)
    }
  }


  //  private def unaryPostFix: Parser[ExpressionAST] = positioned {
  //    (expr ~ UNARY("not") ~ expr) ^^ {
  //      case pre ~ UNARY(op) ~ ex => AndThen(UNARYColumn(op, pre), ex)
  //    }
  //  }

  private def ifThen: Parser[IfThen] = positioned {
    (condition ~ ARROW() ~ INDENT() ~ block ~ DEDENT()) ^^ {
      case cond ~ _ ~ _ ~ block ~ _ => IfThen(cond, block)
    }
  }

  private def colIdentifier: Parser[ExpressionAST] = positioned {
    accept("identifier", {
      case IDENTIFIER(name) => MyColumn(name)
    })
  }

  private def column: Parser[ExpressionAST] = positioned {
    (opt(UNARY("!") | BINARY("-")) ~ colIdentifier) ^^ {
      case Some(UNARY(op)) ~ col => UNARYColumn(op, col)
      case Some(BINARY(op)) ~ col => UNARYColumn(op, col)
      case None ~ col => col
    }
  }

  private def elements: Parser[ExpressionAST] =
    (opt(UNARY("!") | BINARY("-"))
      ) ~ (as | cast | in | between | or | and | commaColumn | binary | colIdentifier) ^^ {
      case Some(UNARY(op)) ~ exp => UNARYColumn(op, exp)
      case Some(BINARY(op)) ~ exp => UNARYColumn(op, exp)
      case None ~ exp => exp
    }

  private def aggregator: Parser[(ExpressionAST, ExpressionAST) => ExpressionAST] = success(
    (left: ExpressionAST, right: ExpressionAST) => AndThen(left, right)
  )

  private def expr: Parser[ExpressionAST] = chainl1(column, elements, aggregator)


  private def otherwiseThen: Parser[OtherwiseThen] = positioned {
    (OTHERWISE() ~ ARROW() ~ INDENT() ~ block ~ DEDENT()) ^^ {
      case _ ~ _ ~ _ ~ block ~ _ => OtherwiseThen(block)
    }
  }

  private def condition: Parser[Equals] = positioned {
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
