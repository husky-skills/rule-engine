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
    phrase(rule | expression)
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
      case _ ~ _ ~ _ ~ ifs ~ otherwise ~ _ => ChoiceOld(ifs ++ otherwise)
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
    (opt(NOTSYM("not")) ~ IN() ~ LEFTPAR() ~ rep1sep(column, COMMA())) ~ RIGHTPAR() ^^ {
      case _ ~ _ ~ _ ~ list ~ _ => INColumn(list)
    }
  }

  private def between: Parser[BETWEENColumn] = positioned {
    (opt(NOTSYM("not")) ~ BETWEEN() ~ LEFTPAR() ~ column ~ COMMA() ~ column ~ RIGHTPAR()) ^^ {
      case _ ~ _ ~ _ ~ start ~ _ ~ end ~ _ => BETWEENColumn(start, end)
    }
  }

  private def commaColumn: Parser[ExpressionAST] = positioned {
    COMMA() ~ expr ^^ {
      case _ ~ exp => COMMAColumn(exp)
    }
  }

  private def parentheses: Parser[ExpressionAST] = positioned {
    LEFTPAR() ~ expr ~ RIGHTPAR() ^^ {
      case _ ~ exp ~ _ => exp
    }
  }

  private def binary: Parser[ExpressionAST] = positioned {
    ((BINARY("&&") |
      BINARY("||") |
      BINARY("+") |
      MINUS() |
      BINARY("*") |
      BINARY("/") |
      BINARY("%")
      )
      ~ expr) ^^ {
      case BINARY(op) ~ ex => OPColumn(op, ex)
      case MINUS() ~ ex => OPColumn("-", ex)
    }
  }


  private def colIdentifier: Parser[ExpressionAST] = positioned {
    accept("identifier", {
      case IDENTIFIER(name) => MyColumn(name)
    })
  }

  private def column: Parser[ExpressionAST] = positioned {
    (opt(NOTSYM("!") | MINUS()) ~ colIdentifier) ^^ {
      case Some(NOTSYM(op)) ~ col => OPColumn(op, col)
      case Some(MINUS()) ~ col => OPColumn("-", col)
      case None ~ col => col
    }
  }

  private def arrow: Parser[ExpressionAST] = positioned {
    ARROW() ^^ { case _ => End }
  }

  private def elements: Parser[ExpressionAST] =
    (as | cast | in | between | binary | commaColumn | parentheses) ^^ { case exp => exp }

  private def aggregator: Parser[(ExpressionAST, ExpressionAST) => ExpressionAST] = success(
    (left: ExpressionAST, right: ExpressionAST) => AndThen(left, right)
  )

  private def expr: Parser[ExpressionAST] = chainl1(column | parentheses, elements, aggregator)

  private def rulesss: Parser[ExpressionAST] = chainl1(rule, aggregator)

  private def rule: Parser[ExpressionAST] = positioned {
    def nameClause: Parser[ExpressionAST] =
      (NAME() ~ COLON() ~ identifier) ^^ {
        case _ ~ _ ~ IDENTIFIER(name) => MyColumn(name)
      }

    def givenGetClause =
      opt((GIVEN() | GET()) ~ COLON() ~ expr) ^^ {
        case Some(_ ~ _ ~ exp) => exp
        case None => End
      }

    def whenClause =
      (opt(WHEN() ~ COLON()) ~ expr ~ ARROW() ~ INDENT()) ^^ {
        case _ ~ exp ~ _ ~ _ => exp
      }

    def thenClause =
      (opt(THEN() ~ COLON()) ~ expr ~ DEDENT()) ^^ {
        case _ ~ exp ~ _ => exp
      }

    def otherwiseClause =
      (opt(OTHERWISE() ~ COLON()) ~ expr) ^^ {
        case _ ~ exp => exp
      }

    def caseClause: Parser[Choice] =
      (MATCH_CASE() ~ COLON() ~ INDENT() ~
        rep1(whenClause ~ thenClause) ~
        opt(otherwiseClause)
        ~ DEDENT()
        ) ^^ {
        case _ ~ _ ~ _ ~ cases ~ Some(other) ~ _ => {
          Choice(
            cases.map { case w ~ t => WhenThen(w, t) }
              :+ OtherwiseThen(other))
        }
        case _ ~ _ ~ _ ~ cases ~ None ~ _ => {
          Choice(cases.map { case w ~ t => WhenThen(w, t) })
        }
      }

    //    def nestedRules: Parser[ExpressionAST] =
    //      opt(RULES() ~ COLON() ~ INDENT() ~ rep(rule)) ^^ {
    //        case Some(_ ~ _ ~ _ ~ rules) => (rules :+ End) reduceRight AndThen
    //        case None => End
    //      }

    def nestedRules2: Parser[ExpressionAST] =
      opt(RULES() ~ COLON() ~ INDENT() ~ rulesss) ^^ {
        case Some(_ ~ _ ~ _ ~ rules) => rules
        case None => End
      }

    def first = (nameClause ~ givenGetClause ~ caseClause ~ nestedRules2 ~ givenGetClause) ^^ {
      case name ~ given ~ caseC ~ nested ~ get =>
        Rule(name, caseC, nested, given, get)
    }

    def elems = nestedRules2 ~ givenGetClause

    def concat(): Parser[(Rule, ExpressionAST ~ ExpressionAST) => Rule] = success {
      (half: Rule, secondHalf: ExpressionAST ~ ExpressionAST) =>
        secondHalf match {
          case nested ~ get => half.copy(rules = nested, getClause = get)
        }
    }

    chainl1(first, elems, concat) | success(End)
  }

  private def ifThen: Parser[IfThenOld] = positioned {
    (condition ~ ARROW() ~ INDENT() ~ block ~ DEDENT()) ^^ {
      case cond ~ _ ~ _ ~ block ~ _ => IfThenOld(cond, block)
    }
  }


  private def otherwiseThen: Parser[OtherwiseThenOld] = positioned {
    (OTHERWISE() ~ ARROW() ~ INDENT() ~ block ~ DEDENT()) ^^ {
      case _ ~ _ ~ _ ~ block ~ _ => OtherwiseThenOld(block)
    }
  }

  private def condition: Parser[EqualsOld] = positioned {
    (identifier ~ EQUALS() ~ literal) ^^ {
      case IDENTIFIER(id) ~ eq ~ LITERAL(lit) => EqualsOld(id, lit)
    }
  }

  private def identifier: Parser[IDENTIFIER] = positioned {
    accept("identifier", { case id@IDENTIFIER(name) => id })
  }

  private def literal: Parser[LITERAL] = positioned {
    accept("string literal", { case lit@LITERAL(name) => lit })
  }
}
