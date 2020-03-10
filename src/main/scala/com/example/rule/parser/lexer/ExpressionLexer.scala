package com.example.rule.parser.lexer

import com.example.rule.parser.compiler.{ExpressionLexerError, Location}

import scala.util.parsing.combinator.RegexParsers

object ExpressionLexer extends RegexParsers {
  override def skipWhitespace = true

  override val whiteSpace = "[ \t\r\f]+".r

  def apply(code: String): Either[ExpressionLexerError, List[ExpressionToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(ExpressionLexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  def tokens: Parser[List[ExpressionToken]] = {
    phrase(rep1(
      //      exit
      //      | readInput | callService
      as | cast | minus | notSymbol |
        binary |
        //        and | or
        between | in | comma | leftPar | rightPar |
        //      | switch | otherwise | colon | arrow
        //      | equals | comma
        literal | identifier
      //        | indentation
    )) ^^ { rawTokens =>
      rawTokens
      //      processIndentations(rawTokens)
    }
  }

  private def processIndentations(tokens: List[ExpressionToken],
                                  indents: List[Int] = List(0)): List[ExpressionToken] = {
    tokens.headOption match {

      // if there is an increase in indentation level, we push this new level into the stack
      // and produce an INDENT
      case Some(INDENTATION(spaces)) if spaces > indents.head =>
        INDENT() :: processIndentations(tokens.tail, spaces :: indents)

      // if there is a decrease, we pop from the stack until we have matched the new level and
      // we produce a DEDENT for each pop
      case Some(INDENTATION(spaces)) if spaces < indents.head =>
        val (dropped, kept) = indents.partition(_ > spaces)
        (dropped map (_ => DEDENT())) ::: processIndentations(tokens.tail, kept)

      // if the indentation level stays unchanged, no tokens are produced
      case Some(INDENTATION(spaces)) if spaces == indents.head =>
        processIndentations(tokens.tail, indents)

      // other tokens are ignored
      case Some(token) =>
        token :: processIndentations(tokens.tail, indents)

      // the final step is to produce a DEDENT for each indentation level still remaining, thus
      // "closing" the remaining open INDENTS
      case None =>
        indents.filter(_ > 0).map(_ => DEDENT())

    }
  }

  def identifier: Parser[IDENTIFIER] = positioned {
    //    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENTIFIER(str) }
    //  todo: remove identifier starting with numbers
    "[a-zA-Z0-9_][a-zA-Z0-9_]*".r ^^ { str => IDENTIFIER(str) }
  }

  def literal: Parser[LITERAL] = positioned {
    """"[^"]*"""".r ^^ { str =>
      val content = str.substring(1, str.length - 1)
      LITERAL(content)
    }
  }

  def indentation: Parser[INDENTATION] = positioned {
    "\n[ ]*".r ^^ { whitespace =>
      val nSpaces = whitespace.length - 1
      INDENTATION(nSpaces)
    }
  }

  def exit = positioned {
    "exit" ^^ (_ => EXIT())
  }

  def readInput = positioned {
    "read input" ^^ (_ => READINPUT())
  }

  def callService = positioned {
    "call service" ^^ (_ => CALLSERVICE())
  }

  def switch = positioned {
    "switch" ^^ (_ => SWITCH())
  }

  def otherwise = positioned {
    "otherwise" ^^ (_ => OTHERWISE())
  }

  def colon = positioned {
    ":" ^^ (_ => COLON())
  }

  def arrow = positioned {
    "->" ^^ (_ => ARROW())
  }

  def equals = positioned {
    "==" ^^ (_ => EQUALS())
  }

  def between = positioned {
    "between" ^^ (_ => BETWEEN())
  }

  def in = positioned {
    "in" ^^ (_ => IN())
  }

  def leftPar = positioned {
    "(" ^^ (_ => LEFTPAR())
  }

  def rightPar = positioned {
    ")" ^^ (_ => RIGHTPAR())
  }

  def comma = positioned {
    "," ^^ (_ => COMMA())
  }

  def as = positioned {
    "as" ^^ (_ => AS())
  }

  def cast = positioned {
    "cast" ^^ (_ => CAST())
  }

  def and = positioned {
    "&&" ^^ (_ => AND())
  }

  def or = positioned {
    "||" ^^ (_ => OR())
  }

  def binary = positioned {
    //https://regex101.com/r/Wwq3e9/4
    """[\+\*\/\%]|(?:\|){2}|(?:\&){2}""".r ^^ (op => BINARY(op))
  }

  def minus = positioned {
    "-" ^^ (_ => MINUS())
  }

  def notSymbol = positioned {
    //https://regex101.com/r/Wwq3e9/5
    """(!)|(not)""".r ^^ (op => NOTSYM(op))
  }
}
