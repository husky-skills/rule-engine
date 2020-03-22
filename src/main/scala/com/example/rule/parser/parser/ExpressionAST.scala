package com.example.rule.parser.parser

import scala.collection.immutable.List
import scala.util.parsing.input.Positional

sealed trait ExpressionAST extends Positional

case class AndThen(step1: ExpressionAST, step2: ExpressionAST) extends ExpressionAST

case class ReadInput(inputs: Seq[String]) extends ExpressionAST

case class CallService(serviceName: String) extends ExpressionAST

case class ChoiceOld(alternatives: Seq[ConditionThenOld]) extends ExpressionAST

case object Exit extends ExpressionAST

//===============================================

case class MyColumn(name: String) extends ExpressionAST

case class LetMyColumn(name: String, value: Any) extends ExpressionAST

case class LitMyColumn(value: Any) extends ExpressionAST

case class ASColumn(alias: String) extends ExpressionAST

case class CASTColumn(`type`: String) extends ExpressionAST

case class OPColumn(operator: String, col: ExpressionAST) extends ExpressionAST

case class OPColumnAndArgs(operator: String, args: List[Any], col: ExpressionAST) extends ExpressionAST

case class BETWEENColumn(start: ExpressionAST, end: ExpressionAST) extends ExpressionAST

case class INColumn(items: List[ExpressionAST]) extends ExpressionAST

case class COMMAColumn(col: ExpressionAST) extends ExpressionAST

case class ANDColumn(col: ExpressionAST) extends ExpressionAST

case class ORColumn(col: ExpressionAST) extends ExpressionAST

//===============================================

sealed trait ConditionThenOld extends Positional {
  def thenBlock: ExpressionAST
}

case class IfThenOld(predicate: ConditionOld, thenBlock: ExpressionAST) extends ConditionThenOld

case class OtherwiseThenOld(thenBlock: ExpressionAST) extends ConditionThenOld

sealed trait ConditionOld extends Positional

case class EqualsOld(factName: String, factValue: String) extends ConditionOld

//===============================================


case class Rule(
                 name: ExpressionAST,
                 caseClause: Choice,
                 rules: ExpressionAST,
                 givenClause: ExpressionAST,
                 getClause: ExpressionAST
               ) extends ExpressionAST

object End extends ExpressionAST

case class Choice(alternatives: Seq[ConditionThen]) extends ExpressionAST

sealed trait ConditionThen extends Positional {
  def thenBlock: ExpressionAST
}

case class WhenThen(predicate: ExpressionAST, thenBlock: ExpressionAST) extends ConditionThen

case class OtherwiseThen(thenBlock: ExpressionAST) extends ConditionThen

case class GroupBy(grouped: List[GroupExpr], by: ExpressionAST) extends ExpressionAST

case class GroupExpr(fun: String, expressionAST: ExpressionAST)
