package com.example.rule.parser.parser

import scala.util.parsing.input.Positional

sealed trait ExpressionAST extends Positional

case class AndThen(step1: ExpressionAST, step2: ExpressionAST) extends ExpressionAST

case class ReadInput(inputs: Seq[String]) extends ExpressionAST

case class CallService(serviceName: String) extends ExpressionAST

case class Choice(alternatives: Seq[ConditionThen]) extends ExpressionAST

case object Exit extends ExpressionAST

//===============================================

case class MyColumn(name: String) extends ExpressionAST

case class ASColumn(alias: String) extends ExpressionAST

case class CASTColumn(`type`: String) extends ExpressionAST

case class ANDColumn(right: ExpressionAST) extends ExpressionAST

case class ORColumn(right: ExpressionAST) extends ExpressionAST

case class BETWEENColumn(start: ExpressionAST, end: ExpressionAST) extends ExpressionAST

case class INColumn(items: List[ExpressionAST]) extends ExpressionAST

case class COMMAColumn(right: ExpressionAST) extends ExpressionAST

//===============================================


sealed trait ConditionThen extends Positional {
  def thenBlock: ExpressionAST
}

case class IfThen(predicate: Condition, thenBlock: ExpressionAST) extends ConditionThen

case class OtherwiseThen(thenBlock: ExpressionAST) extends ConditionThen

sealed trait Condition extends Positional

case class Equals(factName: String, factValue: String) extends Condition
