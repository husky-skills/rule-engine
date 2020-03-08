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

case class ASColumn(thenBlock: ExpressionAST, alias: String) extends ExpressionAST

case class CASTColumn(left: ExpressionAST, `type`: String) extends ExpressionAST

case class ANDColumn(left: ExpressionAST, right: ExpressionAST) extends ExpressionAST

case class ORColumn(left: ExpressionAST, right: ExpressionAST) extends ExpressionAST

//===============================================


sealed trait ConditionThen extends Positional {
  def thenBlock: ExpressionAST
}

case class IfThen(predicate: Condition, thenBlock: ExpressionAST) extends ConditionThen

case class OtherwiseThen(thenBlock: ExpressionAST) extends ConditionThen

sealed trait Condition extends Positional

case class Equals(factName: String, factValue: String) extends Condition
