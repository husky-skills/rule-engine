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

//case class ASColumn(column: MyColumn, name: String) extends ExpressionAST

sealed trait ASThen1 extends Positional {
  def thenBlock: ExpressionAST
}

case class ASThen(name:AS1,thenBlock:ExpressionAST) extends ASThen1
case class ASColumn(thenBlock: ExpressionAST, alias: String) extends AS1

//===============================================


sealed trait ConditionThen extends Positional {
  def thenBlock: ExpressionAST
}

case class IfThen(predicate: Condition, thenBlock: ExpressionAST) extends ConditionThen

case class OtherwiseThen(thenBlock: ExpressionAST) extends ConditionThen

sealed trait Condition extends Positional
sealed trait AS1 extends Positional

case class Equals(factName: String, factValue: String) extends Condition
