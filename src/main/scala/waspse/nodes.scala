package waspse

import scala.util.parsing.input.Positional

sealed trait Statement
sealed trait Expression

case class Assignment(variable: Identifier, value: Expression) extends Statement

sealed trait Number extends Expression with Positional
case class DoubleLiteral(value: Double) extends Number
case class IntLiteral(value: Int) extends Number

case class Identifier(id: String) extends Expression with Positional
case class Constant(name: String) extends Expression with Positional
case class FunctionCall(function: Identifier, arguments: List[Expression]) extends Statement with Expression
case class Operator(op: String) extends Positional
case class BinaryOperation(leftOperand: Expression, operator: Operator, rightOperand: Expression) extends Expression
