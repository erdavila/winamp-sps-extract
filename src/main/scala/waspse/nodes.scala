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

case class BlockExpression(statements: List[Statement], value: Expression) extends Expression
case class BlockStatement(statements: List[Statement]) extends Statement
case class IfExpression(condition: Expression, ifTrue: Expression, ifFalse: Expression) extends Expression
case class IfStatement(condition: Expression, ifTrue: Statement, ifFalse: Option[Statement]) extends Statement
case class MegabufAssignment(index: Expression, value: Expression) extends Statement
case class Not(value: Expression) extends Expression

case class MethodCall(`object`: Expression, method: String) extends Expression
case class BooleanLiteral(value: Boolean) extends Expression
