package waspse

import scala.util.parsing.input.Position

object Normalizer {

  def normalize(statement: Statement): Statement =
    normalizeStatement(statement)

  private def normalizeStatement(statement: Statement): Statement =
    statement match {
      case a: Assignment => normalizeAssignment(a)
      case b: BlockStatement => normalizeBlockStatement(b)
      case fc: FunctionCall => normalizeFunctionCallAsStatement(fc)
    }

  private def normalizeAssignment(assignment: Assignment): Statement = {
    val value = normalizeExpression(assignment.value)
    assignment.copy(value = value)
  }

  private def normalizeBlockStatement(blockStatement: BlockStatement): BlockStatement = {
    val statements = blockStatement.statements.map(normalizeStatement)
    BlockStatement(statements)
  }

  private def normalizeFunctionCallAsStatement(functionCall: FunctionCall): Statement =
    functionCall match {
      case FunctionCall(Identifier("assign"), List(variable: Identifier, value)) =>
        toAssignment(variable, value)
      case FunctionCall(Identifier("assign"), List(FunctionCall(Identifier("megabuf"), List(index)), value)) =>
        toMegabufAssignment(index, value)
      case FunctionCall(Identifier("exec2"), List(expr1, expr2)) =>
        toBlockStatement(List(normalizeExpressionAsStatement(expr1), normalizeExpressionAsStatement(expr2)))
      case FunctionCall(id@Identifier("if"), List(condition, ifTrue, ifFalse)) =>
        toIfStatement(condition, ifTrue, ifFalse, id.pos)
    }

  private def toAssignment(variable: Identifier, value: Expression): Assignment = {
    val newValue = normalizeExpression(value)
    Assignment(variable, newValue)
  }

  private def toMegabufAssignment(index: Expression, value: Expression): MegabufAssignment = {
    val newIndex = normalizeExpression(index)
    val newValue = normalizeExpression(value)
    MegabufAssignment(newIndex, newValue)
  }

  private def toIfStatement(condition: Expression, ifTrue: Expression, ifFalse: Expression, ifPos: Position): Statement = {
    val newCondition = normalizeExpression(condition)
    val newIfTrue = normalizeExpressionAsStatement(ifTrue)
    val newIfFalse = normalizeExpressionAsStatement(ifFalse)
    (newIfTrue, newIfFalse) match {
      case (BlockStatement(Nil), BlockStatement(Nil)) => BlockStatement(Nil)
      case (BlockStatement(Nil), _) =>
        printLog("Eliminating empty block and inverting if statement", ifPos)
        IfStatement(Not(newCondition), newIfFalse, None)
      case (_, BlockStatement(Nil)) => IfStatement(newCondition, newIfTrue, None)
      case _ => IfStatement(newCondition, newIfTrue, Some(newIfFalse))
    }
  }

  private def normalizeExpression(expression: Expression): Expression =
    expression match {
      case bo: BinaryOperation => normalizeBinaryOperation(bo)
      case c: Constant => normalizeConstant(c)
      case d: DoubleLiteral => d
      case fc: FunctionCall => normalizeFunctionCallAsExpression(fc)
      case id: Identifier => id
      case int: IntLiteral => int
    }

  private def normalizeBinaryOperation(binaryOperation: BinaryOperation): BinaryOperation = {
    val lhs = normalizeExpression(binaryOperation.leftOperand)
    val rhs = normalizeExpression(binaryOperation.rightOperand)
    BinaryOperation(lhs, binaryOperation.operator, rhs)
  }

  private def normalizeExpressionAsStatement(expression: Expression): Statement =
    expression match {
      case bo: BinaryOperation => normalizeBinaryOperationAsStatement(bo)
      case fc: FunctionCall => normalizeFunctionCallAsStatement(fc)
      case int: IntLiteral => normalizeIntLiteralAsStatement(int)
    }

  private def normalizeBinaryOperationAsStatement(binaryOperation: BinaryOperation): Statement = {
    printLog("Dropping operator " + binaryOperation.operator.op, binaryOperation.operator.pos)
    val statement1 = normalizeExpressionAsStatement(binaryOperation.leftOperand)
    val statement2 = normalizeExpressionAsStatement(binaryOperation.rightOperand)
    toBlockStatement(List(statement1, statement2))
  }

  private def normalizeConstant(constant: Constant): Constant = {
    val scalaName = ConstantValue.SPSToScalaName(constant.name)
    Constant(scalaName)
  }

  private def normalizeFunctionCallAsExpression(functionCall: FunctionCall): Expression =
    functionCall match {
      case FunctionCall(Identifier("assign"), List(variable: Identifier, value)) =>
        BlockExpression(List(Assignment(variable, value)), variable)
      case FunctionCall(Identifier("below"), List(leftOperand, rightOperand)) =>
        BinaryOperation(normalizeExpression(leftOperand), Operator("<"), normalizeExpression(rightOperand))
      case FunctionCall(Identifier("bor"), List(leftOperand, rightOperand)) =>
        BinaryOperation(normalizeExpression(leftOperand), Operator("||"), normalizeExpression(rightOperand))
      case FunctionCall(Identifier("above"), List(leftOperand, rightOperand)) =>
        BinaryOperation(normalizeExpression(leftOperand), Operator(">"), normalizeExpression(rightOperand))
      case FunctionCall(Identifier("bnot"), List(value)) =>
        Not(normalizeExpression(value))
      case FunctionCall(Identifier("equal"), List(leftOperand, rightOperand)) =>
        BinaryOperation(normalizeExpression(leftOperand), Operator("=="), normalizeExpression(rightOperand))
      case FunctionCall(Identifier("if"), List(condition, ifTrue, ifFalse)) =>
        IfExpression(normalizeExpression(condition), normalizeExpression(ifTrue), normalizeExpression(ifFalse))
      case FunctionCall(Identifier(name), arguments) =>
        FunctionCall(Identifier(Function.SPSToScalaName(name)), arguments.map(normalizeExpression))
    }

  private def normalizeIntLiteralAsStatement(intLiteral: IntLiteral): Statement = {
    printLog("Dropping unused int literal " + intLiteral.value, intLiteral.pos)
    BlockStatement(Nil)
  }

  private def toBlockStatement(statements: List[Statement]): BlockStatement = {
    val newStatements = statements flatMap {
      case BlockStatement(stmts) => stmts
      case stmt => List(stmt)
    }
    BlockStatement(newStatements)
  }

  private def printLog(message: String, position: Position): Unit =
    println("> " + message + " at " + position)
}
