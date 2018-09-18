package waspse

import scala.util.parsing.input.Position

object Transformer {

  private val FunctionNameTranslation = Map(
    "cos" -> "math.cos",
    "max" -> "math.max",
    "megabuf" -> "megabuf",
    "min" -> "math.min",
    "sin" -> "math.sin",
  )

  def transform(statement: Statement): Statement =
    transformStatement(statement)

  private def transformStatement(statement: Statement): Statement =
    statement match {
      case a: Assignment => transformAssignment(a)
      case b: BlockStatement => transformBlockStatement(b)
      case fc: FunctionCall => transformFunctionCallToStatement(fc)
    }

  private def transformAssignment(assignment: Assignment): Statement = {
    val value = transformExpression(assignment.value)
    assignment.copy(value = value)
  }

  private def transformBlockStatement(blockStatement: BlockStatement): BlockStatement = {
    val statements = blockStatement.statements.map(transformStatement)
    BlockStatement(statements)
  }

  private def transformFunctionCallToStatement(functionCall: FunctionCall): Statement =
    functionCall match {
      case FunctionCall(Identifier("assign"), List(variable: Identifier, value)) =>
        toAssignment(variable, value)
      case FunctionCall(Identifier("assign"), List(FunctionCall(Identifier("megabuf"), List(index)), value)) =>
        toMegabufAssignment(index, value)
      case FunctionCall(Identifier("exec2"), List(expr1, expr2)) =>
        toBlockStatement(List(transformExpressionToStatement(expr1), transformExpressionToStatement(expr2)))
      case FunctionCall(id@Identifier("if"), List(condition, ifTrue, ifFalse)) =>
        toIfStatement(condition, ifTrue, ifFalse, id.pos)
    }

  private def toAssignment(variable: Identifier, value: Expression): Assignment = {
    val newValue = transformExpression(value)
    Assignment(variable, newValue)
  }

  private def toMegabufAssignment(index: Expression, value: Expression): MegabufAssignment = {
    val newIndex = transformExpression(index)
    val newValue = transformExpression(value)
    MegabufAssignment(newIndex, newValue)
  }

  private def toIfStatement(condition: Expression, ifTrue: Expression, ifFalse: Expression, ifPos: Position): Statement = {
    val newCondition = transformExpression(condition)
    val newIfTrue = transformExpressionToStatement(ifTrue)
    val newIfFalse = transformExpressionToStatement(ifFalse)
    (newIfTrue, newIfFalse) match {
      case (BlockStatement(Nil), BlockStatement(Nil)) => BlockStatement(Nil)
      case (BlockStatement(Nil), _) =>
        printLog("Eliminating empty block and inverting if statement", ifPos)
        IfStatement(Not(newCondition), newIfFalse, None)
      case (_, BlockStatement(Nil)) => IfStatement(newCondition, newIfTrue, None)
      case _ => IfStatement(newCondition, newIfTrue, Some(newIfFalse))
    }
  }

  private def transformExpression(expression: Expression): Expression =
    expression match {
      case bo: BinaryOperation => transformBinaryOperation(bo)
      case c: Constant => c
      case d: DoubleLiteral => d
      case fc: FunctionCall => transformFunctionCallToExpression(fc)
      case id: Identifier => id
      case int: IntLiteral => int
    }

  private def transformBinaryOperation(binaryOperation: BinaryOperation): BinaryOperation = {
    val lhs = transformExpression(binaryOperation.leftOperand)
    val rhs = transformExpression(binaryOperation.rightOperand)
    BinaryOperation(lhs, binaryOperation.operator, rhs)
  }

  private def transformExpressionToStatement(expression: Expression): Statement =
    expression match {
      case bo: BinaryOperation => transformBinaryOperationToStatement(bo)
      case fc: FunctionCall => transformFunctionCallToStatement(fc)
      case int: IntLiteral => transformIntLiteralToStatement(int)
    }

  private def transformBinaryOperationToStatement(binaryOperation: BinaryOperation): Statement = {
    printLog("Dropping operator " + binaryOperation.operator.op, binaryOperation.operator.pos)
    val statement1 = transformExpressionToStatement(binaryOperation.leftOperand)
    val statement2 = transformExpressionToStatement(binaryOperation.rightOperand)
    toBlockStatement(List(statement1, statement2))
  }

  private def transformFunctionCallToExpression(functionCall: FunctionCall): Expression =
    functionCall match {
      case FunctionCall(Identifier("assign"), List(variable: Identifier, value)) =>
        BlockExpression(List(Assignment(variable, value)), variable)
      case FunctionCall(Identifier("below"), List(leftOperand, rightOperand)) =>
        BinaryOperation(transformExpression(leftOperand), Operator("<"), transformExpression(rightOperand))
      case FunctionCall(Identifier("bor"), List(leftOperand, rightOperand)) =>
        BinaryOperation(transformExpression(leftOperand), Operator("||"), transformExpression(rightOperand))
      case FunctionCall(Identifier("above"), List(leftOperand, rightOperand)) =>
        BinaryOperation(transformExpression(leftOperand), Operator(">"), transformExpression(rightOperand))
      case FunctionCall(Identifier("bnot"), List(value)) =>
        Not(transformExpression(value))
      case FunctionCall(Identifier("equal"), List(leftOperand, rightOperand)) =>
        BinaryOperation(transformExpression(leftOperand), Operator("=="), transformExpression(rightOperand))
      case FunctionCall(Identifier("if"), List(condition, ifTrue, ifFalse)) =>
        IfExpression(transformExpression(condition), transformExpression(ifTrue), transformExpression(ifFalse))
      case FunctionCall(Identifier(name), arguments) if FunctionNameTranslation.contains(name) =>
        FunctionCall(Identifier(FunctionNameTranslation(name)), arguments.map(transformExpression))
    }

  private def transformIntLiteralToStatement(intLiteral: IntLiteral): Statement = {
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
