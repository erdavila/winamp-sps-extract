package waspse

import waspse.typeInference.{BooleanType, DoubleType, IntType, Type}

private sealed trait RequiredType {
  def matches(typ: Type): Boolean
}
private case object BooleanRequired extends RequiredType {
  override def matches(typ: Type): Boolean = typ == BooleanType
}
private case object IntRequired extends RequiredType {
  override def matches(typ: Type): Boolean = typ == IntType
}
private case object DoubleRequired extends RequiredType {
  override def matches(typ: Type): Boolean = typ == DoubleType
}
private case object NumericTypeRequired extends RequiredType {
  override def matches(typ: Type): Boolean = typ == IntType || typ == DoubleType
}

private object RequiredType extends (Type => RequiredType) {
  def apply(typ: Type): RequiredType =
    typ match {
      case BooleanType => BooleanRequired
      case IntType => IntRequired
      case DoubleType => NumericTypeRequired
    }
}

class ExpressionTypeAdapter(varsTypes: Map[String, Type]) {
  private def allVarsTypes = varsTypes ++ PredefinedVars.All

  def adaptIn(statement: Statement): Statement =
    adaptInStatement(statement)

  private def adaptInStatement(statement: Statement): Statement =
    statement match {
      case a: Assignment => adaptInAssignment(a)
      case bs: BlockStatement => adaptInBlockStatement(bs)
      case is: IfStatement => adaptInIfStatement(is)
      case ma: MegabufAssignment => adaptInMegabufAssignment(ma)
    }

  private def adaptInAssignment(assignment: Assignment): Assignment = {
    val variable = assignment.variable
    val requiredType = RequiredType(allVarsTypes(variable.id))
    val (adaptedValue, valueType) = adaptExpression(assignment.value)(requiredType)
    assert(requiredType matches valueType)
    Assignment(variable, adaptedValue)
  }

  private def adaptInBlockStatement(blockStatement: BlockStatement): BlockStatement =
    BlockStatement(blockStatement.statements.map(adaptInStatement))

  private def adaptInIfStatement(statement: IfStatement): IfStatement = {
    val (adaptedCondition, BooleanType) = adaptExpression(statement.condition)(BooleanRequired)
    val adaptedIfTrue = adaptInStatement(statement.ifTrue)
    val adaptedIfFalse = statement.ifFalse.map(adaptInStatement)
    IfStatement(adaptedCondition, adaptedIfTrue, adaptedIfFalse)
  }

  private def adaptInMegabufAssignment(assignment: MegabufAssignment): MegabufAssignment = {
    val (adaptedIndex, IntType) = adaptExpression(assignment.index)(IntRequired)
    val (adaptedValue, _) = adaptExpression(assignment.value)(NumericTypeRequired)
    MegabufAssignment(adaptedIndex, adaptedValue)
  }

  private def adaptExpression(expression: Expression): RequiredType => (Expression, Type) =
    expression match {
      case bo: BinaryOperation => adaptBinaryOperation(bo)
      case be: BlockExpression => adaptBlockExpression(be)
      case c: Constant => adaptConstant(c)
      case dl: DoubleLiteral => adaptDoubleLiteral(dl)
      case fc: FunctionCall => adaptFunctionCall(fc)
      case id: Identifier => adaptIdentifier(id)
      case ie: IfExpression => adaptIfExpression(ie)
      case il: IntLiteral => adaptIntLiteral(il)
      case not: Not => adaptNot(not)
    }

  private def adaptBinaryOperation(operation: BinaryOperation) = ensureType {
    operation.operator.op match {
      case "|" | "&" | "%" =>
        val (adaptedL, IntType) = adaptExpression(operation.leftOperand)(IntRequired)
        val (adaptedR, IntType) = adaptExpression(operation.rightOperand)(IntRequired)
        val adaptedOperation = BinaryOperation(adaptedL, operation.operator, adaptedR)
        (adaptedOperation, IntType)
      case "||" =>
        val (adaptedL, BooleanType) = adaptExpression(operation.leftOperand)(BooleanRequired)
        val (adaptedR, BooleanType) = adaptExpression(operation.rightOperand)(BooleanRequired)
        val adaptedOperation = BinaryOperation(adaptedL, operation.operator, adaptedR)
        (adaptedOperation, BooleanType)
      case "==" =>
        operation match {
          case BinaryOperation(id@Identifier(variable), _, i: IntLiteral) =>
            val varType = allVarsTypes(variable)
            val (adaptedLiteral, _) = adaptIntLiteral(i)(RequiredType(varType))
            (BinaryOperation(id, operation.operator, adaptedLiteral), BooleanType)
        }
      case "<" | ">" =>
        val (adaptedL, _) = adaptExpression(operation.leftOperand)(NumericTypeRequired)
        val (adaptedR, _) = adaptExpression(operation.rightOperand)(NumericTypeRequired)
        val adaptedOperation = BinaryOperation(adaptedL, operation.operator, adaptedR)
        (adaptedOperation, BooleanType)
      case "+" | "*" | "-" =>
        val (adaptedL, lType) = adaptExpression(operation.leftOperand)(NumericTypeRequired)
        val (adaptedR, rType) = adaptExpression(operation.rightOperand)(NumericTypeRequired)
        val resultType =
          if (lType == DoubleType || rType == DoubleType) DoubleType
          else IntType
        val adaptedOperation = BinaryOperation(adaptedL, operation.operator, adaptedR)
        (adaptedOperation, resultType)
      case "/" =>
        val (adaptedL, DoubleType) = adaptExpression(operation.leftOperand)(DoubleRequired)
        val (adaptedR, _) = adaptExpression(operation.rightOperand)(NumericTypeRequired)
        val adaptedOperation = BinaryOperation(adaptedL, operation.operator, adaptedR)
        (adaptedOperation, DoubleType)
    }
  }

  private def adaptBlockExpression(blockExpression: BlockExpression)(requiredType: RequiredType) = {
    val adaptedStatements = blockExpression.statements.map(adaptInStatement)
    val (adaptedValue, resultType) = adaptExpression(blockExpression.value)(requiredType)
    (BlockExpression(adaptedStatements, adaptedValue), resultType)
  }

  private def adaptConstant(constant: Constant) = ensureType {
    (constant, DoubleType)
  }

  private def adaptDoubleLiteral(literal: DoubleLiteral)(requiredType: RequiredType) =
    requiredType match {
      case DoubleRequired | NumericTypeRequired => (literal, DoubleType)
    }

  private def adaptFunctionCall(functionCall: FunctionCall) = ensureType {
    val function = Function.ByScalaName(functionCall.function.id)

    assert(functionCall.arguments.length == function.argumentsTypes.length)

    val adaptedArgs =
      for ((arg, requiredType) <- functionCall.arguments zip function.argumentsTypes.map(RequiredType))
      yield {
        val (adaptedArg, argType) = adaptExpression(arg)(requiredType)
        assert(requiredType matches argType)
        adaptedArg
      }

    val adaptedFunctionCall = FunctionCall(functionCall.function, adaptedArgs)
    val resultType = function.returnType
    (adaptedFunctionCall, resultType)
  }

  private def adaptIdentifier(identifier: Identifier) = ensureType {
    val typ = allVarsTypes(identifier.id)
    (identifier, typ)
  }

  private def adaptIfExpression(ifExpression: IfExpression)(requiredType: RequiredType) = {
    val (adaptedCondition, BooleanType) = adaptExpression(ifExpression.condition)(BooleanRequired)
    val (adaptedIfTrue, ifTrueType) = adaptExpression(ifExpression.ifTrue)(requiredType)
    val (adaptedIfFalse, ifFalseType) = adaptExpression(ifExpression.ifFalse)(requiredType)
    val resultType = (ifTrueType, ifFalseType) match {
      case (BooleanType, BooleanType) => BooleanType
      case (IntType, IntType) => IntType
      case _ => DoubleType
    }
    (IfExpression(adaptedCondition, adaptedIfTrue, adaptedIfFalse), resultType)
  }

  private def adaptIntLiteral(literal: IntLiteral)(requiredType: RequiredType) =
    requiredType match {
      case BooleanRequired =>
        val booleanLiteral = if (literal.value != 0) BooleanLiteral(true) else BooleanLiteral(false)
        (booleanLiteral, BooleanType)
      case IntRequired | NumericTypeRequired => (literal, IntType)
      case DoubleRequired => (DoubleLiteral(literal.value), DoubleType)
    }

  private def adaptNot(not: Not) = ensureType {
    val (adaptedValue, BooleanType) = adaptExpression(not.value)(BooleanRequired)
    (Not(adaptedValue), BooleanType)
  }

  private def ensureType(f: => (Expression, Type)): RequiredType => (Expression, Type) =
    requiredType => {
      val (adaptedExpression, resultType) = f
      (requiredType, resultType) match {
        case _ if requiredType matches resultType => (adaptedExpression, resultType)
        case (BooleanRequired, IntType | DoubleType) =>
          val convertedExpression = BinaryOperation(adaptedExpression, Operator("!="), IntLiteral(0))
          (convertedExpression, BooleanType)
        case (IntRequired, DoubleType) =>
          val convertedExpression = MethodCall(adaptedExpression, "toInt")
          (convertedExpression, IntType)
        case (NumericTypeRequired, BooleanType) =>
          val convertedExpression = IfExpression(adaptedExpression, IntLiteral(1), IntLiteral(0))
          (convertedExpression, IntType)
        case (DoubleRequired, IntType) =>
          val convertedExpression = MethodCall(adaptedExpression, "toDouble")
          (convertedExpression, DoubleType)
      }
    }
}
