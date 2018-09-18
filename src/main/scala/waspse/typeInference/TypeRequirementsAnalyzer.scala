package waspse.typeInference

import waspse.{Assignment, BinaryOperation, BlockExpression, BlockStatement, Constant, DoubleLiteral, Expression, Function, FunctionCall, Identifier, IfExpression, IfStatement, IntLiteral, MegabufAssignment, Not, PredefinedVars, Statement, typeInference}

object TypeRequirementsAnalyzer {

  private type TypeReqsMap = Map[String, TypeRequirements]

  def analyzeIn(methods: Seq[Statement]): TypeReqsMap =
    methods.map(analyzeStatement).reduce(_ +++ _) -- PredefinedVars.List.keys

  private def analyzeStatement(statement: Statement): TypeReqsMap =
    statement match {
      case a: Assignment => analyzeAssignment(a)
      case bs: BlockStatement => analyzeBlockStatement(bs)
      case is: IfStatement => analyzeIfStatement(is)
      case ma: MegabufAssignment => analyzeMegabufAssignment(ma)
    }

  private def analyzeAssignment(assignment: Assignment): TypeReqsMap = {
    val variable = assignment.variable.id
    val (analyzedValue, typeReqs) = analyzeExpression(assignment.value)
    analyzedValue +++ toMap(variable, typeReqs)
  }

  private def analyzeBlockStatement(blockStatement: BlockStatement): TypeReqsMap =
    blockStatement.statements.map(analyzeStatement).reduceOption(_ +++ _).getOrElse(Map.empty)

  private def analyzeIfStatement(ifStatement: IfStatement): TypeReqsMap = {
    val (analyzedCondition, _) = analyzeExpression(ifStatement.condition)
    val analyzedIfTrue = analyzeStatement(ifStatement.ifTrue)
    val analyzedIfFalse = ifStatement.ifFalse.map(analyzeStatement).getOrElse(Map.empty)
    analyzedCondition +++ analyzedIfTrue +++ analyzedIfFalse
  }

  private def analyzeMegabufAssignment(megabufAssignment: MegabufAssignment): TypeReqsMap = {
    val (analyzedIndex, _) = analyzeExpression(megabufAssignment.index)
    val (analyzedValue, _) = analyzeExpression(megabufAssignment.value)
    analyzedIndex +++ analyzedValue
  }

  private def analyzeExpression(expression: Expression): (TypeReqsMap, TypeRequirements) =
    expression match {
      case bo: BinaryOperation => analyzeBinaryOperation(bo)
      case be: BlockExpression => analyzeBlockExpression(be)
      case c: Constant => analyzeConstant(c)
      case f: DoubleLiteral => analyzeDoubleLiteral(f)
      case fc: FunctionCall => analyzeFunctionCall(fc)
      case id: Identifier => analyzeIdentifier(id)
      case ie: IfExpression => analyzeIfExpression(ie)
      case i: IntLiteral => analyzeIntLiteral(i)
      case not: Not => analyzeNot(not)
    }

  private def analyzeBinaryOperation(binaryOperation: BinaryOperation): (TypeReqsMap, TypeRequirements) = {
    def operandsIndependentTypeReqs(minimumLeveledType: LeveledType): (TypeReqsMap, TypeRequirements) = {
      val (analyzedLeft, _) = analyzeExpression(binaryOperation.leftOperand)
      val (analyzedRight, _) = analyzeExpression(binaryOperation.rightOperand)
      (analyzedLeft +++ analyzedRight, minimumLeveledType)
    }

    binaryOperation.operator.op match {
      case "|" | "&" | "%" => operandsIndependentTypeReqs(IntType)
      case "||" => operandsIndependentTypeReqs(BooleanType)
      case "==" => operandsIndependentTypeReqs(BooleanType)
      case "+" | "-" | "*" =>
        val (lAnalyzed, lTypeReqs) = analyzeExpression(binaryOperation.leftOperand)
        val (rAnalyzed, rTypeReqs) = analyzeExpression(binaryOperation.rightOperand)
        val analyzed = lAnalyzed +++ rAnalyzed
        val typeReqs = lTypeReqs +++ rTypeReqs +++ IntType
        (analyzed, typeReqs)
      case "<" | ">" => operandsIndependentTypeReqs(BooleanType)
      case "/" => operandsIndependentTypeReqs(DoubleType)
    }
  }

  private def analyzeBlockExpression(blockExpression: BlockExpression): (TypeReqsMap, TypeRequirements) = {
    val analyzedStatements = blockExpression.statements.map(analyzeStatement).reduceOption(_ +++ _).getOrElse(Map.empty)
    val (analyzedValue, valueTypeReqs) = analyzeExpression(blockExpression.value)
    (analyzedStatements +++ analyzedValue, valueTypeReqs)
  }

  private def analyzeConstant(constant: Constant): (TypeReqsMap, TypeRequirements) =
    (Map.empty, DoubleType)

  private def analyzeDoubleLiteral(doubleLiteral: DoubleLiteral): (TypeReqsMap, TypeRequirements) =
    (Map.empty, DoubleType)

  private def analyzeFunctionCall(functionCall: FunctionCall): (TypeReqsMap, TypeRequirements) = {
    val function = Function.ByScalaName(functionCall.function.id)
    val analyzedVars = functionCall.arguments.map { arg =>
      val (analyzedVars, _) = analyzeExpression(arg)
      analyzedVars
    }.reduce(_ +++ _)
    (analyzedVars, function.returnType)
  }

  private def analyzeIdentifier(identifier: Identifier): (TypeReqsMap, TypeRequirements) = {
    val variable = identifier.id
    (Map.empty, variable)
  }

  private def analyzeIfExpression(ifExpression: IfExpression): (TypeReqsMap, TypeRequirements) = {
    val (analyzedCondition, _) = analyzeExpression(ifExpression.condition)
    val (analyzedIfTrue, ifTrueTypeReqs) = analyzeExpression(ifExpression.ifTrue)
    val (analyzedIfFalse, ifFalseTypeReqs) = analyzeExpression(ifExpression.ifFalse)
    val typeReqs = ifTrueTypeReqs +++ ifFalseTypeReqs
    (analyzedCondition +++ analyzedIfTrue +++ analyzedIfFalse, typeReqs)
  }

  private def analyzeIntLiteral(intLiteral: IntLiteral): (TypeReqsMap, TypeRequirements) = {
    val minimumLeveledType = intLiteral.value match {
      case 0 | 1 => BooleanOrIntType
      case _ => IntType
    }
    (Map.empty, minimumLeveledType)
  }

  private def analyzeNot(not: Not): (TypeReqsMap, TypeRequirements) = {
    val (analyzedNot, _) = analyzeExpression(not.value)
    (analyzedNot, BooleanType)
  }

  private def toMap(variable: String, typeRequirements: TypeRequirements): TypeReqsMap =
    Map(variable -> typeRequirements)

  private implicit class TypeReqsMapOps(val a: TypeReqsMap) extends AnyVal {
    def +++(b: TypeReqsMap): TypeReqsMap =
      (a.keySet ++ b.keySet).map { variable =>
        val aTypeReqs = a.getOrElse(variable, TypeRequirements())
        val bTypeReqs = b.getOrElse(variable, TypeRequirements())
        variable -> (aTypeReqs +++ bTypeReqs)
      }.toMap
  }
}
