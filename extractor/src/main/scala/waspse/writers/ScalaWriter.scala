package waspse.writers

import waspse._
import waspse.typeInference.{BooleanType, DoubleType, IntType, Type}

object ScalaWriter extends Writer {

  private val MethodsAndArgType = Seq(
    ("initialize", "InitializeContext"),
    ("onSample", "SampleContext"),
    ("onSliderChange", "SliderChangeContext"),
  )

  private val ContextArg = "ctx"

  def write(methods: Seq[Statement], name: String, `type`: String): Unit =
    write(methods, None, name, `type`)

  def write(methods: Seq[Statement], varsTypes: Map[String, Type], name: String, `type`: String): Unit =
    write(methods, Some(varsTypes), name, `type`)

  private def write(methods: Seq[Statement], varsTypes: Option[Map[String, Type]], name: String, `type`: String): Unit = {
    val methodsDecls = (methods zip MethodsAndArgType)
      .map { case (methodStmt, (methodName, argType)) => writeMethod(methodStmt, methodName, argType) }
      .reduce(_ ++ List("") ++ _)

    val body = varsTypes.toList.flatMap { varsTypes =>
      writeVarsDecls(varsTypes) ++ List("")
    } ++ methodsDecls

    val `class` = List("class `" ++ name ++ "` extends DSPScript {") ++ body.indented ++ List("}")
    val content = List("package " ++ `type`, "", "import player.script._", "") ++ `class`

    val w = getPrintWriter(name, `type`, ".scala")
    content foreach w.println
    w.close()
  }

  private def writeMethod(statement: Statement, name: String, argType: String): List[String] =
    ("def " + name + "(" + ContextArg + ": " + argType + "): Unit = ") +/+ write(statement)

  private def writeVarsDecls(varsTypes: Map[String, Type]): List[String] =
    for ((variable, typ) <- varsTypes.toList.sortBy { case (name, _) => name })
    yield {
      val (typeName, initialValue) = typ match {
        case BooleanType => ("Boolean", "false")
        case IntType => ("Int", "0")
        case DoubleType => ("Double", "0.0")
      }
      "private var " + variable + ": " + typeName + " = " + initialValue
    }

  private def write(statement: Statement): List[String] =
    statement match {
      case a: Assignment => write(a)
      case b: BlockStatement => write(b)
      case fc: FunctionCall => write(fc)
      case is: IfStatement => write(is)
      case ma: MegabufAssignment => write(ma)
    }

  private def write(assignment: Assignment): List[String] =
    prefixedVarName(assignment.variable.id) +/+ " = " +/+ write(assignment.value)

  private def write(block: BlockStatement): List[String] =
    List("{") ++
    block.statements.flatMap(write).indented ++
    List("}")

  private def write(ifStatement: IfStatement): List[String] = {
    def writeSingleStatementBlock(stmt: Statement): List[String] =
      write {
        stmt match {
          case bs: BlockStatement => bs
          case _ => BlockStatement(List(stmt))
        }
      }

    val conditionLine = "if (" +/+ write(ifStatement.condition) +/+ ") "
    val ifTrueWritten = writeSingleStatementBlock(ifStatement.ifTrue)
    val rest = ifStatement.ifFalse match {
      case Some(ifInElse: IfStatement) => " else " +/+ write(ifInElse)
      case Some(ifFalse) => " else " +/+ writeSingleStatementBlock(ifFalse)
      case None => Nil
    }
    conditionLine +/+ ifTrueWritten +/+ rest
  }

  private def write(megabufAssignment: MegabufAssignment): List[String] =
    ContextArg + ".megabuf(" +/+ write(megabufAssignment.index) +/+ ") = " +/+ write(megabufAssignment.value)

  def write(expression: Expression): List[String] =
    expression match {
      case bo: BinaryOperation => write(bo)
      case be: BlockExpression => write(be)
      case BooleanLiteral(value) => List(value.toString)
      case c: Constant => write(c)
      case DoubleLiteral(value) => List(value.toString)
      case fc: FunctionCall => write(fc)
      case id: Identifier => write(id)
      case ie: IfExpression => write(ie)
      case IntLiteral(value) => List(value.toString)
      case mc: MethodCall => write(mc)
      case not: Not => write(not)
    }

  private def write(binaryOperation: BinaryOperation): List[String] = {
    val maybeParenthesizedLeftOperand = parenthesizerIf {
      binaryOperation.leftOperand match {
        case lOp: BinaryOperation => precedence(lOp) < precedence(binaryOperation)
        case _: DoubleLiteral => false
        case _: FunctionCall => false
        case _: Identifier => false
        case _: IfExpression => true
        case _: IntLiteral => false
        case _: MethodCall => false
        case _: Not => false
      }
    }
    val maybeParenthesizedRightOperand = parenthesizerIf {
      binaryOperation.rightOperand match {
        case rOp: BinaryOperation => precedence(binaryOperation) >= precedence(rOp)
        case _: BooleanLiteral => false
        case _: Constant => false
        case _: DoubleLiteral => false
        case _: FunctionCall => false
        case _: Identifier => false
        case _: IfExpression => true
        case _: IntLiteral => false
        case _: MethodCall => false
        case _: Not => false
      }
    }
    val leftOperandWritten = maybeParenthesizedLeftOperand(write(binaryOperation.leftOperand))
    val rightOperandWritten = maybeParenthesizedRightOperand(write(binaryOperation.rightOperand))
    leftOperandWritten +/+ " " +/+ binaryOperation.operator.op +/+ " " +/+ rightOperandWritten
  }

  private def write(block: BlockExpression): List[String] =
    List("{") ++
    block.statements.flatMap(write).indented ++
    write(block.value).indented ++
    List("}")

  private def write(constant: Constant): List[String] =
    List(constant.name)

  private def write(functionCall: FunctionCall): List[String] = {
    val funcName = functionCall.function.id match {
      case "if" => "`if`"
      case "megabuf" => ContextArg + ".megabuf"
      case name => name
    }
    funcName +/+ "(" +/+ functionCall.arguments.map(write).reduce(_ +/+ ", " +/+ _) +/+ ")"
  }

  private def write(identifier: Identifier): List[String] =
    List(prefixedVarName(identifier.id))

  private def write(ifExpression: IfExpression): List[String] = {
    val conditionLine = "if (" +/+ write(ifExpression.condition) +/+ ") "

    def isSimple(expression: Expression): Boolean =
      expression match {
        case BinaryOperation(l, _, r) => isSimple(l) && isSimple(r)
        case _: BooleanLiteral => true
        case _: DoubleLiteral => true
        case _: Identifier => true
        case _: IntLiteral => true
      }

    val ifTrueWritten = if (isSimple(ifExpression.ifTrue)) {
      write(ifExpression.ifTrue)
    } else {
      write(BlockExpression(Nil, ifExpression.ifTrue))
    }

    val ifFalseWritten = ifExpression.ifFalse match {
      case ifInElse: IfExpression => write(ifInElse)
      case ifFalse => write(ifFalse)
    }
    conditionLine +/+ ifTrueWritten +/+ " else " +/+ ifFalseWritten
  }

  private def write(methodCall: MethodCall): List[String] = {
    val maybeParenthesize = parenthesizerIf {
      methodCall.`object` match {
        case _: BinaryOperation => true
        case _: Identifier => false
      }
    }
    maybeParenthesize(write(methodCall.`object`)) +/+ "." +/+ methodCall.method
  }

  private def write(not: Not): List[String] = {
    val maybeParenthesize = parenthesizerIf {
      not.value match {
        case _: BinaryOperation => true
        case _: Identifier => false
      }
    }
    "!" +/+ maybeParenthesize(write(not.value))
  }

  private def prefixedVarName(varName: String): String =
    PredefinedVars.All.get(varName).map(_ => ContextArg + ".").getOrElse("") + varName

  private def parenthesizerIf(condition: Boolean): List[String] => List[String] =
    if (condition) {
      "(" +/+ _ +/+ ")"
    } else {
      identity
    }

  private def precedence(binaryOperation: BinaryOperation): Int =
    binaryOperation.operator.op.head match {
      //          (all letters)
      case '|' => 2
      //          ^
      case '&' => 4
      case '=' | '!' => 5
      case '<' | '>' => 6
      //          :
      case '+' | '-' => 8
      case '*' | '/' | '%' => 9
      //          (all other special characters)
    }

  private implicit class StringOps(val string: String) extends AnyVal {
    def indented: String =
      "  " + string

    def +/+(that: String): List[String] =
      List(string) +/+ List(that)

    def +/+(that: List[String]): List[String] =
      List(string) +/+ that
  }

  private implicit class StringListOps(val stringList: List[String]) extends AnyVal {
    def indented: List[String] =
      stringList map { _.indented }

    def +/+(that: String): List[String] =
      stringList +/+ List(that)

    def +/+(that: List[String]): List[String] =
      if (stringList.isEmpty) {
        that
      } else if (that.isEmpty) {
        stringList
      } else {
        stringList.init ++ List(stringList.last + that.head) ++ that.tail
      }
  }
}
