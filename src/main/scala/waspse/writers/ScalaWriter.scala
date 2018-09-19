package waspse.writers

import waspse._

object ScalaWriter extends Writer {

  private val MethodNames = Seq("initialize", "onSample", "onSliderChange")

  def write(methods: Seq[Statement], name: String, `type`: String): Unit = {
    val body = (methods zip MethodNames)
      .map((writeMethod _).tupled)
      .reduce(_ ++ List("") ++ _)
      .indented
    val `trait` = List("trait `" ++ name ++ "` {") ++ body ++ List("}")
    val content = List("package " ++ `type`, "") ++ `trait`

    val w = getPrintWriter(name, `type`, ".scala")
    content foreach w.println
    w.close()
  }

  private def writeMethod(statement: Statement, name: String): List[String] = {
    val written = ScalaWriter.write(statement)
    ("def " + name + "(): Unit = ") +/+ written
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
    assignment.variable.id +/+ " = " +/+ write(assignment.value)

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
    "megabuf(" +/+ write(megabufAssignment.index) +/+ ") = " +/+ write(megabufAssignment.value)

  private def write(expression: Expression): List[String] =
    expression match {
      case bo: BinaryOperation => write(bo)
      case be: BlockExpression => write(be)
      case c: Constant => write(c)
      case DoubleLiteral(value) => List(value.toString)
      case fc: FunctionCall => write(fc)
      case Identifier(id) => List(id)
      case ie: IfExpression => write(ie)
      case IntLiteral(value) => List(value.toString)
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
        case _: Not => false
      }
    }
    val maybeParenthesizedRightOperand = parenthesizerIf {
      binaryOperation.rightOperand match {
        case rOp: BinaryOperation => precedence(binaryOperation) >= precedence(rOp)
        case _: Constant => false
        case _: DoubleLiteral => false
        case _: FunctionCall => false
        case _: Identifier => false
        case _: IntLiteral => false
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
    constant.name match {
      case "$pi" => List("math.Pi")
    }

  private def write(functionCall: FunctionCall): List[String] = {
    val funcName = functionCall.function.id match {
      case "if" => "`if`"
      case name => name
    }
    funcName +/+ "(" +/+ functionCall.arguments.map(write).reduce(_ +/+ ", " +/+ _) +/+ ")"
  }

  private def write(ifExpression: IfExpression): List[String] = {
    val conditionLine = "if (" +/+ write(ifExpression.condition) +/+ ") "

    def isSimple(expression: Expression): Boolean =
      expression match {
        case BinaryOperation(l, _, r) => isSimple(l) && isSimple(r)
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

  private def write(not: Not): List[String] = {
    val maybeParenthesize = parenthesizerIf {
      not.value match {
        case _: BinaryOperation => true
        case _: Identifier => false
      }
    }
    "!" +/+ maybeParenthesize(write(not.value))
  }

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