package symbexec

import scala.collection.mutable
import waspse._
import waspse.typeInference.{BooleanType, DoubleType, IntType, Type}

sealed abstract class SymbolicExpression(val typ: Type)

sealed abstract class ResolvedExpression(typ: Type) extends SymbolicExpression(typ)
case class BooleanValue(boolean: Boolean) extends ResolvedExpression(BooleanType)

sealed abstract class NumericValue(typ: Type) extends ResolvedExpression(typ)
case class IntValue(int: Int) extends NumericValue(IntType)
case class DoubleValue(double: Double) extends NumericValue(DoubleType)
object NumericValue {
  def unapply(symbolicExpression: SymbolicExpression): Option[Double] =
    symbolicExpression match {
      case IntValue(int) => Some(int)
      case DoubleValue(double) => Some(double)
      case _ => None
    }
}

sealed abstract class UnresolvedExpression(typ: Type) extends SymbolicExpression(typ)
case class Variable(name: String, override val typ: Type) extends UnresolvedExpression(typ)
case class SymbolicBinaryOperation(leftOperand: SymbolicExpression, op: String, rightOperand: SymbolicExpression, override val typ: Type) extends UnresolvedExpression(typ)
case class SymbolicMethodCall(obj: SymbolicExpression, methodName: String, override val typ: Type) extends UnresolvedExpression(typ: Type)

class SymbolicExecutor(varsTypes: Map[String, Type]) {

  val variables = mutable.Map.empty[String, SymbolicExpression]

  private val allVarsTypes = varsTypes ++ PredefinedVars.All

  private object megabuf {
    private var values = Array.empty[SymbolicExpression]

    def apply(index: Int): SymbolicExpression = {
      val possiblyNullValue = values.applyOrElse(index, (_: Int) => DoubleValue(0.0))
      Option(possiblyNullValue).getOrElse(DoubleValue(0.0))
    }

    def update(index: Int, value: SymbolicExpression): Unit = {
      if (index >= values.length) {
        val newValues = Array.ofDim[SymbolicExpression](index + 1)
        Array.copy(values, 0, newValues, 0, values.length)
        values = newValues
      }
      values(index) = value
    }
  }

  def execute(code: Statement): Unit =
    executeStatement(code)

  private def executeStatement(statement: Statement): Unit =
    statement match {
      case a: Assignment => executeAssignment(a)
      case bs: BlockStatement => executeBlockStatement(bs)
      case is: IfStatement => executeIfStatement(is)
      case ma: MegabufAssignment => executeMegabufAssignment(ma)
    }

  private def executeAssignment(assignment: Assignment): Unit = {
    val value = evaluateExpression(assignment.value)
    val variable = assignment.variable.id

    variables(variable) = (allVarsTypes(variable), value) match {
      case (varT, value) if varT == value.typ => value
      case (DoubleType, IntValue(int)) => DoubleValue(int)
    }
  }

  private def executeBlockStatement(blockStatement: BlockStatement): Unit =
    for (stmt <- blockStatement.statements) {
      executeStatement(stmt)
    }

  private def executeIfStatement(ifStatement: IfStatement): Unit = {
    val BooleanValue(condition) = evaluateExpression(ifStatement.condition)
    if (condition) {
      executeStatement(ifStatement.ifTrue)
    } else {
      for (s <- ifStatement.ifFalse) {
        executeStatement(s)
      }
    }
  }

  private def executeMegabufAssignment(megabufAssignment: MegabufAssignment): Unit = {
    val IntValue(index) = evaluateExpression(megabufAssignment.index)
    val value = evaluateExpression(megabufAssignment.value)
    assert(value.typ == DoubleType)
    megabuf(index) = value
  }

  private def evaluateExpression(expression: Expression): SymbolicExpression =
    expression match {
      case bo: BinaryOperation => evaluateBinaryOperation(bo)
      case BooleanLiteral(value) => BooleanValue(value)
      case c: Constant => evaluateConstant(c)
      case DoubleLiteral(value) => DoubleValue(value)
      case fc: FunctionCall => evaluateFunctionCall(fc)
      case id: Identifier => evaluateIdentifier(id)
      case ie: IfExpression => evaluateIfExpression(ie)
      case IntLiteral(value) => IntValue(value)
      case mc: MethodCall => evaluateMethodCall(mc)
      case not: Not => evaluateNot(not)
    }

  private def evaluateBinaryOperation(binaryOperation: BinaryOperation): SymbolicExpression = {
    val leftOperand = evaluateExpression(binaryOperation.leftOperand)
    val rightOperand = evaluateExpression(binaryOperation.rightOperand)

    object NumericType {
      def unapply(typ: Type): Boolean =
        typ match {
          case BooleanType => false
          case IntType | DoubleType => true
        }
    }

    type SymExprOperationPF = PartialFunction[(SymbolicExpression, SymbolicExpression), SymbolicExpression]

    def intOperation(operation: (Int, Int) => Int)(resolveKnownCases: SymExprOperationPF): SymbolicExpression = {
      assert(leftOperand.typ == IntType)
      assert(rightOperand.typ == IntType)

      val makeResolvedValue: SymExprOperationPF = {
        case (IntValue(l), IntValue(r)) => IntValue(operation(l, r))
      }

      val makeUnresolvedValue: SymExprOperationPF = {
        case _ if false => ???
      }

      val makeResult = makeResolvedValue orElse resolveKnownCases orElse makeUnresolvedValue
      makeResult((leftOperand, rightOperand)) ensuring { _.typ == IntType }
    }

    def booleanOperation(operation: (Boolean, Boolean) => Boolean)(resolveKnownCases: SymExprOperationPF): SymbolicExpression = {
      assert(leftOperand.typ == BooleanType)
      assert(rightOperand.typ == BooleanType)

      val makeResolvedValue: SymExprOperationPF = {
        case (BooleanValue(l), BooleanValue(r)) => BooleanValue(operation(l, r))
      }

      val makeUnresolvedValue: SymExprOperationPF = {
        case _ if false => ???
      }

      val makeResult = makeResolvedValue orElse resolveKnownCases orElse makeUnresolvedValue
      makeResult((leftOperand, rightOperand)) ensuring { _.typ == BooleanType }
    }

    def comparisonOperation(operation: (Double, Double) => Boolean): SymbolicExpression = {
      assert(leftOperand.typ != BooleanType)
      assert(rightOperand.typ != BooleanType)

      val result = (leftOperand, rightOperand) match {
        case (NumericValue(l), NumericValue(r)) => operation(l, r)
      }
      BooleanValue(result)
    }

    def arithmeticOperation(operation: (Double, Double) => Double)(resolveKnownCases: SymExprOperationPF): SymbolicExpression = {
      val resultType = (leftOperand.typ, rightOperand.typ) match {
        case (IntType, IntType) => IntType
        case (NumericType(), NumericType()) => DoubleType
      }

      val makeResolvedValue: SymExprOperationPF = {
        case (NumericValue(l), NumericValue(r)) => DoubleValue(operation(l, r))
      }

      val makeUnresolvedValue: SymExprOperationPF = {
        case (lOp, rOp) => SymbolicBinaryOperation(lOp, binaryOperation.operator.op, rOp, resultType)
      }

      val makeResult = makeResolvedValue orElse resolveKnownCases orElse makeUnresolvedValue andThen { result =>
        (result, resultType) match {
          case _ if result.typ == resultType => result
          case (DoubleValue(double), IntType) => IntValue(double.toInt)
        }
      }

      makeResult((leftOperand, rightOperand))
    }

    val All1BitsIntValue = IntValue(~0)

    binaryOperation.operator.op match {
      case "|" =>
        intOperation(_ | _) {
          case (IntValue(0), rOp) => rOp
          case (lOp, IntValue(0)) => lOp
          case (All1BitsIntValue, _) => ???
          case (_, All1BitsIntValue) => ???
        }
      case "||" =>
        booleanOperation(_ || _) {
          case (BooleanValue(false), rOp) => ??? //rOp
          case (lOp, BooleanValue(false)) => ??? // lOp
          case (BooleanValue(true), _) => ??? // BooleanValue(true)
          case (_, BooleanValue(true)) => ??? // BooleanValue(true)
        }
      case "&" =>
        intOperation(_ & _) {
          case (All1BitsIntValue, rOp) => ??? //rOp
          case (lOp, All1BitsIntValue) => ??? //lOp
          case (IntValue(0), _) => ??? //IntValue(0)
          case (_, IntValue(0)) => ??? //IntValue(0)
        }
      case "==" =>
        comparisonOperation(_ == _)
      case "!=" =>
        comparisonOperation(_ != _)
      case "<" =>
        comparisonOperation(_ < _)
      case ">" =>
        comparisonOperation(_ > _)
      case "+" =>
        arithmeticOperation(_ + _) {
          case (NumericValue(0), rOp) => rOp
          case (lOp, NumericValue(0)) => lOp
        }
      case "-" =>
        arithmeticOperation(_ - _) {
          case (lOp, NumericValue(0)) => ??? //lOp
        }
      case "*" =>
        arithmeticOperation(_ * _) {
          case (NumericValue(1), rOp) => ??? //rOp
          case (lOp, NumericValue(1)) => lOp
          case (NumericValue(0), _) => ??? //DoubleValue(0)
          case (_, NumericValue(0)) => DoubleValue(0)
        }
      case "/" =>
        assert(leftOperand.typ == DoubleType || rightOperand.typ == DoubleType)
        arithmeticOperation(_ / _) {
          case (_, NumericValue(1)) => ???
          case (NumericValue(0), _) => ???
        }
      case "%" =>
        intOperation(_ % _) {
          case (_, IntValue(1)) => ??? //IntValue(0)
          case (IntValue(0), _) => ??? //IntValue(0)
        }
    }
  }

  private def evaluateConstant(constant: Constant): SymbolicExpression =
    constant.name match {
      case "math.Pi" => DoubleValue(math.Pi)
    }

  private def evaluateFunctionCall(functionCall: FunctionCall): SymbolicExpression = {
    val args = functionCall.arguments.map(evaluateExpression)
    functionCall.function.id match {
      case "math.cos" =>
        val List(NumericValue(a)) = args
        DoubleValue(math.cos(a))
      case "math.max" =>
        val List(NumericValue(a), NumericValue(b)) = args
        DoubleValue(math.max(a, b))
      case "math.min" =>
        val List(NumericValue(a), NumericValue(b)) = args
        DoubleValue(math.min(a, b))
      case "math.sin" =>
        val List(NumericValue(a)) = args
        DoubleValue(math.sin(a))
      case "megabuf" =>
        val List(IntValue(index)) = args
        megabuf(index)
    }
  }

  private def evaluateIdentifier(identifier: Identifier): SymbolicExpression =
    variables(identifier.id)

  private def evaluateIfExpression(ifExpression: IfExpression): SymbolicExpression = {
    val BooleanValue(condition) = evaluateExpression(ifExpression.condition)
    if (condition) {
      evaluateExpression(ifExpression.ifTrue)
    } else {
      evaluateExpression(ifExpression.ifFalse)
    }
  }

  private def evaluateMethodCall(methodCall: MethodCall): SymbolicExpression = {
    val obj = evaluateExpression(methodCall.`object`)
    (obj, methodCall.method) match {
      case (DoubleValue(value), "toInt") => IntValue(value.toInt)
      case (obj: SymbolicBinaryOperation, "toInt") => SymbolicMethodCall(obj, "toInt", IntType)
    }
  }

  private def evaluateNot(not: Not): SymbolicExpression = {
    val BooleanValue(value) = evaluateExpression(not.value)
    BooleanValue(!value)
  }
}
