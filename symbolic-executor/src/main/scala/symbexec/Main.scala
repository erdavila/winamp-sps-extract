package symbexec

import waspse._
import waspse.typeInference.{BooleanType, DoubleType, IntType}
import waspse.writers.ScalaWriter

object Main {

  private val nch = 2
  private val srate = 100
  private val SampleCount = 1000

  def main(args: Array[String]): Unit = {
    val steps = new Steps(spsPath = args(0))
    val Seq(initialize, onSample, onSliderChange) = steps.typedMethods

    val executor = new SymbolicExecutor(steps.varsTypes)
    for ((variable, typ) <- steps.inferredVarsTypes) {
      executor.variables(variable) = typ match {
        case BooleanType => BooleanValue(false)
        case IntType => IntValue(0)
        case DoubleType => DoubleValue(0.0)
      }
    }

    executor.variables("nch") = IntValue(nch)
    executor.variables("srate") = IntValue(srate)
    executor.execute(initialize)

    executor.variables("slider1") = DoubleValue(0.5)
    executor.variables("slider2") = DoubleValue(0.5)
    executor.variables("slider3") = DoubleValue(0.5)
    executor.variables("slider4") = DoubleValue(0.5)
    executor.execute(onSliderChange)

    executor.variables("trig1") = BooleanValue(false)
    executor.variables("trig2") = BooleanValue(false)
    executor.variables("trig3") = BooleanValue(false)
    executor.variables("trig4") = BooleanValue(false)
    for (i <- 0 until SampleCount) {
      executor.variables("repeat") = BooleanValue(false)
      executor.variables("skip") = BooleanValue(false)
      executor.variables("spl0") = Variable("spl0[" + i.toString + "]", DoubleType)
      executor.variables("spl1") = Variable("spl1[" + i.toString + "]", DoubleType)
      executor.execute(onSample)

      print(i.toString + ": ")
      if (executor.variables("skip") == BooleanValue(true)) {
        println("skip")
      } else {
        for (s <- ScalaWriter.write(toExpression(executor.variables("spl0")))) {
          println(s)
        }
        if (executor.variables("repeat") == BooleanValue(true)) {
          println("  repeat")
        }
      }
    }
  }

  private def toExpression(symbolicValue: SymbolicExpression): Expression =
    symbolicValue match {
      case DoubleValue(double) => DoubleLiteral(double)
      case SymbolicBinaryOperation(leftOperand, op, rightOperand, _) => BinaryOperation(toExpression(leftOperand), Operator(op), toExpression(rightOperand))
      case SymbolicMethodCall(obj, methodName, _) => MethodCall(toExpression(obj), methodName)
      case Variable(name, _) => Identifier(name)
    }
}
