package waspse.sps

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import waspse._

object SPSParsers extends RegexParsers {

  override protected val whiteSpace: Regex = """(\s|//.*|/\*(\r|\n|.)*?\*/)+""".r

  def apply(input: CharSequence): BlockStatement =
    parseAll(sps, input) match {
      case Success(statements, _) => BlockStatement(statements)
      case ns: NoSuccess => sys.error(ns.toString)
    }

  private def sps: Parser[List[Statement]] = (statement <~ ";").*

  private def statement: Parser[Statement] = assignment | functionCall

  private def assignment: Parser[Assignment] = (variable <~ "=") ~ expression ^^ { case v ~ e => Assignment(v, e) }

  private def expression: Parser[Expression] = orOperand ~ (operator("|") ~ orOperand).* ^^ {
    case x ~ ys => ys.foldLeft(x) {
      case (a, op ~ b) => BinaryOperation(a, op, b)
    }
  }

  private def orOperand: Parser[Expression] = andOperand ~ (operator("&") ~ andOperand).* ^^ {
    case x ~ ys => ys.foldLeft(x) {
      case (a, op ~ b) => BinaryOperation(a, op, b)
    }
  }

  private def andOperand: Parser[Expression] = term ~ (operator("+", "-") ~ term).* ^^ {
    case x ~ ys => ys.foldLeft(x) {
      case (a, op ~ b) => BinaryOperation(a, op, b)
    }
  }

  private def term: Parser[Expression] = factor ~ (operator("*", "/", "%") ~ factor).* ^^ {
    case x ~ ys => ys.foldLeft(x) {
      case (a, op ~ b) => BinaryOperation(a, op, b)
    }
  }

  private def operator(op: String, ops: String*): Parser[Operator] = positioned {
    ops.foldLeft(literal(op)) { case (x, y) =>
      x | y
    } ^^ Operator
  }

  private def factor: Parser[Expression] = atom | "(" ~> expression <~ ")"

  private def atom: Parser[Expression] = functionCall | variable | constant | number

  private def functionCall: Parser[FunctionCall] = identifier ~ ("(" ~> repsep(expression, ",") <~ ")") ^^ {
    case function ~ arguments => FunctionCall(function, arguments)
  }

  private def variable: Parser[Identifier] = identifier

  private def identifier: Parser[Identifier] = positioned {
    """[a-z]([a-z0-9])*""".r ^^ Identifier
  }

  private def constant: Parser[Constant] = positioned {
    """\$[a-z]+""".r ^^ Constant
  }

  private def number: Parser[waspse.Number] = doubleLiteral | intLiteral

  private def doubleLiteral: Parser[DoubleLiteral] = positioned {
    """[0-9]+\.[0-9]+""".r ^^ { f => DoubleLiteral(f.toDouble) }
  }

  private def intLiteral: Parser[IntLiteral] = positioned {
    """[0-9]+""".r ^^ { i => IntLiteral(i.toInt) }
  }
}
