package waspse

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Positional

case class Str(s: String) extends Positional

trait SPSParsers extends RegexParsers {
  override protected val whiteSpace: Regex = """(\s|//.*|/\*(\n|.)*?\*/)+""".r

  def name: Parser[Str] = positioned { """[a-z]+""".r ^^ Str }
  def all: Parser[List[Str]] = name.*
}

object Main extends SPSParsers {

  private val str =
    """
      |aaa bbb /* ccc*/ ddd
      |ee fff /* gggg
      |hhh ii
      |jjj */ kkkk ll
      |   mmm n // oooo
      | pp
      |// qqq rrr
      |ss
    """.stripMargin

  def main(args: Array[String]): Unit =
    parseAll(all, str) match {
      case Success(result, _) => result foreach { case Str(s) => println(s) }
      case ns: NoSuccess => println(ns)
    }
}
