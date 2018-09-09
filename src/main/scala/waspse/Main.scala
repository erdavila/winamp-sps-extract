package waspse

import scala.io.Source
import waspse.sps.SPSParsers

object Main {

  def main(args: Array[String]): Unit = {
    val inputFile = args(0)
    val input =  new ArrayCharSequence(Source.fromFile(inputFile).toArray[Char])
    val statements = SPSParsers(input)
    statements foreach println
  }
}
