package waspse

import scala.io.Source
import waspse.sps.SPSParsers

object Main {

  def main(args: Array[String]): Unit = {
    val inputFile = args(0)
    println(">>> " + inputFile)
    val input = new ArrayCharSequence(Source.fromFile(inputFile).toArray[Char])
    val spsStatements = SPSParsers(input)
    val transformed = Transformer.transform(spsStatements)
    val written = ScalaWriter.write(transformed)

//    println(spsStatements)
//    println()
//    println(transformed)
//    println()
    written.foreach(println)
  }
}
