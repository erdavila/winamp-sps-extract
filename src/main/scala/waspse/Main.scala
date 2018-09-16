package waspse

import java.io.File
import scala.io.Source
import waspse.sps.SPSParsers
import waspse.ScalaWriter.{StringListOps, StringOps}

object Main {

  def main(args: Array[String]): Unit = {
    val presetDir = args(0)
    val presetName = new File(presetDir).getName
    println(">>> " + presetName)

    val initialize = codeToScala(presetDir, "initialize", 0)
    val onSample = codeToScala(presetDir, "onSample", 1)
    val onSliderChange = codeToScala(presetDir, "onSliderChange", 2)

    val body = initialize ++ List("") ++ onSliderChange ++ List("") ++ onSample
    val `trait` = List("trait `" + presetName + "` {") ++ body.indented ++ List("}")

    println()
    `trait` foreach println
  }

  private def codeToScala(presetDir: String, methodName: String, i: Int): List[String] = {
    val fileName = s"code$i.txt"
    println(">> " + fileName)
    val inputFile = new File(presetDir, fileName).toString
    val input = new ArrayCharSequence(Source.fromFile(inputFile).toArray[Char])
    val spsStatements = SPSParsers(input)
    val transformed = Transformer.transform(spsStatements)
    val written = ScalaWriter.write(transformed)
    ("def " + methodName + "(): Unit = ") +/+ written
  }
}
