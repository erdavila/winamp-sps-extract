package waspse

import java.io.File
import waspse.ScalaWriter.{StringListOps, StringOps}
import waspse.sps.{DecodedSPSWriter, Decoder, Parsers}

object Main {

  private val OutputDir = "output"

  def main(args: Array[String]): Unit = {
    val spsFile = args(0)
    val presetName = removeExtension(new File(spsFile).getName)
    println(">>> " + presetName)

    val outputPath = new File(OutputDir, presetName)
    outputPath.mkdirs()

    val sps = Decoder.decode(spsFile)
    DecodedSPSWriter.write(new File(outputPath, "sps.decoded"), sps)

    val initialize = codeToScala(sps.initializationCode, "initialize")
    val onSliderChange = codeToScala(sps.onSliderChangeCode, "onSliderChange")
    val onSample = codeToScala(sps.onSampleCode, "onSample")
    val body = initialize ++ List("") ++ onSliderChange ++ List("") ++ onSample
    val `trait` = List("trait `" + presetName + "` {") ++ body.indented ++ List("}")

    println()
    `trait` foreach println
  }

  private def removeExtension(fileName: String): String = {
    val pos = fileName.indexOf('.')
    if (pos >= 0) {
      fileName.take(pos)
    } else {
      fileName
    }
  }

  private def codeToScala(code: String, methodName: String): List[String] = {
    val spsStatements = Parsers.parse(code)
    val transformed = Transformer.transform(spsStatements)
    val written = ScalaWriter.write(transformed)
    ("def " + methodName + "(): Unit = ") +/+ written
  }
}
