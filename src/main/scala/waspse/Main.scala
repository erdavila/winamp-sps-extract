package waspse

import java.io.File
import waspse.sps.{DecodedSPSWriter, Decoder, Parsers}

object Main {

  private val OutputDir = "output"

  def main(args: Array[String]): Unit = {
    val spsFile = args(0)
    val presetName = removeExtension(new File(spsFile).getName)
    println(">>> " + presetName)

    val sps = Decoder.decode(spsFile)
    DecodedSPSWriter.write(sps, presetName, OutputDir)

    val methods = sps.codes map Parsers.parse
    ScalaWriter.write(methods, presetName, "parsed", OutputDir)

    val transformedMethods = methods map Transformer.transform
    ScalaWriter.write(transformedMethods, presetName, "transformed", OutputDir)
  }

  private def removeExtension(fileName: String): String = {
    val pos = fileName.indexOf('.')
    if (pos >= 0) {
      fileName.take(pos)
    } else {
      fileName
    }
  }
}
