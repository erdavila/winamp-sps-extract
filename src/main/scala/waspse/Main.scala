package waspse

import java.io.File
import waspse.sps.{Decoder, Parsers}
import waspse.writers.{TypeRequirementsWriter, DecodedSPSWriter, ScalaWriter}
import waspse.typeInference.TypeRequirementsAnalyzer

object Main {

  def main(args: Array[String]): Unit = {
    val spsFile = args(0)
    val name = removeExtension(new File(spsFile).getName)
    println(">>> " + name)

    val sps = Decoder.decode(spsFile)
    DecodedSPSWriter.write(sps, name)

    val methods = sps.codes map Parsers.parse
    ScalaWriter.write(methods, name, "parsed")

    val transformedMethods = methods map Transformer.transform
    ScalaWriter.write(transformedMethods, name, "untyped")

    val varsTypesReqs = TypeRequirementsAnalyzer.analyzeIn(transformedMethods)
    TypeRequirementsWriter.write(varsTypesReqs, name)
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
