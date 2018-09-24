package waspse

import java.io.File

import waspse.sps.{Decoder, Parsers}
import waspse.writers.{DecodedSPSWriter, ScalaWriter, TypeInferenceWriter}
import waspse.typeInference.{TypeInferrer, TypeRequirementsAnalyzer}

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
    val inferrer = new TypeInferrer(varsTypesReqs)
    val inferredVarsTypes = inferrer.infer()
    val defaultTypedVars = inferrer.defaultTypedVars(inferredVarsTypes.keySet)
    TypeInferenceWriter.write(varsTypesReqs, inferredVarsTypes, defaultTypedVars, name)

    val varsTypes = inferredVarsTypes ++ defaultTypedVars
    val typedMethods = transformedMethods map { new ExpressionTypeAdapter(varsTypes).adaptIn }
    ScalaWriter.write(typedMethods, varsTypes, name, "typed")
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
