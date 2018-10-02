package waspse

import java.io.File

import waspse.sps.{DecodedSPS, Decoder, Parsers}
import waspse.typeInference.{Type, TypeInferrer, TypeRequirementsAnalyzer}
import waspse.writers.{DecodedSPSWriter, ScalaWriter, TypeInferenceWriter}

class Steps(spsPath: String) {
  lazy val name: String = removeExtension(new File(spsPath).getName)

  println(">>> " + name)

  lazy val decodedSPS: DecodedSPS = {
    val decoded = Decoder.decode(spsPath)
    DecodedSPSWriter.write(decoded, name)
    decoded
  }

  lazy val methods: Seq[BlockStatement] = {
    val ms = decodedSPS.codes map Parsers.parse
    ScalaWriter.write(ms, name, "parsed")
    ms
  }

  lazy val normalizedMethods: Seq[Statement] = {
    val normMs = methods map Normalizer.normalize
    ScalaWriter.write(normMs, name, "untyped")
    normMs
  }

  lazy val (inferredVarsTypes, defaultTypedVars) = {
    val varsTypesReqs = TypeRequirementsAnalyzer.analyzeIn(normalizedMethods)
    val inferrer = new TypeInferrer(varsTypesReqs)
    val inferredVarsTypes = inferrer.infer()
    val defaultTypedVars = inferrer.defaultTypedVars(inferredVarsTypes.keySet)
    TypeInferenceWriter.write(varsTypesReqs, inferredVarsTypes, defaultTypedVars, name)
    (inferredVarsTypes, defaultTypedVars)
  }

  lazy val varsTypes: Map[String, Type] = inferredVarsTypes ++ defaultTypedVars

  lazy val typedMethods: Seq[Statement] = {
    val typedMethods = normalizedMethods map { new ExpressionTypeAdapter(varsTypes).adaptIn }
    ScalaWriter.write(typedMethods, varsTypes, name, "typed")
    typedMethods
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
