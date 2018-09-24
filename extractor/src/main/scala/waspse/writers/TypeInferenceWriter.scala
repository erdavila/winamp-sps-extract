package waspse.writers

import waspse.typeInference.{Type, TypeRequirements}

object TypeInferenceWriter extends Writer {

  def write(
    varsTypesReqs: Map[String, TypeRequirements],
    inferredVarsTypes: Map[String, Type],
    defaultTypedVars: Map[String, Type],
    name: String,
  ): Unit = {
    val w = getPrintWriter(name, "type-inference", ".txt")

    def printSorted(label: String, set: Set[String]): Unit = {
      w.print("  " + label + ": ")
      if (set.isEmpty) {
        w.println("-")
      } else {
        w.println(set.toSeq.sorted.mkString(", "))
      }
    }

    val allVarsTypes = inferredVarsTypes ++ defaultTypedVars

    for ((variable, typeReqs) <- varsTypesReqs.toSeq.sortBy { case (variable, _) => variable }) {
      w.println(variable)
      printSorted("compatible with variables", typeReqs.compatibleWithVariables)
      w.println("  minimum leveled type: " + typeReqs.minimumLeveledType)

      val defaulted = if (defaultTypedVars.contains(variable)) " (default)" else ""
      w.println("  inferred type: " + allVarsTypes(variable) + defaulted)

      w.println()
    }

    w.close()
  }
}
