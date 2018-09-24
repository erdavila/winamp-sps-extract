package waspse.writers

import waspse.typeInference.TypeRequirements

object TypeRequirementsWriter extends Writer {

  def write(varsTypesReqs: Map[String, TypeRequirements], name: String): Unit = {
    val w = getPrintWriter(name, "type-requirements", ".txt")

    def printSorted(label: String, set: Set[String]): Unit = {
      w.print("  " + label + ": ")
      if (set.isEmpty) {
        w.println("-")
      } else {
        w.println(set.toSeq.sorted.mkString(", "))
      }
    }

    for ((variable, typeReqs) <- varsTypesReqs.toSeq.sortBy { case (variable, _) => variable }) {
      w.println(variable)
      printSorted("compatible with variables", typeReqs.compatibleWithVariables)
      w.println("  minimum leveled type: " + typeReqs.minimumLeveledType)
      w.println()
    }

    w.close()
  }
}
