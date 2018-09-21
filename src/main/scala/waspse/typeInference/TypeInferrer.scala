package waspse.typeInference

import scala.annotation.tailrec
import waspse.PredefinedVars

class TypeInferrer(varsTypesReqs: Map[String, TypeRequirements]) {

  private type VarsTypes = Map[String, Type]

  def infer(): VarsTypes =
    inferMissingVarsTypes(alreadyInferredVarsTypes = PredefinedVars.All) -- PredefinedVars.All.keySet

  @tailrec
  private def inferMissingVarsTypes(alreadyInferredVarsTypes: VarsTypes): VarsTypes = {
    val varsToInfer = varsTypesReqs.keySet -- alreadyInferredVarsTypes.keySet

    val newVarsTypes =
      for {
        variable <- varsToInfer
        typ <- inferType(variable, alreadyInferredVarsTypes)
      } yield variable -> typ

    if (newVarsTypes.isEmpty) {
      alreadyInferredVarsTypes
    } else {
      inferMissingVarsTypes(alreadyInferredVarsTypes ++ newVarsTypes)
    }
  }

  private def inferType(variable: String, knownVarsTypes: VarsTypes): Option[Type] =
    varsTypesReqs(variable) match {
      case TypeRequirements(_, DoubleType) => Some(DoubleType)

      case TypeRequirements(vars, minType) =>
        val resolvedVarsCompat = (vars - variable).map(knownVarsTypes.get)
        resolvedVarsCompat.foldLeft(Option(minType)) { (a, b) =>
          for (x <- a; y <- b) yield x max y
        } map {
          case t: Type => t
          case BooleanOrIntType => BooleanType
        }
    }

  def defaultTypedVars(inferredVars: Set[String]): VarsTypes = {
    val nonInferredVars = varsTypesReqs.keySet -- inferredVars
    nonInferredVars.map(variable => variable -> DoubleType).toMap
  }
}
