package waspse.typeInference

import scala.language.implicitConversions

case class TypeRequirements(compatibleWithVariables: Set[String], minimumLeveledType: LeveledType) {

  def +++(that: TypeRequirements): TypeRequirements = {
    val minimumLeveledType = this.minimumLeveledType max that.minimumLeveledType
    val compatibleWithVariables = this.compatibleWithVariables ++ that.compatibleWithVariables
    TypeRequirements(compatibleWithVariables, minimumLeveledType)
  }
}

object TypeRequirements {

  def apply(): TypeRequirements =
    TypeRequirements(Set.empty, BooleanType)

  implicit def apply(compatibleWithVariable: String): TypeRequirements =
    TypeRequirements(Set(compatibleWithVariable), BooleanType)

  implicit def apply(minimumLeveledType: LeveledType): TypeRequirements =
    TypeRequirements(Set.empty, minimumLeveledType)
}
