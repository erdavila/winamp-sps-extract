package waspse

import waspse.typeInference.{BooleanType, DoubleType, IntType, Type}

case class PredefinedVars(`type`: Type, readOnly: Boolean = false)

object PredefinedVars {
  val All = Map(
    "nch" -> IntType,
    "srate" -> IntType,
    "slider1" -> DoubleType,
    "slider2" -> DoubleType,
    "slider3" -> DoubleType,
    "slider4" -> DoubleType,
    "trig1" -> BooleanType,
    "trig2" -> BooleanType,
    "trig3" -> BooleanType,
    "trig4" -> BooleanType,
    "spl0" -> DoubleType,
    "spl1" -> DoubleType,
    "skip" -> BooleanType,
    "repeat" -> BooleanType,
  )
}
