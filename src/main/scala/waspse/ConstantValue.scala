package waspse

import waspse.typeInference.{DoubleType, Type}

case class ConstantValue(`type`: Type, spsName: String)

object ConstantValue {

  val ByScalaName = Map(
    "math.Pi" -> ConstantValue(`type` = DoubleType, spsName = "$pi"),
  )

  val SPSToScalaName: Map[String, String] =
    ByScalaName.map { case (scalaName, c) =>
      c.spsName -> scalaName
    }
}
