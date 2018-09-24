package waspse

import waspse.typeInference.{DoubleType, IntType, Type}

case class Function(returnType: Type, argumentsTypes: List[Type], spsName: String)

object Function {

  val ByScalaName = Map(
    "math.cos" -> Function(returnType = DoubleType, argumentsTypes = List(DoubleType), spsName = "cos"),
    "math.max" -> Function(returnType = DoubleType, argumentsTypes = List(DoubleType, DoubleType), spsName = "max"),
    "math.min" -> Function(returnType = DoubleType, argumentsTypes = List(DoubleType, DoubleType), spsName = "min"),
    "math.sin" -> Function(returnType = DoubleType, argumentsTypes = List(DoubleType), spsName = "sin"),
    "megabuf" -> Function(returnType = DoubleType, argumentsTypes = List(IntType), spsName = "megabuf"),
  )

  val SPSToScalaName: Map[String, String] =
    ByScalaName.map { case (scalaName, f) =>
      f.spsName -> scalaName
    }
}
