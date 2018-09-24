package waspse.typeInference

sealed abstract class LeveledType(val level: Int) {
  def max(that: LeveledType): LeveledType =
    if (this.level >= that.level) this else that
}

sealed abstract class Type(level: Int) extends LeveledType(level)

case object BooleanType extends Type(1)
case object BooleanOrIntType extends LeveledType(2)
case object IntType extends Type(3)
case object DoubleType extends Type(4)
