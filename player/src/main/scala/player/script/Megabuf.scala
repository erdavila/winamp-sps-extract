package player.script

class Megabuf {
  private var values = Array.emptyDoubleArray

  def apply(index: Int): Double =
    values.applyOrElse(index, (_: Int) => 0.0)

  def update(index: Int, value: Double): Unit = {
    if (index >= values.length) {
      val newValues = Array.ofDim[Double](index + 1)
      Array.copy(values, 0, newValues, 0, values.length)
      values = newValues
    }
    values(index) = value
  }
}
