package player

import java.nio.{ByteBuffer, ByteOrder}

object S16leSample {

  val FFMpegFormat = "s16le"

  private val Scale = Short.MaxValue.toDouble

  def fromBytes(bytes: Iterator[Byte]): Iterator[Double] = {
    bytes
      .grouped(java.lang.Short.BYTES)
      .map { case Seq(b1: Byte, b2: Byte) =>
        ByteBuffer
          .allocate(2)
          .order(ByteOrder.LITTLE_ENDIAN)
          .put(b1)
          .put(b2)
          .getShort(0) / Scale
      }
  }

  def toBytes(value: Double): Array[Byte] = {
    val scaledValue = value * Scale
    val shortValue =
      if (scaledValue > Short.MaxValue) Short.MaxValue
      else if (scaledValue < Short.MinValue) Short.MinValue
      else scaledValue.toShort

    val buffer = ByteBuffer
      .allocate(2)
      .order(ByteOrder.LITTLE_ENDIAN)
      .putShort(shortValue)
    assert(buffer.hasArray)
    buffer.array()
  }
}
