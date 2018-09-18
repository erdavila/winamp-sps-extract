package waspse.sps

import scala.io.Source

case class Slider(
  name: String,
  labelMin: String,
  labelMax: String,
  value: Int,
)

case class DecodedSPS(
  sliders: Seq[Slider],
  initializationCode: String,
  onSliderChangeCode: String,
  onSampleCode: String,
)

object Decoder {

  def decode(path: String): DecodedSPS = {
    val entries = Source.fromFile(path).getLines().collect {
      case line if line.contains('=') =>
        val Array(key, value) = line.split("=", 2)
        key -> value
    }.toMap

    val sliders =
      for {
        n <- 0 to 3
        name <- entries.get(s"labels_${n}_0")
      } yield {
        val labelMin = entries(s"labels_${n}_1")
        val labelMax = entries(s"labels_${n}_2")
        val value = entries(s"slider${n + 1}").toInt
        Slider(name, labelMin, labelMax, value)
      }

    val initializationCode = decodeCode(0, entries)
    val onSampleCode = decodeCode(1, entries)
    val onSliderChangeCode = decodeCode(2, entries)

    DecodedSPS(sliders, initializationCode, onSliderChangeCode, onSampleCode)
  }

  private def decodeCode(i: Int, entries: Map[String, String]): String =
    entries.get(s"code${i}_data") match {
      case Some(data) =>
        val chars = data.grouped(2).map { Integer.parseInt(_, 16).toChar }.toArray
        val size = entries(s"code${i}_size").toInt
        String.valueOf(chars, 0, size)

      case None =>
        ""
    }
}
