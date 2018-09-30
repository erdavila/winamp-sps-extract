package player

import java.io._
import player.script._
import scala.reflect.runtime.{universe => ru}

object Main {

  private val Sample = S16leSample

  def main(args: Array[String]): Unit = {
    val Array(inputFile, scriptName, sliderValues@_*) = args
    val Seq(slider1, slider2, slider3, slider4, _*) = sliderValues.map(_.toDouble) ++ (1 to 4).map(_ => 0.5)

    val (sampleRate, channels) = probeInputFile(inputFile)

    val className = "typed." + ru.TypeName(scriptName).encodedName.toString
    val script = getClass.getClassLoader.loadClass(className).newInstance() match { case s: DSPScript => s }

    script.initialize(InitializeContext(
      nch = channels,
      srate = sampleRate,
    ))

    script.onSliderChange(SliderChangeContext(
      nch = channels,
      srate = sampleRate,
      slider1 = slider1,
      slider2 = slider2,
      slider3 = slider3,
      slider4 = slider4,
    ))

    val samples = getSamples(inputFile, channels)
    val megabuf = new Megabuf
    val output = samples.flatMap { case Array(spl0, spl1) =>
      val ctx = SampleContext(
        nch = channels,
        srate = sampleRate,
        megabuf = megabuf,
        repeat = false,
        skip = false,
        slider1 = slider1,
        slider2 = slider2,
        slider3 = slider3,
        slider4 = slider4,
        spl0 = spl0,
        spl1 = spl1,
        trig1 = false,
        trig2 = false,
        trig3 = false,
        trig4 = false,
      )
      script.onSample(ctx)

      val n = (ctx.repeat, ctx.skip) match {
        case (true, _) => 2
        case (_, true) => 0
        case _ => 1
      }
      lazy val outputSample = Array(ctx.spl0, ctx.spl1)
      for (_ <- 1 to n) yield outputSample
    }

    putSamples(output, channels)
  }

  private def probeInputFile(inputFile: String) = {
    val inputStream = new ProcessBuilder()
      .command(
        "ffprobe",
        "-hide_banner",
        "-v", "error",
        "-show_entries", "stream=sample_rate,channels",
        "-print_format", "default=noprint_wrappers=1",
        inputFile,
      )
      .redirectInput(ProcessBuilder.Redirect.INHERIT)
      .redirectOutput(ProcessBuilder.Redirect.PIPE)
      .redirectError(ProcessBuilder.Redirect.INHERIT)
      .start()
      .getInputStream

    val reader = new BufferedReader(new InputStreamReader(inputStream))

    val entries = Stream.continually(reader.readLine())
      .map(Option.apply)
      .takeWhile(_.isDefined)
      .collect { case Some(line) =>
        val Array(key, value) = line.split("=", 2)
        key -> value.toInt
      }
      .toMap

    (entries("sample_rate"), entries("channels"))
  }

  private def getSamples(inputFile: String, channels: Int): Iterator[Array[Double]] = {
    val bytes = getBytes(inputFile)
    Sample.fromBytes(bytes)
      .grouped(channels)
      .map(_.toArray)
  }

  private def getBytes(inputFile: String): Iterator[Byte] = {
    val is = getInputStream(inputFile)
    Iterator
      .continually(is.read())
      .takeWhile(_ != -1)
      .map(_.toByte)
  }

  private def getInputStream(inputFile: String): InputStream =
    new ProcessBuilder()
      .command(
        "ffmpeg",
        "-hide_banner",
        "-v", "error",
        "-i", inputFile,
        "-f", S16leSample.FFMpegFormat,
        "-"
      )
      .redirectInput(ProcessBuilder.Redirect.INHERIT)
      .redirectOutput(ProcessBuilder.Redirect.PIPE)
      .redirectError(ProcessBuilder.Redirect.INHERIT)
      .start()
      .getInputStream

  private def putSamples(samples: Iterator[Array[Double]], channels: Int): Unit = {
    val bytes = samples.flatten.flatMap(Sample.toBytes)
    putBytes(bytes, channels)
  }

  private def putBytes(bytes: Iterator[Byte], channels: Int): Unit = {
    val os = getOutputStream(channels)
    for (byte <- bytes) {
      os.write(byte)
    }
    os.flush()
  }

  private def getOutputStream(channels: Int): OutputStream =
    new ProcessBuilder()
      .command(
        "ffplay",
        "-hide_banner",
        "-v", "error",
        "-f", S16leSample.FFMpegFormat,
        "-ac", channels.toString,
        "-"
      )
      .redirectInput(ProcessBuilder.Redirect.PIPE)
      .redirectOutput(ProcessBuilder.Redirect.INHERIT)
      .redirectError(ProcessBuilder.Redirect.INHERIT)
      .start()
      .getOutputStream
}
