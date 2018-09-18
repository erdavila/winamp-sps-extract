package waspse.sps

import java.io.{File, PrintWriter}

object DecodedSPSWriter {

  private val CodeSessions = Seq("Initialization Code", "On Sample Code", "On Slider Change Code")

  def write(file: File, sps: DecodedSPS): Unit = {
    val w = new PrintWriter(file)

    def printSession(session: String): Unit =
      w.println("/" + ("** " + session + " " + "*" * 80).take(78) + "/")

    printSession("Sliders")
    for ((slider, i) <- sps.sliders.zipWithIndex) {
      w.println()
      w.println("// " + (i + 1))
      w.println("name: " + slider.name)
      w.println("labelMin: " + slider.labelMin)
      w.println("labelMax: " + slider.labelMax)
      w.println("value: " + slider.value)
    }

    def printCode(session: String, code: String): Unit = {
      w.println()
      printSession(session)
      if (code.nonEmpty) {
        w.println()
        w.println(code)
      }
    }

    for ((session, code) <- CodeSessions zip sps.codes) {
      printCode(session, code)
    }

    w.close()

  }
}
