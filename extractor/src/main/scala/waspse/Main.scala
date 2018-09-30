package waspse

object Main {

  def main(args: Array[String]): Unit = {
    val steps = new Steps(spsPath = args(0))
    steps.typedMethods
  }
}
