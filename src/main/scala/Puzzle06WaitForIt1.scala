import WaitForIt.Race

object Puzzle06WaitForIt1 extends App with inputFileArgs with CommonUtils {
  // we know that the input is always 2 lines
  val times = numbersPattern.findAllMatchIn(getLines.head).map(_.matched)
  val distances = numbersPattern.findAllMatchIn(getLines.tail.head).map(_.matched)
  val races = times.zip(distances).map { case (t, d) => Race(t.toLong, d.toLong) }.toSeq

  Console.println(races)
  Console.println(races.map(_.winningInterval))
  Console.println(races.map(_.waysToWin))
  Console.println(s"answer: ${races.map(_.waysToWin).product}")
}

object WaitForIt {
  import scala.math._

  final case class Race(time: Long, distance: Long) {
    val winningInterval: (Long, Long) = {
      val d: Double = sqrt(time.toDouble * time.toDouble - 4 * distance.toDouble)
      val x1: Double = (time.toDouble - d) / 2
      val x2: Double = (time.toDouble + d) / 2

      // we need to beat the race, not to repeat the previous wins, therefore using +1 and -1 coefficients
      (floor(x1 + 1).toLong, ceil(x2 - 1).toLong)
    }

    val waysToWin: Long = winningInterval match {
      case (x1, x2) => x2 - x1 + 1
    }
  }
}
