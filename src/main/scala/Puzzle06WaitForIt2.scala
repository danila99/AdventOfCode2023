import WaitForIt.Race

object Puzzle06WaitForIt2 extends App with inputFileArgs with CommonUtils {
  // we know that the input is always 2 lines
  val time = numbersPattern.findAllMatchIn(getLines.head).map(_.matched).mkString("")
  val distance = numbersPattern.findAllMatchIn(getLines.tail.head).map(_.matched).mkString("")
  val race = Race(time.toLong, distance.toLong)

  Console.println(race)
  Console.println(race.winningInterval)
  Console.println(s"answer: ${race.waysToWin}")
}
