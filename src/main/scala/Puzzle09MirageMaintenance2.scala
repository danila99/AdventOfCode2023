import MirageMaintenance._

object Puzzle09MirageMaintenance2 extends App with inputFileArgs with CommonUtils {
  def predictPreviousReading(reading: List[Int]): Int = {
    val heads = (reading +: allDifferences(reading, Nil)).map(_.head)
    heads.foldRight(0) { case (a, b) =>  a - b }
  }

  val predictedReadings = for {
    line <- getLines
    reading = numbersPattern.findAllMatchIn(line).map(_.matched)
  } yield predictPreviousReading(reading.map(_.toInt).toList)

  Console.println(predictedReadings.sum)
}
