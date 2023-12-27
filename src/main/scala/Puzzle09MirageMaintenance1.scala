import MirageMaintenance._

object Puzzle09MirageMaintenance1 extends App with inputFileArgs with CommonUtils {
  def predictNextReading(reading: List[Int]): Int =
    (reading +: allDifferences(reading, Nil)).map(_.last).sum

  val predictedReadings = for {
    line <- getLines
    reading = numbersPattern.findAllMatchIn(line).map(_.matched)
  } yield predictNextReading(reading.map(_.toInt).toList)

  Console.println(predictedReadings.sum)
}

object MirageMaintenance {
  import scala.annotation.tailrec

  @tailrec def sensorReadingDifference(reading: List[Int], reduced: List[Int]): List[Int] = reading match {
    case _ :: Nil => reduced
    case a :: b :: tail => sensorReadingDifference(b +: tail, reduced :+ (b - a))
  }

  @tailrec def allDifferences(reading: List[Int], reduced: List[List[Int]]): List[List[Int]] = {
    sensorReadingDifference(reading, Nil) match {
      case Nil => throw new IllegalArgumentException(s"reduced to Nil, unexpected reading: ${reading.mkString(", ")}")
      case r if r.last == 0 => reduced
      case r => allDifferences(r, reduced :+ r)
    }
  }
}