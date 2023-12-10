import ScratchCards._

object Puzzle04ScratchCards1 extends App with inputFileArgs with CommonUtils {
  val winningPoints = for {
    line <- getLines
    (_, twoPartNumbers) = splitInTwo("\\:", line)
    (part1, part2) = splitInTwo("\\|", twoPartNumbers)
    winning = numbersPattern.findAllMatchIn(part1).map(_.matched).toSet
    youHave = numbersPattern.findAllMatchIn(part2).map(_.matched).toSet
  } yield points(winning, youHave)

  Console.println(winningPoints.sum)
}

object ScratchCards {
  import scala.math.pow

  def points(winning: Set[String], youHave: Set[String]): Int = winning.intersect(youHave).size match {
    case 0 => 0
    case many => pow(2, many - 1).toInt // also works when many == 1
  }

  def copiesWon(cardIndex: Int, winning: Set[String], youHave: Set[String]): Seq[Int] = {
    winning.intersect(youHave).size match {
      case 0 => Seq.empty
      case many => for(i <- cardIndex until cardIndex + many) yield i + 1
    }
  }
}