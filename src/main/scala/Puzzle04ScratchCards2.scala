import ScratchCards._

import scala.collection.mutable

object Puzzle04ScratchCards2 extends App with inputFileArgs with CommonUtils {
  val copies: mutable.Map[Int, Int] = mutable.Map()
  val totalCards = getLines.size

  for {
    (line, index) <- getLines.zipWithIndex
    (_, twoPartNumbers) = splitInTwo("\\:", line)
    (part1, part2) = splitInTwo("\\|", twoPartNumbers)
    winning = numbersPattern.findAllMatchIn(part1).map(_.matched).toSet
    youHave = numbersPattern.findAllMatchIn(part2).map(_.matched).toSet
  } yield {
    val cardIndex = index + 1

    copies.update(cardIndex, copies.getOrElse(cardIndex, 0) + 1) // each card wins itself
    val currentCardCopies = copies(cardIndex)

    copiesWon(cardIndex, winning, youHave)
      .filter(_ <= totalCards) // there are no copies past the last card
      .foreach { wonCardIndex =>
        val wonCardCopies = copies.getOrElse(wonCardIndex, 0)
        copies.update(wonCardIndex, wonCardCopies + currentCardCopies)
      }
  }

  Console.println(copies.values.sum)
}
