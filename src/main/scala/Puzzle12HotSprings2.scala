import HotSprings._

object Puzzle12HotSprings2 extends App with inputFileArgs with CommonUtils {
  val rows = for {
    line <- getLines
    (springs, conditions) = splitInTwo(" ", line)
    brokenConditions = numbersPattern.findAllMatchIn(conditions).map(_.matched.toInt).toIndexedSeq
  } yield Row(springs.toList, brokenConditions)

  val arrangements = rows.map { r =>
    Console.print(s"$r: ")

    val symbols = List.fill(5)(r.symbols ++ List(Unknown)).flatten.dropRight(1)
    val brokenConditions = List.fill(5)(r.brokenConditions).flatten
    val result = countArrangements(symbols,brokenConditions, 0)

    Console.println(result)
    result
  }

  Console.println(arrangements.sum)
}