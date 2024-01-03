import HotSprings._

object Puzzle12HotSprings1 extends App with inputFileArgs with CommonUtils {
  val rows = for {
    line <- getLines
    (springs, conditions) = splitInTwo(" ", line)
    brokenConditions = numbersPattern.findAllMatchIn(conditions).map(_.matched.toInt).toIndexedSeq
  } yield Row(springs.toList, brokenConditions)

  val arrangements = rows.map { r =>rowArrangements("", r.symbols, r.brokenConditions).size }

  Console.println(arrangements.sum)
}

object HotSprings {
  import scala.util.matching.Regex

  val brokenPattern: Regex = "#+".r
  val Good = '.'
  val Damaged = '#'
  val Unknown = '?'

  final case class Row(symbols: List[Char], brokenConditions: Seq[Int])

  def damagedGroups(row: String): Seq[Int] =
    brokenPattern.findAllMatchIn(row).map(_.matched.length).toSeq

  def isRowValid(row: String, damaged: Seq[Int]): Boolean =
    damagedGroups(row) == damaged

  def rowArrangements(head: String, tail: List[Char], damagedGroups: Seq[Int]): Seq[String] = tail match {
    case Nil if isRowValid(head, damagedGroups) => Seq(head)
    case Nil => Seq.empty
    case Good :: rest => rowArrangements(head + Good, rest, damagedGroups)
    case Damaged :: rest => rowArrangements(head + Damaged, rest, damagedGroups)
    case Unknown :: rest =>
      rowArrangements(head + Good, rest, damagedGroups) ++ rowArrangements(head + Damaged, rest, damagedGroups)
  }
}

