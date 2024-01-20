import HotSprings._

object Puzzle12HotSprings1 extends App with inputFileArgs with CommonUtils {
  val rows = for {
    line <- getLines
    (springs, conditions) = splitInTwo(" ", line)
    brokenConditions = numbersPattern.findAllMatchIn(conditions).map(_.matched.toInt).toIndexedSeq
  } yield Row(springs.toList, brokenConditions)

  val arrangements = rows.map { r =>
    Console.print(s"$r: ")
    val result = countArrangements(r.symbols, r.brokenConditions, 0)
    Console.println(result)
    result
  }

  Console.println(arrangements.sum)
}

object HotSprings {
  import scala.collection.mutable

  val Good = '.'
  val Damaged = '#'
  val Unknown = '?'

  final case class Row(symbols: List[Char], brokenConditions: Seq[Int]) {
    override def toString: String = s"${symbols.mkString} ${brokenConditions.mkString(", ")}"
  }

  type CountArgs = (List[Char], Seq[Int], Long)

  val memo = new mutable.HashMap[CountArgs, Long]()

  def countArrangements(countArgs: CountArgs): Long = {
    val (pipes, damagedGroups, result) = countArgs

    if (damagedGroups.isEmpty)
      return if (pipes.contains(Damaged)) result else result + 1
    if (pipes.isEmpty)
      return if (damagedGroups.isEmpty) result + 1 else result

    val damaged = damagedGroups.head

    lazy val canProceedWhenDamaged: Boolean =
      damaged <= pipes.size &&
      !pipes.take(damaged).contains(Good) &&
      (damaged == pipes.size || pipes(damaged) != Damaged)

    lazy val resultForGoodPipe = {
      val args = (pipes.tail, damagedGroups, result)
      memo.getOrElseUpdate(args, countArrangements(args))
    }

    lazy val resultForBrokenPipe = {
      val args = (pipes.drop(damaged + 1), damagedGroups.tail, result)
      memo.getOrElseUpdate(args, countArrangements(args))
    }

    pipes.head match {
      case Damaged if canProceedWhenDamaged => resultForBrokenPipe
      case Unknown if canProceedWhenDamaged => resultForGoodPipe + resultForBrokenPipe
      case Good | Unknown => resultForGoodPipe
      case _ => result
    }
  }
}

