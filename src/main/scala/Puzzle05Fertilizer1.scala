import Fertilizer.{getDestination, readAlmanacMaps}

object Puzzle05Fertilizer1 extends App with inputFileArgs with CommonUtils {
  val seeds = for {
    line <- getLines.headOption.toSeq
    s <- numbersPattern.findAllMatchIn(line).map(_.matched)
  } yield BigInt(s)

  val allMaps = readAlmanacMaps(getLines.tail, Seq.empty)

  val destinations = seeds.map(s => getDestination(s, allMaps.toList))

  Console.println(destinations.min)
}

object Fertilizer extends CommonUtils {
  import scala.annotation.tailrec

  final case class Interval(start: BigInt, range:BigInt) {
    def contains(i: BigInt): Boolean = start <= i && i <= (start + range)
  }

  final case class LineFromAlmanac(destination: BigInt, source: BigInt, range: BigInt)

  final case class AlmanacMap(lines: Seq[LineFromAlmanac]) {
    def map(from: BigInt): BigInt = lines
      .find(l => l.source <= from && from <= (l.source + l.range))
      .map(l => l.destination + (from - l.source))
      .getOrElse(from)

    def mapInReverse(from: BigInt): BigInt = lines
      .map(l => LineFromAlmanac(l.source, l.destination, l.range))
      .find(l => l.source <= from && from <= (l.source + l.range))
      .map(l => l.destination + (from - l.source))
      .getOrElse(from)
  }

  private def getAlmanacMap(linesWithNumbers: Seq[String]): AlmanacMap = {
    val lineFromAlmanacs = for {
      line <- linesWithNumbers
      s = numbersPattern.findAllMatchIn(line).map(_.matched).toIndexedSeq
    } yield LineFromAlmanac(BigInt(s(0)), BigInt(s(1)), BigInt(s(2)))

    AlmanacMap(lineFromAlmanacs)
  }

  @tailrec
  def readAlmanacMaps(lines: Seq[String], initial: Seq[AlmanacMap]): Seq[AlmanacMap] =
    lines
      .dropWhile(numbersPattern.findFirstMatchIn(_).isEmpty)
      .span(numbersPattern.findFirstMatchIn(_).nonEmpty) match {
      case (Seq(), _) => initial
      case (linesWithNumbers, rest) =>
        val almanac = getAlmanacMap(linesWithNumbers)
        readAlmanacMaps(rest, initial.appended(almanac))
    }

  @tailrec
  def getDestination(source: BigInt, restOfMaps: List[AlmanacMap]): BigInt = restOfMaps match {
    case Nil => source
    case h :: Nil => h.map(source)
    case h :: t => getDestination(h.map(source), t)
  }

  @tailrec
  def getSeed(source: BigInt, restOfMaps: List[AlmanacMap]): BigInt = restOfMaps match {
    case Nil => source
    case h :: Nil => h.mapInReverse(source)
    case h :: t => getSeed(h.mapInReverse(source), t)
  }
}