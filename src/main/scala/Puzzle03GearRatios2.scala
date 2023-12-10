import GearRatios._

import scala.util.matching.Regex

object Puzzle03GearRatios2 extends App with inputFileArgs {
  val numbersPattern: Regex = """\d+""".r
  val starsPattern: Regex = """\*""".r

  val numbers: Seq[Num] = for {
    (line, ln) <- getLines.zipWithIndex
    m <- numbersPattern.findAllMatchIn(line)
  } yield Num(Pos(ln, m.start), m.matched)

  val stars: Seq[Sym] = for {
    (line, ln) <- getLines.zipWithIndex
    m <- starsPattern.findAllMatchIn(line)
  } yield Sym(Pos(ln, m.start))

  val gearsRatios = for {
    g <- stars
    neighbors = numbers.filter(n => g.adjacent.intersect(n.positions).nonEmpty)
    if neighbors.size == 2
  } yield neighbors.map(_.value.toInt).product

  Console.println(gearsRatios.sum)
}