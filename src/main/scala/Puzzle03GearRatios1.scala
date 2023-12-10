import GearRatios._

import scala.util.matching.Regex

object Puzzle03GearRatios1 extends App with inputFileArgs {
  val numbersPattern: Regex = """\d+""".r
  val symbolsPattern: Regex = """[^.\d]""".r

  val numbers: Seq[Num] = for {
    (line, ln) <- getLines.zipWithIndex
    m <- numbersPattern.findAllMatchIn(line)
  } yield Num(Pos(ln, m.start), m.matched)

  val symbols: Seq[Sym] = for {
    (line, ln) <- getLines.zipWithIndex
    m <- symbolsPattern.findAllMatchIn(line)
  } yield Sym(Pos(ln, m.start))

  val positionsAdjacentToSymbols = symbols.flatMap(_.adjacent).toSet
  val numbersAdjacentToSymbols = for {
    n <- numbers
    if positionsAdjacentToSymbols.intersect(n.positions).nonEmpty
  } yield n

  Console.println(numbersAdjacentToSymbols.map(_.value.toInt).sum)
}

object GearRatios {
  final case class Pos(ln: Int, col: Int)

  final case class Num(position: Pos, value: String) {
    val positions: Set[Pos] = (0 until value.length).map { i =>
      Pos(ln = position.ln, col = position.col + i)
    }.toSet
  }

  final case class Sym(position: Pos) {
    val adjacent: Set[Pos] = (for {
      x <- Seq(-1, 0, 1)
      y <- Seq(-1, 0, 1)
      ln = position.ln + y
      col = position.col + x
      if ln >= 0 && col >= 0
    } yield Pos(ln, col)).toSet
  }
}