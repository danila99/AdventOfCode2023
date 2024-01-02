import CosmicExpansion._

import scala.math._

object Puzzle11CosmicExpansion extends App with inputFileArgs {
  // This solution solves both parts of the Puzzle. Provide the expansion value to galaxies(...) call, as explained
  // in the puzzle definition. Expansion is 2 for the first part of the puzzle and it is 1,000,000 for the second part.
  val cosmos = new Cosmos(getLines)
  val galaxies = cosmos.galaxies(expansionSize = 1000000)

  val distances: Seq[Int] = for {
    p1 <- galaxies
    p2 <- galaxies
    if p1 != p2
  } yield abs(p1.x - p2.x) + abs(p1.y - p2.y)

  val total = distances.map(_.toLong).sum / 2 // all the pairs were counted twice
  Console.println(total)
}

object CosmicExpansion {
  final case class Point(x: Int, y: Int)

  val EmptySpace: Char = '.'
  val Galaxy: Char = '#'

  final class Cosmos(lines: Iterable[String]) {
    private lazy val chars: Map[Point, Char] = (for {
      (line, y) <- lines.iterator.zipWithIndex
      (char, x) <- line.iterator.zipWithIndex
      point = Point(x, y)
    } yield point -> char).toMap

    private lazy val xMax: Int = lines.headOption.map(_.length - 1).getOrElse {
      throw new IllegalArgumentException("lines are empty, check your input")
    }

    private lazy val yMax: Int = lines.size - 1

    def galaxies(expansionSize: Int = 1): Seq[Point] = {
      val additionalDistance = max(1, expansionSize - 1) // -1 is a mysterious adjustment, I tried it on a hunch.

      val emptyLines: Set[Int] = (for {
        y <- 0 to yMax
        if (0 to xMax).forall(x => chars(Point(x, y)) == EmptySpace)
      } yield y).toSet

      val emptyColumns: Set[Int] = (for {
        x <- 0 to xMax
        if (0 to yMax).forall(y => chars(Point(x, y)) == EmptySpace)
      } yield x).toSet

      (for {
        case (p, c) <- chars
        if c == Galaxy
        xOffset = emptyColumns.count(_ < p.x) * additionalDistance
        yOffset = emptyLines.count(_ < p.y) * additionalDistance
      } yield Point(p.x + xOffset, p.y + yOffset)).toSeq
    }
  }
}