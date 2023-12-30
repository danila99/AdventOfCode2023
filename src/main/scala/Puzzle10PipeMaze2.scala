import PipeMaze._

import scala.math.abs

object Puzzle10PipeMaze2 extends App with inputFileArgs {
  // look for the correct starting 'exits' in the input file. I was too lazy to find it with the code.
  // We are guaranteed to have two exits from the Start pipe by the rules, add them to 'startPipe' value below.
  val startPipe: Pipe = Pipe('S', Seq(East, South))

  val pipeWalker = new PipeWalker(getLines, startPipe)

  val startPoint = pipeWalker.pipes.collectFirst {
    case (point, pipe) if pipe.symbol == startPipe.symbol => point
  }.getOrElse(throw new IllegalArgumentException("no Start position found in the file"))

  val path = startPoint +: pipeWalker.getPath(startPoint)

  def interiorPoints(path: IndexedSeq[Point]): Double = {
    // https://en.wikipedia.org/wiki/Shoelace_formula
    val last = path.indices.last
    val a = for {
      i <- path.indices
      next = if (i + 1 > last) 0 else i + 1
      prev = if (i - 1 < 0) last else i - 1
      p = path(i)
      pNext = path(next)
      pPrev = path(prev)
    } yield p.x * (pNext.y - pPrev.y)

    abs(a.sum / 2) // depending on for-loop orientation this could be a negative (still correct in absolute value)
  }

  // https://en.wikipedia.org/wiki/Pick%27s_theorem
  val interiorSize = interiorPoints(path.toIndexedSeq)
  val boundarySize = path.size
  val areaOfPolygon = interiorSize - boundarySize / 2 + 1

  Console.println(areaOfPolygon)
}
