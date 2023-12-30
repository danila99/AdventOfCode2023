import PipeMaze._

object Puzzle10PipeMaze1 extends App with inputFileArgs {
  // look for the correct starting 'exits' in the input file. I was too lazy to find it with the code.
  // We are guaranteed to have two exits from the Start pipe by the rules, add them to 'startPipe' value below.
  val startPipe: Pipe = Pipe('S', Seq(West, South))
  val pipeWalker = new PipeWalker(getLines, startPipe)

  val startPoint = pipeWalker.pipes.collectFirst {
    case (point, pipe) if pipe.symbol == startPipe.symbol => point
  }.getOrElse(throw new IllegalArgumentException("no Start position found in the file"))

  pipeWalker.drawMap() // optional, will draw something like:
  //  ░░╔╗░
  //  ░╔╝║░
  //  S╝░╚╗
  //  ║╔══╝
  //  ╚╝░░░

  val path = pipeWalker.getPath(startPoint)
  val count = path.size + 1 // adding 1 since Start pipe is not counted by the walk function
  Console.println(count / 2)
}

object PipeMaze {
  type Exit = (Int, Int) // to denote (xOffset, yOffset)

  val North: Exit = (0, -1)
  val South: Exit = (0, 1)
  val East: Exit = (1, 0)
  val West: Exit = (-1, 0)

  final case class Point(x: Int, y: Int) {
    def moveThrough(exit: Exit): Point = {
      val (xOffset, yOffset) = exit
      Point(x + xOffset, y + yOffset)
    }
  }

  final case class Pipe(symbol: Char, exits: Seq[Exit]) {
    override def toString: String = symbol.toString
  }

  object Pipe {
    def apply(symbol: Char): Pipe = symbol match {
      case '|' => Pipe('\u2551', Seq(North, South))
      case '-' => Pipe('\u2550', Seq(East, West))
      case 'L' => Pipe('\u255A', Seq(North, East))
      case 'J' => Pipe('\u255D', Seq(North, West))
      case '7' => Pipe('\u2557', Seq(West, South))
      case 'F' => Pipe('\u2554', Seq(East, South))
      case '.' => Pipe('\u2591', Seq.empty)
      case 'S' => throw new IllegalArgumentException("cannot create Start pipe like this")
      case _ => throw new IllegalArgumentException(s"cannot create pipe from: $symbol")
    }
  }

  final class PipeWalker(lines: Seq[String], startPipe: Pipe) {
    import scala.annotation.tailrec

    lazy val pipes: Map[Point, Pipe] = readMap(lines).toMap

    private def readMap(lines: Seq[String]): Iterator[(Point, Pipe)] = for {
      (line, y) <- lines.iterator.zipWithIndex
      (char, x) <- line.iterator.zipWithIndex
      point = Point(x, y)
    } yield {
      if (char == startPipe.symbol) point -> startPipe
      else point -> Pipe(char)
    }

    def drawMap(): Unit = {
      val totalColumns = pipes.keys.map(_.x).max
      val totalLines = pipes.keys.map(_.y).max

      (0 to totalLines).foreach { y =>
        (0 to totalColumns).foreach(x => Console.print(pipes(Point(x, y))))
        Console.println
      }
    }

    def getPath(start: Point): List[Point] = {
      @tailrec def walk(from: Point, exit: Exit, path: List[Point]): List[Point] = from.moveThrough(exit) match {
        case nextPoint if nextPoint == start => path
        case nextPoint =>
          val nextPipe = pipes.getOrElse(nextPoint, throw new IllegalArgumentException(s"no next pipe at $nextPoint"))
          val nextExit: Exit = (nextPipe.exits :+ exit).foldLeft((0, 0)) {
            case ((x1, y1), (x2, y2)) => (x1 + x2, y1 + y2)
          }
          walk(nextPoint, nextExit, path :+ nextPoint)
      }

      val through = startPipe.exits.head
      walk(start, through, List.empty)
    }
  }
}
