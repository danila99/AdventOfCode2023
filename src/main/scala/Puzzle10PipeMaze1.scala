import PipeMaze._

object Puzzle10PipeMaze1 extends App with inputFileArgs {
  val pipes = PipeWalker.readMap(getLines)

  val startPipe = pipes.values.collectFirst {
    case p if p.exits.size == 4 => p
  }.getOrElse(throw new IllegalArgumentException("no Start position found in the file"))

  // look for the correct starting 'exit' in the input file. I was too lazy to find it with the code.
  // We are guaranteed to have two exits from the start pipe by the rules, pick any.
  val throughExit = East

  PipeWalker.drawMap(pipes) // optional, will draw something like:
  //  ░░╔╗░
  //  ░╔╝║░
  //  ▉╝░╚╗
  //  ║╔══╝
  //  ╚╝░░░

  val count = PipeWalker.count(pipes, startPipe, throughExit) / 2
  Console.println(count)
}

object PipeMaze {
  import scala.annotation.tailrec

  final case class Point(x: Int, y: Int)

  type Exit = (Int, Int) // to denote (xOffset, yOffset)

  val North: Exit = (0, -1)
  val South: Exit = (0, 1)
  val East: Exit = (1, 0)
  val West: Exit = (-1, 0)

  final case class Pipe(symbol: Char, point: Point, exits: Seq[Exit]) {
    override def toString: String = symbol.toString
  }

  object Pipe {
    def apply(symbol: Char, point: Point): Pipe = symbol match {
      case '|' => Pipe('\u2551', point, Seq(North, South))
      case '-' => Pipe('\u2550', point, Seq(East, West))
      case 'L' => Pipe('\u255A', point, Seq(North, East))
      case 'J' => Pipe('\u255D', point, Seq(North, West))
      case '7' => Pipe('\u2557', point, Seq(West, South))
      case 'F' => Pipe('\u2554', point, Seq(East, South))
      case '.' => Pipe('\u2591', point, Seq.empty)
      case 'S' => Pipe('\u2589', point, Seq(North, South, East, West))
    }
  }

  object PipeWalker {
    def readMap(lines: Seq[String]): Map[Point, Pipe] = (for {
      (line, y) <- lines.iterator.zipWithIndex
      (char, x) <- line.iterator.zipWithIndex
      point = Point(x, y)
    } yield point -> Pipe(char, point)).toMap

    def drawMap(pipes: Map[Point, Pipe]): Unit = {
      val totalColumns = pipes.keys.map(_.x).max
      val totalLines = pipes.keys.map(_.y).max

      (0 to totalLines).foreach { y =>
        (0 to totalColumns).foreach(x => Console.print(pipes(Point(x, y))))
        Console.println
      }
    }

    def count(pipes: Map[Point, Pipe], start: Pipe, through: Exit): Int = {
      @tailrec def walk(from: Point, exit: Exit, path: List[Pipe]): List[Pipe] = {
        val (xOffset, yOffset) = exit
        val nextPoint = Point(from.x + xOffset, from.y + yOffset)
        val nextPipe = pipes.getOrElse(nextPoint, throw new IllegalArgumentException(s"no next pipe at $nextPoint"))

        nextPipe match {
          case pipe if pipe == start => path
          case pipe =>
            val nextExit: Exit = (pipe.exits :+ exit).foldLeft((0, 0)) {
              case ((x1, y1), (x2, y2)) => (x1 + x2, y1 + y2)
            }
            walk(nextPoint, nextExit, path :+ pipe)
        }
      }

      val path = walk(start.point, through, List.empty)
      path.size + 1 // adding 1 since Start pipe is not counted by the walk function
    }
  }
}
