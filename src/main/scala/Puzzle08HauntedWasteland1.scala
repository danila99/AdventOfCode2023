import HauntedWasteland._

import scala.annotation.tailrec

object Puzzle08HauntedWasteland1 extends App with inputFileArgs {
  def countSteps(network: Map[Key, Node], instructions: IndexedSeq[Char]): Long = {
    val firstKey: Key = "AAA"
    val finalKey: Key = "ZZZ"

    @tailrec
    def walk(key: Key, instructionIndex: Int, stepsTaken: Long): Long = {
      val instruction = instructions(instructionIndex % instructions.size) // '%' helps with index overflow

      network.get(key) match {
        case Some(node) if node.next(instruction) == finalKey => stepsTaken
        case Some(node) => walk(node.next(instruction), instructionIndex + 1, stepsTaken + 1)
        case None => throw new IllegalArgumentException(s"no Node by key: $key")
      }
    }

    walk(firstKey, instructionIndex = 0, stepsTaken = 1)
  }

  val instructions: IndexedSeq[Char] = getLines.head.toIndexedSeq
  val network: Map[Key, Node] = NetworkLoader.parse(getLines.drop(2))

  Console.println(countSteps(network, instructions))
}

object HauntedWasteland {
  type Key = String

  final case class Node(left: Key, right: Key) {
    def next(instruction: Char): Key = instruction match {
      case 'L' => left
      case 'R' => right
      case _ => throw new IllegalArgumentException(s"unknown instruction: $instruction")
    }
  }

  object NetworkLoader {
    import scala.util.matching.Regex

    private val nodesPattern: Regex = """([A-Z]){3}""".r

    def parse(lines: Seq[String]): Map[Key, Node] =
      lines.map { line =>
        nodesPattern.findAllMatchIn(line).map(_.matched).toSeq match {
          case Seq(key, left, right) => key -> Node(left, right)
          case _ => throw new IllegalArgumentException(s"unknown network line: $line")
        }
      }.toMap
  }
}