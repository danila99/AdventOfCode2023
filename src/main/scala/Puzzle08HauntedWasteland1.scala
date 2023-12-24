import HauntedWasteland._

import scala.annotation.tailrec

object Puzzle08HauntedWasteland1 extends App with inputFileArgs {
  val instructions: IndexedSeq[Char] = getLines.head.toIndexedSeq
  val network: Map[NodeKey, Node] = NetworkLoader.parse(getLines.drop(2))

  Console.println(NetworkWalker.countSteps(network, instructions))
}

object HauntedWasteland {
  type NodeKey = String

  final case class Node(left: NodeKey, right: NodeKey) {
    def next(instruction: Char): NodeKey = instruction match {
      case 'L' => left
      case 'R' => right
      case _ => throw new IllegalArgumentException(s"unknown instruction: $instruction")
    }
  }

  object NetworkLoader {
    import scala.util.matching.Regex

    val nodesPattern: Regex = """([A-Z]){3}""".r

    def parse(lines: Seq[String]): Map[NodeKey, Node] =
      lines.map { line =>
        nodesPattern.findAllMatchIn(line).map(_.matched).toSeq match {
          case Seq(key, left, right) => key -> Node(left, right)
          case _ => throw new IllegalArgumentException(s"unknown network line: $line")
        }
      }.toMap
  }

  object NetworkWalker {
    private val firstNode: NodeKey = "AAA"
    private val finalNode: NodeKey = "ZZZ"

    def countSteps(network: Map[NodeKey, Node], instructions: IndexedSeq[Char]): Long = {
      @tailrec
      def walk(nodeKey: NodeKey, instructionIndex: Int, stepsTaken: Long): Long = {
        val instruction = instructions(instructionIndex % instructions.size) // '%' helps with an index overflow

        network.get(nodeKey) match {
          case Some(node) if node.next(instruction) == finalNode => stepsTaken
          case Some(node) => walk(node.next(instruction), instructionIndex + 1, stepsTaken + 1)
          case None => throw new IllegalArgumentException(s"no Node by NodKey: $nodeKey")
        }
      }

      walk(firstNode, instructionIndex = 0, stepsTaken = 1)
    }
  }
}