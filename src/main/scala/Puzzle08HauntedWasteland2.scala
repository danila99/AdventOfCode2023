import HauntedWasteland._

import scala.annotation.tailrec

object Puzzle08HauntedWasteland2 extends App with inputFileArgs {
  def countLoopSizes(network: Map[Key, Node], instructions: IndexedSeq[Char]): Seq[Long] = {
    def isFirstNode(key: Key): Boolean = key(2) == 'A'
    def isFinalNode(key: Key): Boolean = key(2) == 'Z'

    @tailrec
    def walk(key: Key, instructionIndex: Int, stepsTaken: Long): Long = {
      val instruction = instructions(instructionIndex % instructions.size) // '%' helps with an index overflow

      network.get(key) match {
        case Some(node) if isFinalNode(node.next(instruction)) => stepsTaken
        case Some(node) => walk(node.next(instruction), instructionIndex + 1, stepsTaken + 1)
        case None => throw new IllegalArgumentException(s"no Node by key: $key")
      }
    }

    val firstKeys = network.collect { case (key, _) if isFirstNode(key) => key }.toSeq
    Console.println(s"need to calculate loops: ${firstKeys.size}")

    firstKeys.map { firstKey =>
      walk(firstKey, instructionIndex = 0, stepsTaken = 1)
    }
  }

  @tailrec def gcd(a: Long, b: Long): Long =
    if (b == 0) a.abs
    else gcd(b, a % b)

  @tailrec def lcm(numbers: List[Long], result: Long): Long = numbers match {
    case Nil => result
    case n :: rest => lcm(rest, (n * result).abs / gcd(n, result))
  }

  val instructions: IndexedSeq[Char] = getLines.head.toIndexedSeq
  val network: Map[Key, Node] = NetworkLoader.parse(getLines.drop(2))
  val loopSizes = countLoopSizes(network, instructions)

  Console.println(loopSizes.mkString(", "))
  Console.println(s"answer (Least common multiple): ${lcm(loopSizes.toList, 1)}")
}
