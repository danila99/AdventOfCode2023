import CubeConundrum.Game
import CubeConundrum.Round._

object Puzzle02CubeConundrum1 extends App with inputFileArgs {
  val ids = for {
    (line, index) <- getLines.zipWithIndex
    if Game(line).rounds.forall(_.valid)
  } yield index + 1

  Console.println(ids.sum)
}

object CubeConundrum {
  private def splitInTwo(delimiter: String, target: String): (String, String) = target.split(delimiter).toSeq match {
    case Seq(one, two) => one -> two
    case _ => throw new IllegalArgumentException(s"cannot split on [$delimiter], target: $target")
  }

  final case class Game(rounds: Seq[Round]) {
    def minimalColors: Round = Round(
      rounds.map(_.green).max,
      rounds.map(_.red).max,
      rounds.map(_.blue).max,
    )
  }

  object Game {
    def apply(line: String): Game = {
      val (_, roundsStr) = splitInTwo(": ", line)

      val rounds = for {
        roundStr <- roundsStr.split("; ")
        colors = roundStr.split(", ")
      } yield Round(colors)

      Game(rounds)
    }
  }

  final case class Round(green: Int, red: Int, blue: Int) {
    def valid: Boolean = red <= redLimit && green <= greenLimit && blue <= blueLimit
    def power: Int = green * red * blue
  }

  object Round {
    private val redLimit = 12
    private val greenLimit = 13
    private val blueLimit = 14

    def apply(colors: Seq[String]): Round = {
      val colorsMap: Map[String, Int] = (for {
        colorStr <- colors
        (n, c) = splitInTwo(" ", colorStr)
      } yield (c, n.toInt)).toMap

      Round(colorsMap.getOrElse("green", 0), colorsMap.getOrElse("red", 0), colorsMap.getOrElse("blue", 0))
    }
  }
}
