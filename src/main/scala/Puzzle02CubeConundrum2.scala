import CubeConundrum.Game
import CubeConundrum.Round._

object Puzzle02CubeConundrum2 extends App with inputFileArgs {
  val powers = getLines.map(line => Game(line).minimalColors.power)

  Console.println(powers.sum)
}