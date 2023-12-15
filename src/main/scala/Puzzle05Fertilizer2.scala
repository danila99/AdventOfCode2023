import Fertilizer._

import scala.annotation.tailrec

object Puzzle05Fertilizer2 extends App with inputFileArgs with CommonUtils {
  @tailrec
  def getIntervals(nums: Seq[BigInt], initial: Seq[Interval]): Seq[Interval] = nums.size match {
    case s if s < 2 => initial
    case _ =>
      val rest = nums.drop(2)
      val two = nums.take(2).toIndexedSeq
      getIntervals(rest, initial.appended(Interval(two(0), two(1))))
  }

  @tailrec
  def findSeed(location: BigInt): BigInt = getSeed(location, allReverseMaps) match {
    case seed if intervals.exists(_.contains(seed)) => location
    case seed if location >= maxDestination => location
    case _ =>
      Console.println(location) // logging the progress for the long runs
      findSeed(location + one)
  }

  val numbers: Seq[BigInt] = for {
    line <- getLines.headOption.toSeq
    s <- numbersPattern.findAllMatchIn(line).map(_.matched)
  } yield BigInt(s)

  val intervals = getIntervals(numbers, Seq.empty)
  val allReverseMaps = readAlmanacMaps(getLines.tail, Seq.empty).reverse.toList
  val maxDestination = BigInt("4392422582")
  val one = BigInt(1)

  Console.println(s"found: ${findSeed(BigInt(0))}")
}
