object Puzzle01Trebuchet2 extends App with inputFileArgs {
  val digits = Map(
    "0" -> 0,
    "1" -> 1,
    "2" -> 2,
    "3" -> 3,
    "4" -> 4,
    "5" -> 5,
    "6" -> 6,
    "7" -> 7,
    "8" -> 8,
    "9" -> 9,
    "zero" -> 0,
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
  )

  val reverseDigits = digits.map { case (k, v) => k.reverse -> v }

  val numbers = getLines.map { l =>
    val digit1 = findDigit(l, digits)
    val digit2 = findDigit(l.reverse, reverseDigits)
    digit1 * 10 + digit2
  }

  def findDigit(line: String, digitsMap: Map[String, Int]): Int = {
    for (i <- 0 until line.length) {
      val shorterLine = line.drop(i)
      for (k <- digitsMap.keys)
        if (shorterLine.startsWith(k))
          return digitsMap(k)
    }

    throw new IllegalArgumentException(s"line does not have a digit: $line")
  }

  Console.println(numbers.sum)
}
