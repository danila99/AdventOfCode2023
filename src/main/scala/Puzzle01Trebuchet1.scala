object Puzzle01Trebuchet1 extends App with inputFileArgs {
  val digits = Map(
    '0' -> 0,
    '1' -> 1,
    '2' -> 2,
    '3' -> 3,
    '4' -> 4,
    '5' -> 5,
    '6' -> 6,
    '7' -> 7,
    '8' -> 8,
    '9' -> 9,
  )

  def findDigit(line: String): Int = line.toCharArray.collectFirst {
    case c if digits.contains(c) => digits(c)
  }.getOrElse(throw new IllegalArgumentException(s"line does not have a digit: $line"))

  val numbers = getLines.map { l =>
    val digit1 = findDigit(l)
    val digit2 = findDigit(l.reverse)
    digit1 * 10 + digit2
  }

  Console.println(numbers.sum)
}
