import scala.util.matching.Regex

trait CommonUtils {
  protected def splitInTwo(delimiter: String, target: String): (String, String) = target.split(delimiter).toSeq match {
    case Seq(one, two) => one -> two
    case _ => throw new IllegalArgumentException(s"cannot split on [$delimiter], target: $target")
  }

  val numbersPattern: Regex = """\d+""".r
}
