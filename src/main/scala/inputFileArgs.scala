import scala.io.Source

trait inputFileArgs {
  def getLines: Seq[String] = {
    val fileNameFromArgs = args(0)
    val bufferedSource = Source.fromFile(fileNameFromArgs)
    val lines = bufferedSource.getLines().toSeq
    bufferedSource.close()
    lines
  }

  protected def args: Array[String]
}
