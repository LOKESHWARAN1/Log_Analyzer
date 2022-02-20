import java.io.FileNotFoundException
import scala.collection.mutable.ListBuffer
import scala.reflect.io.File
import scala.io.Source.fromFile
import scala.util.matching.Regex

class ProcessLogFile {

  /** Given the file path and check the file extension.
    * @param filePath give the log file path.
    */
  def checkFileExtension(filePath: String): Boolean = {
    val file =
      File(filePath).exists
    if (file)
      if (filePath.endsWith(".log"))
        return true
    false
//    filePath.endsWith(".log")
//    file
  }

  /** read that log file.
    * @param filePath give the log file path.
    */
  protected def readLogFile(filePath: String): String = {
    try {
      fromFile(filePath).mkString("")
    } catch {
      case f: FileNotFoundException => s"file not found given path."
      case e: Exception             => "Exception the read log file."
    }
  }

  /** process of task name and time matching.
    * @param strLogContent give the string of log content.
    */
  def jobPatternMatching(strLogContent: String): String = {
    val jobPatternMatchingRegex =
      """[\w]+[(]([\w \-_,/]+)?[)]+[:]+[\w]+[ |]+[0-9]+[msMS ]+""".r
    val processNameMatchingRegex = """[\w]+[(]([\w \-_,/]+)?[)]+""".r
    val processTimeMatchingRegex = """[0-9 ]+[msMS]+""".r

    val fullLineMatchString = jobPatternMatchingRegex
      .findAllMatchIn(strLogContent)
      .mkString("")
    processNameMatchingRegex.findAllMatchIn(fullLineMatchString).mkString("")
    processTimeMatchingRegex.findAllMatchIn(fullLineMatchString).mkString("")
//      .mkString("") //.toArray
  }

  /** process of task names return the unique process of task names.
    * @param matchString process of task names and times.
    */
  def strUniqueValue(matchString: ListBuffer[(String, Int)]) = {}

  /** process of task taking minimum time.
    * @param matchString process of task names and times.
    */
  def processMinValue(matchString: ListBuffer[(String, Int)]) = {}

  /** process of task taking maximum time.
    * @param matchString process of task names and times.
    */
  def processMaxValue(matchString: ListBuffer[(String, Int)]) = {}

  /** process of task taking average time.
    * @param matchString process of task name and times.
    */
  def processMeanValue(matchString: ListBuffer[(String, Int)]) = {}
}
