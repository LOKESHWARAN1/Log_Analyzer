import scala.collection.mutable.ListBuffer
import scala.reflect.io.File

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
  def readLogFile(filePath: String) = {}

  /** process of task name and time matching.
    * @param strLogContent give the string of log content.
    */
  def jobPatternMatching(strLogContent: String) = {}

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
