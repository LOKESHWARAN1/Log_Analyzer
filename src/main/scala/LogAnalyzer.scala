import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.lang3.time.StopWatch

class LogAnalyzer extends ProcessLogFile with LazyLogging {

  /** given the log file path and performing the operation and return the csv file.
    * @param logFilePath give the log file path.
    */
  def analyzeLogFile(logFilePath: String): String = {
    val stopWatch = new StopWatch()
    stopWatch.start()
    logger.info("analyzeLogFile()::entry")
    val fileExtension = checkFileExtension(filePath = logFilePath)
    val readFile = readLogFile(logFilePath)
    val jobPatternMatch = jobPatternMatching(readFile)
//    if (fileExtension) {
//      readLogFile(logFilePath) match {
//        case "file not found given path." =>
//          return "file not found given path."
//        case "Exception the read log file." => "Exception the read log file."
//        case _                              => return "working"
//      }
//    } else {
//      return "Invalid log file or file path"
//    }
//    if (fileExtension) {
////      Thread.sleep(12000)
//      stopWatch.stop()
//      logger.info(
//        "analyzeLogFile()::normal_exit | " + stopWatch
//          .getTime() + " ms "
//      )
//      return "valid"
//    } else {
//      stopWatch.stop()
//      logger.debug(
//        "analyzeLogFile()::normal_exit | " + stopWatch
//          .getTime() + " ms "
//      )
//      "Invalid log file or file path"
//    }
//    readFile
    jobPatternMatch
  }

  /** Given the unique Process of task name and Process of task taking time of minimum,maximum and average the data write to csv file.
    * @param uniqueStr process of task unique name.
    * @param miniValue process of task taking minimum time.
    * @param maxValue process of task taking maximum time.
    * @param meanValue process of task taking total time of meanValue.
    */
  def summariseLogContent(
      uniqueStr: Array[String],
      miniValue: Array[Double],
      maxValue: Array[Double],
      meanValue: Array[Double]
  ): Unit = {}
}
object LogAnalyzerObject extends App {
  val logAnalyzerObject = new LogAnalyzer
  println(
    logAnalyzerObject
      .analyzeLogFile(logFilePath = "logFile/expert-system.log")
//      .mkString("Array(", ", ", ")")
      .length
  )

}
