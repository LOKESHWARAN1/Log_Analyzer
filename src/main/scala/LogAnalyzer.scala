import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.lang3.time.StopWatch

import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer

class LogAnalyzer extends ProcessLogFile with LazyLogging {

  /** given the log file path and performing the operation and return the csv file.
    * @param logFilePath give the log file path.
    */
  def analyzeLogFile(
      logFilePath: String
  ): Any = {
    val stopWatch = new StopWatch()
    stopWatch.start()
    logger.info("analyzeLogFile()::entry")
    val isFileExtension = checkFileExtension(logFilePath)
    if (isFileExtension) {
      val contentFile = readLogFile(logFilePath)
      val jobPatternMatch = jobPatternMatching(contentFile)
      val uniqueStr = strUniqueValue(jobPatternMatch)
      val processMin = processMinValue(jobPatternMatch)
      val processMax = processMaxValue(jobPatternMatch)
      val processMean = processMeanValue(jobPatternMatch)
      val csvFileConvert =
        summariseLogContent(uniqueStr, processMin, processMax, processMean)
      contentFile match {
        case "file not found given path." =>
          return "file not found given path."
        case "Exception the read log file." =>
          return "Exception the read log file."
        case _ =>
          jobPatternMatch.map(x => x._1).mkString("") match {
            case "Process name and time length is not same." =>
              return "Process name and time length is not same."
            case _ => uniqueStr

          }
      }
    } else {
      return "Invalid log file or file path"
    }
//    if (fileExtension) {
//      stopWatch.stop()
//      logger.info(
//        "analyzeLogFile()::normal_exit | " + stopWatch
//          .getTime() + " ms "
//      )
//    } else {
//      stopWatch.stop()
//      logger.debug(
//        "analyzeLogFile()::normal_exit | " + stopWatch
//          .getTime() + " ms "
//      )
//      return "Invalid log file or file path"
//    }
    csvFileConvert
  }

  /** Given the unique Process of task name and Process of task taking time of minimum,maximum and average the data write to csv file.
    * @param uniqueStr process of task unique name.
    * @param miniValue process of task taking minimum time.
    * @param maxValue process of task taking maximum time.
    * @param meanValue process of task taking total time of meanValue.
    */
  def summariseLogContent(
      uniqueStr: ArrayBuffer[String],
      miniValue: ArrayBuffer[Int],
      maxValue: ArrayBuffer[Int],
      meanValue: ArrayBuffer[Double]
  ): Any = {
    val stopWatch = new StopWatch()
    stopWatch.start()
    logger.info(
      "summariseLogContent(uniqueStr,miniValue,maxValue,meanValue)::entry"
    )
    val csvFileCreate = new PrintWriter(
      "logFile/log-analyzer.csv"
    )
    var count = 1
    var strCSV = ""
    try {
      for (i <- uniqueStr.indices) {
        strCSV +=
          count + "\t" + "PROCESS NAME : " + "\t" + uniqueStr(i) + "\n" + "\t" +
            "PROCESS TAKING MINIMUM TIME : " + "\t" + miniValue(
              i
            ) + "\n" + "\t" +
            "PROCESS TAKING MAXIMUM TIME : " + "\t" + maxValue(
              i
            ) + "\n" + "\t" +
            "PROCESS TAKING AVERAGE TIME : " + "\t" + meanValue(i) + "\n\n"
        count += 1
      }
    } catch {
      case a: ArrayIndexOutOfBoundsException =>
        stopWatch.stop()
        logger.error(
          "summariseLogContent(uniqueStr,miniValue,maxValue,meanValue)::error | " + stopWatch
            .getTime() + " ms "
        )
        return "Values of Data Index is mismatch."
    }
    csvFileCreate.write(strCSV)
    csvFileCreate.close()
    stopWatch.stop()
    logger.info(
      "summariseLogContent(uniqueStr,miniValue,maxValue,meanValue)::normal_exit | " + stopWatch
        .getTime() + " ms "
    )
    csvFileCreate
  }
}
object LogAnalyzerObject extends App {
  val logAnalyzerObject = new LogAnalyzer
  println(
    logAnalyzerObject
      .analyzeLogFile(logFilePath = "logFile/expert-system.log")
//      ._1
//      .length
//      .mkString("Array(", ", ", ")")
//      .length
  )

}
