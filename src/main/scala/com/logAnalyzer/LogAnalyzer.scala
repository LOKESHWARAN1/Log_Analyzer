package com.logAnalyzer
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.lang3.time.StopWatch

import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer

class LogAnalyzer extends ProcessLogFile with LazyLogging {

  /** given the log file path and performing the operation and return the csv file.
    * @param logFilePath give the log file path.
    * @param outputCSVFilePath give the output log file.
    * @return log file analyze return the csv file.
    */
  def analyzeLogFile(
      logFilePath: String,
      outputCSVFilePath: String
  ): Any = {
    val stopWatch = new StopWatch()
    stopWatch.start()
    logger.info("analyzeLogFile()::entry")
    val isFileExtension = checkFileExtension(logFilePath)
    if (isFileExtension) {
      val fileContent = readLogFile(logFilePath)
      fileContent match {
        case "file not found given path." =>
          "file not found given path."
        case "Exception the read log file." =>
          "Exception the read log file."
        case _ =>
          val csvFilePath = createCSVFilePath(outputCSVFilePath)
          csvFilePath match {
            case "give the valid output csv path. Example of valid path=> /home/profile/IdeaProjects/Log_Analyzer/logFile/" =>
              "give the valid output csv path. Example of valid path is => /home/profile/IdeaProjects/Log_Analyzer/logFile/"
            case _ =>
              val jobPatternMatch = jobPatternMatching(fileContent)
              jobPatternMatch.map(x => x._1).mkString("") match {
                case "Process name and time length is not same." =>
                  "Process name and time length is not same."
                case _ =>
                  val uniqueStr = strUniqueValue(jobPatternMatch)
                  val processTimeMin = processMinValue(jobPatternMatch)
                  val processTimeMax = processMaxValue(jobPatternMatch)
                  val processTimeMean = processMeanValue(jobPatternMatch)
                  val processCount = getProcessRunningCount(jobPatternMatch)
                  val csvFileConvert =
                    summariseLogContent(
                      csvFilePath,
                      uniqueStr,
                      processCount,
                      processTimeMin,
                      processTimeMax,
                      processTimeMean
                    )
                  csvFileConvert match {
                    case "Data length is not Equal to Process name,Process Minimum Time and Process Maximum Time" =>
                      "Data length is not Equal to Process name,Process Minimum Time and Process Maximum Time"
                    case _ =>
                      stopWatch.stop()
                      logger.info(
                        "analyzeLogFile()::normal_exit | " + stopWatch
                          .getTime() + " ms "
                      )
                      s"CSV File Is Created Successfully. The File Path is $csvFilePath"
                  }
              }
          }
      }
    } else {
      stopWatch.stop()
      logger.debug(
        "analyzeLogFile()::normal_exit | " + stopWatch
          .getTime() + " ms "
      )
      "Invalid log file or file path"
    }

  }

  /** Given the unique Process of task name and Process of task taking time of minimum,maximum and average the data write to csv file.
    * @param uniqueStr process of task unique name.
    * @param miniValue process of task taking minimum time.
    * @param maxValue process of task taking maximum time.
    * @param meanValue process of task taking total time of meanValue.
    * @return perform the operation return the csv file.
    */
  protected def summariseLogContent(
      csvFilePath: String,
      uniqueStr: Array[String],
      processCount: Array[Int],
      miniValue: ArrayBuffer[Int],
      maxValue: ArrayBuffer[Int],
      meanValue: ArrayBuffer[Double]
  ): Any = {
    val stopWatch = new StopWatch()
    stopWatch.start()
    logger.info(
      "summariseLogContent(uniqueStr,miniValue,maxValue,meanValue)::entry"
    )
    val csvFileCreate = new PrintWriter(csvFilePath)
    var count = 1
    var strCSV = ""
    try {
      for (i <- uniqueStr.indices) {
        strCSV +=
          count + "\t" + "PROCESS NAME : " + "\t" + uniqueStr(i) + "\n" + "\t" +
            "PROCESS RUNNING TOTAL COUNT : " + "\t" + processCount(
              i
            ) + "\n" + "\t" +
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
        return "Data length is not Equal to Process name,Process Minimum Time and Process Maximum Time"
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
//object LogAnalyzerObject extends App {
//  val logAnalyzerObject = new LogAnalyzer
//  println(
//    logAnalyzerObject
//      .analyzeLogFile(
//        logFilePath =
//          "/home/lokeshwaranm/snap/teams/6/Downloads/expert-system (1).log",
//        "/home/lokeshwaranm/Documents/"
//      )
//  )
//
//}
