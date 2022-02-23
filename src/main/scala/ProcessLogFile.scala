import org.apache.commons.lang3.time.StopWatch

import java.io.FileNotFoundException
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.io.File
import scala.io.Source.fromFile
import com.typesafe.scalalogging.LazyLogging

class ProcessLogFile extends LazyLogging {

  /** Given the file path and check the file extension.
    * @param filePath give the log file path.
    * @return is return true or false.
    */
  protected def checkFileExtension(filePath: String): Boolean = {
    val stopWatch = new StopWatch()
    stopWatch.start()
    logger.info("checkFileExtension(filePath)::entry")
    if (File(filePath).exists && filePath.endsWith(".log")) {
      stopWatch.stop()
      logger.info(
        "checkFileExtension(filePath)::normal_exit | " + stopWatch
          .getTime() + " ms "
      )
      return true
    }
    stopWatch.stop()
    logger.info(
      "checkFileExtension(filePath)::normal_exit | " + stopWatch
        .getTime() + " ms "
    )
    false
  }

  /** That output CSV file path check valid or not.
    * @param outputCSVFilePath write the output in csv file give the csv file path.
    * @return that file path check is valid and create the file path
    */
  protected def createCSVFilePath(outputCSVFilePath: String): String = {
    val stopWatch = new StopWatch()
    stopWatch.start()
    logger.info("createCSVFilePath(outputCSVFilePath)::entry")
    var filePath = outputCSVFilePath
    if (File(filePath).exists) {
      if (filePath.endsWith(".csv")) {
        var count = 1
        filePath = filePath
          .split("\\.csv")
          .map(x =>
            x
              + s"$count" + ".csv"
          )
          .mkString("")
        count += 1
        stopWatch.stop()
        logger.info(
          "createCSVFilePath(outputCSVFilePath)::normal_exit | " + stopWatch
            .getTime() + " ms "
        )
        filePath
      } else {
        var pathCreating = ""
        filePath
          .split("/")
          .foreach(x => if (!x.contains(".")) pathCreating += x + "/")
        filePath = pathCreating + "/log-analyzer.csv"
        stopWatch.stop()
        logger.debug(
          "createCSVFilePath(outputCSVFilePath)::normal_exit | " + stopWatch
            .getTime() + " ms "
        )
        filePath
      }
    } else {
      stopWatch.stop()
      logger.debug(
        "createCSVFilePath(outputCSVFilePath)::normal_exit | " + stopWatch
          .getTime() + " ms "
      )
      "give the valid output csv path. Example of valid path=> /home/profile/IdeaProjects/Log_Analyzer/logFile/"
    }
  }

  /** read that log file.
    * @param filePath give the log file path.
    */
  protected def readLogFile(filePath: String): String = {
    val stopWatch = new StopWatch()
    stopWatch.start()
    logger.info("readLogFile(filePath)::entry")
    stopWatch.stop()
    try {
      logger.info(
        "readLogFile(filePath)::normal_exit | " + stopWatch
          .getTime() + " ms "
      )
      val file = fromFile(filePath)
      file.mkString("")
    } catch {
      case f: FileNotFoundException =>
        logger.error("readLogFile :: error | " + stopWatch.getTime() + " ms ")
        "file not found given path."
      case e: Exception =>
        logger.error("readLogFile :: error | " + stopWatch.getTime() + " ms ")
        "Exception the read log file."
    }
  }

  /** process of task name and time matching.
    * @param strLogContent give the string of log content.
    */
  protected def jobPatternMatching(
      strLogContent: String
  ): ListBuffer[(String, Int)] = {
    val stopWatch = new StopWatch()
    stopWatch.start()
    logger.info("jobPatternMatching(strLogContent)::entry")
    val processNameTime = new ListBuffer[(String, Int)]()
    val jobPatternMatchingRegex =
      """[\w]+[(]([\w \-_,/=]+)?[)]+[:]+[\w]+[ |]+[0-9]+[ms ]+""".r
    val processNameMatchingRegex = """[\w]+[(]([\w \-_,/=]+)?[)]+""".r
    val processTimeMatchingRegex = """([0-9]+[ ms])""".r
    val processTimeRegex = """[0-9]+""".r

    val fullLineMatchString = jobPatternMatchingRegex
      .findAllMatchIn(strLogContent)
      .mkString("")

    val processName =
      processNameMatchingRegex.findAllMatchIn(fullLineMatchString).toArray

    val processTimes = processTimeRegex
      .findAllMatchIn(
        processTimeMatchingRegex
          .findAllMatchIn(fullLineMatchString)
          .mkString("")
      )
      .toArray
    try {
      for (i <- processName.indices) {
        processNameTime += (
          (
            processName(i).toString(),
            processTimes(i).toString().toInt
          )
        )
      }
    } catch {
      case a: ArrayIndexOutOfBoundsException =>
        stopWatch.stop()
        logger.error(
          "jobPatternMatching::error | " + stopWatch.getTime() + " ms "
        )
        processNameTime.clear()
        return processNameTime += (
          (
            "Process name and time length is not same.",
            1
          )
        )
    }
    stopWatch.stop()
    logger.info(
      "jobPatternMatching(strLogContent)::normal_exit | " + stopWatch
        .getTime() + " ms "
    )
    processNameTime
  }

  /** process of task names return the unique process of task names.
    * @param matchString process of task names and times.
    */
  protected def strUniqueValue(
      matchString: ListBuffer[(String, Int)]
  ): ArrayBuffer[String] = {
    val stopWatch = new StopWatch()
    stopWatch.start()
    logger.info("strUniqueValue(ListBuffer[(String,Int)])::entry")
    val processName = matchString.map(x => x._1)
    val uniqueNameArray = new ArrayBuffer[String]()
    for (i <- processName.indices) {
      if (!uniqueNameArray.contains(processName(i))) {
        uniqueNameArray += processName(i)
      }
    }
    stopWatch.stop()
    logger.info(
      "strUniqueValue(ListBuffer[(String,Int)])::normal_exit | " + stopWatch
        .getTime() + " ms "
    )
    uniqueNameArray
  }

  /** process of task taking minimum time.
    * @param matchString process of task names and times.
    */
  protected def processMinValue(
      matchString: ListBuffer[(String, Int)]
  ): ArrayBuffer[Int] = {
    val stopWatch = new StopWatch()
    stopWatch.start()
    logger.info("processMinValue(ListBuffer[(String,Int)])::entry")
    val uniqueStr = strUniqueValue(matchString)
    val minValues = new ArrayBuffer[Int]()
    val collection = new ArrayBuffer[Int]()
    uniqueStr.foreach { i => //compare uniqueStr and matchString.
      val uniStr = i
      matchString.foreach { j =>
        val matchCurrentString = j._1
        if (uniStr == matchCurrentString) {
          collection += j._2
        }
      }
      minValues += collection.min
      collection.clear()
    }
    stopWatch.stop()
    logger.info(
      "processMinValue(ListBuffer[(String,Int)])::normal_exit | " + stopWatch
        .getTime() + " ms "
    )
    minValues
  }

  /** process of task taking maximum time.
    * @param matchString process of task names and times.
    */
  protected def processMaxValue(
      matchString: ListBuffer[(String, Int)]
  ): ArrayBuffer[Int] = {
    val stopWatch = new StopWatch()
    stopWatch.start()
    logger.info("processMaxValue(ListBuffer[(String,Int)])::entry")
    val uniqueStr = strUniqueValue(matchString)
    val maxValues = new ArrayBuffer[Int]()
    val individualProcessTimeCollection = new ArrayBuffer[Int]()
    uniqueStr.foreach { i => //compare uniqueStr and matchString.
      val uniStr = i
      matchString.foreach { j =>
        val matchCurrentString = j._1
        if (uniStr == matchCurrentString) {
          individualProcessTimeCollection += j._2
        }
      }
      maxValues += individualProcessTimeCollection.max
      individualProcessTimeCollection.clear()
    }
    stopWatch.stop()
    logger.info(
      "processMaxValue(ListBuffer[(String,Int)])::normal_exit | " + stopWatch
        .getTime() + " ms "
    )
    maxValues
  }

  /** process of task taking average time.
    * @param matchString process of task name and times.
    */
  protected def processMeanValue(
      matchString: ListBuffer[(String, Int)]
  ): ArrayBuffer[Double] = {
    val stopWatch = new StopWatch()
    stopWatch.start()
    logger.info("processMeanValue(ListBuffer[(String,Int)])::entry")
    val uniqueStr = strUniqueValue(matchString)
    val meanValues = new ArrayBuffer[Double]()
    val collection = new ArrayBuffer[Int]()
    uniqueStr.foreach { i => //compare uniqueStr and matchString.
      val uniStr = i
      matchString.foreach { j =>
        val matchCurrentString = j._1
        if (uniStr == matchCurrentString) {
          collection += j._2
        }
      }
      meanValues += ((collection.sum).toDouble / collection.length.toDouble)
      collection.clear()
    }
    stopWatch.stop()
    logger.info(
      "processMeanValue(ListBuffer[(String,Int)])::normal_exit | " + stopWatch
        .getTime() + " ms "
    )
    meanValues
  }

}
