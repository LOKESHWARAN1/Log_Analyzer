import org.scalatest.funsuite.AnyFunSuite
import com.logAnalyzer

class TestLogAnalyzer extends AnyFunSuite {
  val LogAnalyzers = new logAnalyzer.LogAnalyzer
  test("Analyze log file.") {
    LogAnalyzers.analyzeLogFile(
      "logFile/samplesTestCase.log",
      "logFile/"
    ) == "CSV File Is Created Successfully. The File Path is logFile//log-analyzer.csv"
  }
  test("Analyze log file.") {
    LogAnalyzers.analyzeLogFile(
      "logFile/samplesTestCase.log",
      "logFile/"
    ) == "CSV File Is Created Successfully. The File Path is logFile//log-analyzer.csv"
  }
//  test("Analyze log file.") {
//    LogAnalyzers.analyzeLogFile(
//      "logFile/samplesTestCase.pdf",
//      "logFile/"
//    ) == "Invalid log file or file path"
//  }
}
