package scalan.performance

import org.scalameter.api._
import scalan.Scalan
import org.scalameter.reporting.RegressionReporter
import java.io.File

import scalan.util.FileUtil

trait BaseRegressionTest extends PerformanceTest.OnlineRegressionReport {
  lazy val persistFile = FileUtil.file("perf-out", "history", this.getClass.getSimpleName)
  override lazy val persistor = {
    persistFile.mkdirs()
    new SerializationPersistor(persistFile)
  }
  override lazy val executor = LocalExecutor(warmer, aggregator, measurer)
}