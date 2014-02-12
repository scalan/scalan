package tests.performance

import org.scalameter.api._
import scalan.ScalanDsl
import org.scalameter.reporting.RegressionReporter
import java.io.File

trait BaseRegressionTest extends PerformanceTest.Regression {
  lazy val persistFile = new File(s"perf-out${File.separator}history", this.getClass.getSimpleName)
  lazy val persistor = {
    persistFile.mkdirs()
    new SerializationPersistor(persistFile)
  }
  override lazy val executor = LocalExecutor(warmer, aggregator, measurer)
}