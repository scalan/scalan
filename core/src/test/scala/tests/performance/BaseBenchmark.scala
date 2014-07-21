package tests.performance

import org.scalameter.Aggregator
import org.scalameter.api._

import scalan.ScalanDsl

trait BaseBenchmark extends PerformanceTest {
  lazy val warmer: Executor.Warmer = new Executor.Warmer.Default
  
  lazy val aggregator: Aggregator = Aggregator.min
  
  lazy val measurer: Executor.Measurer = new Measurer.Default
  
  lazy val executor: Executor = LocalExecutor(
    warmer,
    aggregator,
    measurer)
  
  lazy val reporter: Reporter = new LoggingReporter
  
  lazy val persistor: Persistor = Persistor.None
}
