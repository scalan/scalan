package tests.performance

import org.scalameter.api._
import scalan.ScalanDsl

trait BaseBenchmark extends PerformanceTest with ScalanDsl {
  lazy val warmer = new Executor.Warmer.Default
  
  lazy val aggregator = Aggregator.min
  
  lazy val measurer = new Measurer.Default
  
  lazy val executor = SeparateJvmsExecutor(
    warmer,
    aggregator,
    measurer)
  
  lazy val reporter = new LoggingReporter
  
  lazy val persistor = Persistor.None
}