package tests.performance
import org.scalameter.api._

class CoreRegressionSuite extends BaseRegressionTest {
  include[PArrayBenchmarkStaged]
}