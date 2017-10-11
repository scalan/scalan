package scalan

import scalan.util.FileUtil
import org.scalactic.TripleEquals
import org.scalatest.{Inside, Matchers, Suite}

/**
  * Created by slesarenko on 11/10/2017.
  */
trait TestUtils extends Suite with Matchers with Inside with TripleEquals {
  def testOutDir = "test-out"

  def testSuffixes = Seq("Suite", "Tests", "It", "_")

  lazy val prefix = {
    val suiteName = testSuffixes.foldLeft(getClass.getName)(_.stripSuffix(_))
    val pathComponents = suiteName.split('.')
    FileUtil.file(testOutDir, pathComponents: _*)
  }

  /* if runs in continuous integration environment */
  def isCI = sys.env.get("CI").flatMap(toBoolean).getOrElse(false)
  private def toBoolean(s: String): Option[Boolean] =
    scala.util.Try(s.toBoolean).toOption
  def pendingOnCI(): Unit = if (isCI) { pending }

  private val _currentTestName = new ThreadLocal[String]

  override def withFixture(test: NoArgTest) = {
    _currentTestName.set(test.name)
    val outcome = super.withFixture(test)
    _currentTestName.set(null)
    outcome
  }

  protected def currentTestName: String = {
    val testName = _currentTestName.get()
    assert(testName != null, "currentTestName called outside a test")
    testName
  }

  protected def currentTestNameAsFileName: String = FileUtil.cleanFileName(currentTestName)
}
