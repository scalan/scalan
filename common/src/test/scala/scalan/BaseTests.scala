package scalan

import org.scalatest.words.ResultOfStringPassedToVerb
import org.scalatest.{FlatSpec, FunSuite, Inside, Matchers}

import scalan.util.FileUtil

trait TestsUtil {
  def testOutDir = "test-out"

  def testSuffixes = Seq("Suite", "Tests", "It", "_")

  lazy val prefix = {
    val suiteName = testSuffixes.foldLeft(getClass.getName)(_.stripSuffix(_))
    val pathComponents = suiteName.split('.')
    FileUtil.file(testOutDir, pathComponents: _*)
  }

  /* if runs in continious integration environment */
  def isCI = sys.env.get("CI").flatMap(toBoolean).getOrElse(false)
  private def toBoolean(s: String): Option[Boolean] =
    scala.util.Try(s.toBoolean).toOption
}

// TODO switch to FunSpec and eliminate duplication in test names (e.g. RewriteSuite)
abstract class BaseTests extends FunSuite with Matchers with Inside with TestsUtil

abstract class BaseShouldTests extends FlatSpec with Matchers with TestsUtil {
  protected final class InAndIgnoreMethods2(resultOfStringPassedToVerb: ResultOfStringPassedToVerb) {

    import resultOfStringPassedToVerb.rest
    val _inner = new InAndIgnoreMethods(resultOfStringPassedToVerb)
    /**
     * Supports the registration of tests in shorthand form.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "A Stack" must "pop values in last-in-first-out order" in { ... }
     *                                                        ^
     * </pre>
     *
     * <p>
     * For examples of test registration, see the <a href="FlatSpec.html">main documentation</a>
     * for trait <code>FlatSpec</code>.
     * </p>
     */
    def beArgFor(testFun: String => Unit) {
      _inner.in(testFun(rest.trim))
    }
  }

  protected implicit def convertToInAndIgnoreMethods2(resultOfStringPassedToVerb: ResultOfStringPassedToVerb) =
    new InAndIgnoreMethods2(resultOfStringPassedToVerb)

}
