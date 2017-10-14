package scalan

import org.scalactic.TripleEquals
import org.scalatest._
import org.scalatest.words.ResultOfStringPassedToVerb
import org.scalatest.{FlatSpec, Inside, Matchers}

import scalan.util.FileUtil

/**
 * Standard base class for most test suites. See BaseNestedTests and BaseShouldTests for alternatives
 *
 * See <a>http://doc.scalatest.org/2.2.4/#org.scalatest.FunSuite</a>.
 */
abstract class BaseTests extends FunSuite with TestUtils

/**
 * Standard base class for test suites with nested tests.
 *
 * See <a>http://doc.scalatest.org/2.2.4/#org.scalatest.FunSpec</a>.
 */
abstract class BaseNestedTests extends FunSpec with TestUtils

/**
 * See <a>http://doc.scalatest.org/2.2.4/#org.scalatest.FlatSpec</a>.
 */
abstract class BaseShouldTests extends FlatSpec with TestUtils {
  protected final class InAndIgnoreMethods2(resultOfStringPassedToVerb: ResultOfStringPassedToVerb) {

    import resultOfStringPassedToVerb.rest
    val _inner = new InAndIgnoreMethods(resultOfStringPassedToVerb)
    def beArgFor(testFun: String => Unit) {
      _inner.in(testFun(rest.trim))
    }
  }

  protected implicit def convertToInAndIgnoreMethods2(resultOfStringPassedToVerb: ResultOfStringPassedToVerb) =
    new InAndIgnoreMethods2(resultOfStringPassedToVerb)

}
