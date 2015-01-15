package scalan

import org.scalatest.{Matchers, FunSuite}
import org.scalatest.FlatSpec
import org.scalatest.words.ResultOfStringPassedToVerb

// TODO switch to FunSpec and eliminate duplication in test names (e.g. RewriteSuite)
class BaseTests extends FunSuite with Matchers

class BaseShouldTests extends FlatSpec with Matchers {

  protected final class InAndIgnoreMethods2(resultOfStringPassedToVerb: ResultOfStringPassedToVerb) {

    import resultOfStringPassedToVerb.verb
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