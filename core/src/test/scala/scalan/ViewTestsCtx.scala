package scalan

import scalan.common.{ViewExamples, SegmentsDslExp, CommonExamples}
import org.scalatest.Assertions._

class ViewTestsCtx(suite: TestsUtil, testName: String)
  extends TestContext(suite, testName)
{
  def testLambdaResultHasViewsWithDataType[A,B](msg: String, f: Rep[A => B], expectedDataElem: Elem[_]) =
    _testLambdaResultHasViews(msg, f, Some(expectedDataElem))

  def testLambdaResultHasViews[A,B](msg: String, f: Rep[A => B]) =
    _testLambdaResultHasViews(msg, f, None)

  private def _testLambdaResultHasViews[A,B](msg: String, f: Rep[A => B], expectedDataElem: Option[Elem[_]]) = {
    val ok = f match {
      case LambdaResultHasViews(f, iso) =>
        expectedDataElem.isDefined && expectedDataElem.get == iso.eFrom
      case _ =>
        !expectedDataElem.isDefined
    }
    assert(ok, msg)
  }

  def testGetIso[From, To](e: Elem[To], eFromExpected: Elem[From]) = {
    val iso = getIsoByElem(e)
    assertResult(eFromExpected)(iso.eFrom)
  }

  def testHasViews[T](s: Rep[T], eExpected: Elem[_]) = {
    val HasViews(source, iso) = s
    assertResult(eExpected)(iso.eFrom)
    assertResult(eExpected)(source.elem)
  }

  def testNoViews[T](s: Rep[T]) = {
    s match {
      case HasViews(source, iso) =>
        assert(false, s"no views expected, but found ($source, $iso)")
      case _ => // ok
    }
  }


}
