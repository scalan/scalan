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

}
