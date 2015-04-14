package scalan

import scala.language.reflectiveCalls
import scalan.common.{SegmentsDslExp, CommonExamples, ViewExamples}

class ViewTests extends BaseTests { suite =>

  test("LambdaResultHasViews") {
    val ctx = new ViewTestsCtx(this, "LambdaResultHasViews")
                   with ViewExamples with CommonExamples with SegmentsDslExp
    import ctx._
    testLambdaResultHasViews("t1", t1, element[(Int,Int)])
    testLambdaResultHasViews("t2", t2, element[(Int,Int)])
    testLambdaResultHasViews("t3", t3)
    testLambdaResultHasViews("t4", t4)
    testLambdaResultHasViews("t5", t5)
    testLambdaResultHasViews("t7", t7)
    testLambdaResultHasViews("t8", t8)
    testLambdaResultHasViews("v1", v1, element[(Int,Int)])
    testLambdaResultHasViews("v2", v2, element[((Int,Int),(Int, Int))])
    testLambdaResultHasViews("v3", v3, element[((Int,Int), Int)])
    testLambdaResultHasViews("v4", v4, element[(Int, (Int,Int))])
    testLambdaResultHasViews("v5", v5)
    testLambdaResultHasViews("v6", v6)
    testLambdaResultHasViews("v7", v7)
    testLambdaResultHasViews("v8", v8, element[((Int,Int) | Unit)])
    testLambdaResultHasViews("v9", v9, element[((Int,Int) | (Int,Int))])
  }

  test("LambdaResultHasViews_Sums") {
    val ctx = new ViewTestsCtx(this, "LambdaResultHasViews")
                   with SegmentsDslExp {
      lazy val v1 = fun { (in: Rep[Unit]) => in.asLeft[Slice] }
      lazy val v2 = fun { (in: Rep[(Int,Int)]) => SumView(in.asRight[Unit])(identityIso[Unit], isoSlice) }
    }
    import ctx._
    testLambdaResultHasViews("v1", v1, element[Unit | (Int,Int)])
    testLambdaResultHasViews("v2", v2, element[Unit | (Int,Int)])
    emit("v2", v2)

  }

}
