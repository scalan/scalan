package scalan.collections

import scala.language.reflectiveCalls
import scalan.common.SegmentsDslExp
import scalan.{ScalanCommunityDslExp, ViewTestsCtx, BaseTests}

class CollectionViewTests extends BaseTests { suite =>

  test("LambdaResultHasViews") {
    val ctx = new ViewTestsCtx(this, "LambdaResultHasViews")
                  with CollectionsDslExp with ScalanCommunityDslExp {
      lazy val t1 = fun { (in: Rep[Array[Int]]) => BaseCollection(in) }
      lazy val t2 = fun { (in: Rep[Array[Int]]) => (BaseCollection(in), in.length) }
      lazy val t3 = fun { (in: Rep[Array[Int]]) => (BaseCollection(in), in) }
      lazy val t4 = fun { (in: Rep[Array[Int]]) => PairCollection(BaseCollection(in), ListCollection(in.toList)) }
    }
    import ctx._
    testLambdaResultHasViewsWithDataType("t1", t1, element[Array[Int]])
    testLambdaResultHasViewsWithDataType("t2", t2, element[(Array[Int], Int)])
    testLambdaResultHasViewsWithDataType("t3", t3, element[(Array[Int], Array[Int])])
    testLambdaResultHasViewsWithDataType("t4", t4, element[(Collection[Int], Collection[Int])])
  }

  test("getIsoByElem") {
    val ctx = new ViewTestsCtx(this, "LambdaResultHasViews_Sums")
                  with CollectionsDslExp with ScalanCommunityDslExp with SegmentsDslExp
    import ctx._

    testGetIso(element[Collection[Int]], element[Collection[Int]])
    testGetIso(element[Collection[Segment]], element[Collection[Segment]])
    //testGetIso(element[Collection[Interval]], element[Collection[(Int,Int)]])  // TODO Iso

    testGetIso(element[BaseCollection[Int]], element[Array[Int]])
    testGetIso(element[ListCollection[Int]], element[List[Int]])
    testGetIso(element[ListCollection[(Int,Int)]], element[List[(Int,Int)]])
    testGetIso(element[ListCollection[Segment]], element[List[Segment]])
    testGetIso(element[ListCollection[Interval]], element[List[(Int,Int)]])

    testGetIso(element[Seq[Int]], element[Seq[Int]])
    testGetIso(element[SSeq[Int]], element[SSeq[Int]])
    testGetIso(element[SSeqImpl[Int]], element[Seq[Int]])
  }
}
