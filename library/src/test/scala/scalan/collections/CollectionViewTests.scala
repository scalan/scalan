package scalan.collections

import scala.language.reflectiveCalls
import scalan.common.SegmentsDslExp
import scalan.{ScalanCommunityDslExp, BaseViewTests}

class CollectionViewTests extends BaseViewTests {
  class Ctx(name: String) extends
    ViewTestsCtx(name) with CollectionsDslExp with ScalanCommunityDslExp with SegmentsDslExp

  test("LambdaResultHasViews") {
    val ctx = new Ctx("LambdaResultHasViews") {
      lazy val t1 = fun { (in: Rep[Array[Int]]) => CollectionOverArray(in) }
      lazy val t2 = fun { (in: Rep[Array[Int]]) => (CollectionOverArray(in), in.length) }
      lazy val t3 = fun { (in: Rep[Array[Int]]) => (CollectionOverArray(in), in) }
      lazy val t4 = fun { (in: Rep[Array[Int]]) => PairCollectionSOA(CollectionOverArray(in), CollectionOverList(in.toList)) }
    }
    import ctx._
    testLambdaResultHasViewsWithDataType("t1", t1, element[Array[Int]])
    testLambdaResultHasViewsWithDataType("t2", t2, element[(Array[Int], Int)])
    testLambdaResultHasViewsWithDataType("t3", t3, element[(Array[Int], Array[Int])])
    testLambdaResultHasViewsWithDataType("t4", t4, element[(Collection[Int], Collection[Int])])
  }

  test("getIsoByElem") {
    val ctx = new Ctx("LambdaResultHasViews_Sums")
    import ctx._

    testGetIso(element[Collection[Int]], element[Collection[Int]])
    testGetIso(element[Collection[Segment]], element[Collection[Segment]])
    //testGetIso(element[Collection[Interval]], element[Collection[(Int,Int)]])  // TODO Iso

    testGetIso(element[CollectionOverArray[Int]], element[Array[Int]])
    testGetIso(element[CollectionOverList[Int]], element[List[Int]])
    testGetIso(element[CollectionOverList[(Int,Int)]], element[List[(Int,Int)]])
    testGetIso(element[CollectionOverList[Segment]], element[List[Segment]])
    testGetIso(element[CollectionOverList[Interval]], element[List[(Int,Int)]])

    // wrappers
    testGetIso(element[Seq[Int]], element[Seq[Int]])
    testGetIso(element[Seq[(Int,Int)]], element[Seq[(Int, Int)]])
    testGetIso(element[Seq[Segment]], element[Seq[Segment]])
    //testGetIso(element[Seq[Interval]], element[Seq[(Int, Int)]])

    testGetIso(element[SSeq[Int]], element[Seq[Int]])
    testGetIso(element[SSeq[(Int,Int)]], element[Seq[(Int, Int)]])
    testGetIso(element[SSeq[Segment]], element[Seq[Segment]])
    //testGetIso(element[SSeq[Interval]], element[Seq[(Int, Int)]])

    testGetIso(element[SSeqImpl[Int]], element[Seq[Int]])

    testGetIso(element[Throwable], element[Throwable])
    testGetIso(element[SThrowable], element[Throwable])
    testGetIso(element[SThrowableImpl], element[Throwable])
  }
}
