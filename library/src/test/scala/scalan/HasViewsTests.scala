package scalan

import scala.language.reflectiveCalls
import scalan.common.SegmentsDslExp

class HasViewsTests extends BaseViewTests {

  test("HasViews") {
    val ctx = new ViewTestsCtx("HasViews") with SegmentsDslExp with ScalanCommunityDslExp
    import ctx._

    testNoViews(10)
    testNoViews(Pair(10,10))
    testNoViews(toRep(10).asLeft[Boolean])
    testNoViews(toRep(10).asRight[Boolean])
    testNoViews(SArray.empty[Int])
    testNoViews(SArray.empty[(Int,Boolean)])
    testNoViews(SArray.empty[(Int|Boolean)])

    testHasViews(toRep(10).asLeft[Interval], element[Int | (Int, Int)])
    testHasViews(toRep(10).asRight[Interval], element[(Int, Int) | Int])
    testNoViews(toRep(10).asRight[Segment])
    testHasViews(Interval(10,10).asRight[Segment], element[Segment | (Int, Int)])

    testHasViews(Interval(10,10), element[(Int, Int)])
    testHasViews(Pair(Interval(10,10), 1), element[((Int, Int),Int)])
    testHasViews(SArray.empty[Interval], element[Array[(Int, Int)]])

    // Lambda argument tests
    fun { x: Rep[Segment] =>
      testNoViews(x)
      testHasViews(Pair(Interval(10,10), x), element[((Int, Int),Segment)])
      x
    }
    fun { in: Rep[(Interval,Segment)] =>
      val Pair(x,y) = in
      testHasViews(x, element[(Int, Int)])
      testHasViews(in, element[((Int, Int), Segment)])
      x
    }

    // TypeWrappers
    val f1 =  fun { x: Rep[Seq[Int]] =>
      val res = SSeqImpl(x)
      testHasViews(res, element[Seq[Int]])
      res
    }
    emit("f1", f1)

    lazy val seqsSimpleMap = fun { x: Rep[Seq[Int]] =>
      val seqImp = SSeqImpl(x)
      testHasViews(seqImp, element[Seq[Int]])
      val res = seqImp.map({i: Rep[Int] => i+1})
      res.wrappedValueOfBaseType
    }
    emit("seqsSimpleMap", seqsSimpleMap)
  }
}
