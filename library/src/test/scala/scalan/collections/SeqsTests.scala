package scalan.collections

import scala.language.reflectiveCalls
import scalan._
import scalan.common.{SegmentsDslSeq, SegmentsDslExp, SegmentsDsl}

class SeqsTests extends BaseCtxTests {
  trait SeqSimple extends ScalanDsl { self: ScalanCommunityDsl with SegmentsDsl =>
    lazy val tElem = element[Seq[Int]]
    lazy val empty = SSeq.empty[Int]

    lazy val t1 = fun { (t: Rep[SSeq[Int]]) => t }
    lazy val t2 = fun { (in: Rep[(SSeq[Int],Int)]) => val Pair(t, i) = in; i +: t }
    lazy val t3 = fun { (in: Rep[(SSeq[Segment],Segment)]) => val Pair(t, i) = in; i +: t }
    lazy val t4 = fun { (in: Rep[(SSeq[Segment],Int)]) =>
      val Pair(ss, i) = in;
      ss.map(fun {s => s.shift(i)})
    }
    lazy val t5 = fun { (in: Rep[(SSeq[Segment],Int)]) =>
      val Pair(ss, i) = in;
      ss.map(fun {s => s.shift(i).convertTo[Interval].toData})
    }
    lazy val t6 = fun { (in: Rep[SSeq[(Int,Int)]]) =>
      in.map(fun {s => Interval(s)})
    }
    lazy val t7 = fun { (in: Rep[SSeq[(Int,Int)]]) =>
      in.map(fun {s => Interval(s)}).filter(fun {(s: Rep[Interval]) => s.length > 10}).map((s: Rep[Interval]) => s.toData)
    }
    lazy val t7_arr = fun { (in: Rep[Array[(Int,Int)]]) =>
      in.map(s => Interval(s)).filter((s: Rep[Interval]) => s.length > 10).map((s: Rep[Interval]) => s.toData)
    }
    lazy val t8 = fun { (in: Rep[List[SSeq[Segment]]]) =>
      in.map(seq => seq.map(fun {s => s.shift(1)}))
    }
    lazy val t9 = fun { (in: Rep[SSeq[List[Segment]]]) =>
      in.map(fun {xs => xs.mapBy(fun {s => s.shift(1)}).reverse})
    }
    lazy val t10 = fun { (in: Rep[Array[(Int,Int)]]) =>
      SSeq(in.map(p => Interval(p))).map(fun { i => i.toData })
    }
    lazy val t11 = fun { (in: Rep[Array[(Int,Int)]]) =>
      SSeq(in.map(p => Interval(p))).map(fun { i => i.length })
    }

    val e = element[SSeq[Segment]]
  }

  test("basicTests") {
    val ctx = new TestContext with SeqSimple with ScalanCommunityDslExp with SegmentsDslExp {
      def test() = { }
    }
    ctx.test
    ctx.emit("empty", ctx.empty)
    ctx.emit("t1", ctx.t1)
    ctx.emit("t2", ctx.t2)
    ctx.emit("t3", ctx.t3)
  }

  test("Seq_of_domain_type") {
    val ctx = new TestContext with SeqSimple with ScalanCommunityDslExp with SegmentsDslExp {
      def test() = { }
    }
    ctx.test
    ctx.emit("t4", ctx.t4)
    ctx.emit("t5", ctx.t5)
    ctx.emit("t6", ctx.t6)
    ctx.emit("t7", ctx.t7)
    ctx.emit("t7_arr", ctx.t7_arr)
    ctx.emit("t8", ctx.t8)
    ctx.emit("t9", ctx.t9)
    ctx.emit("t10", ctx.t10)
    ctx.emit("t11", ctx.t11)
  }

  test("IsosForSeq") {
    val ctx = new TestContext with SeqSimple with ScalanCommunityDslExp with SegmentsDslExp {
      def test() = { }
    }
    ctx.emit("t7", ctx.t7)
    ctx.emit("t7_arr", ctx.t7_arr)
  }

  test("simpleHashsetSeq") {
    val ctx = new ScalanCtxSeq with SeqSimple with ScalanCommunityDslSeq with SegmentsDslSeq {
      def test() = {
        //assert(!isInlineThunksOnForce, "precondition for tests")
      }
    }
    ctx.test
    import ctx._
    {
      val res = ctx.t2((SSeqImpl(Seq.empty[Int]), 10))
      assertResult(SSeqImpl(Seq(10)))(res)
      val s: Seq[Int]   = null
      //s.zip()

    }
//    {
//      val res = ctx.t3(10)
//      assertResult(Seq(10))(res)
//    }
//    {
//      val res = ctx.t4(Seq(10, 20, 30))
//      assertResult(Seq(11, 21, 31))(res)
//    }
//    {
//      val res = ctx.t7((Seq(10, 20, 30),0))
//      assertResult(60)(res)
//    }
  }
}
