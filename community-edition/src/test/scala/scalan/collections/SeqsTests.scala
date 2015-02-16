package scalan.collections

import scala.language.reflectiveCalls
import scalan._
import scalan.common.{SegmentsDslSeq, SegmentsDslExp, SegmentsDsl}

class SeqsTests extends BaseTests { suite =>
  trait SeqSimple extends ScalanDsl { self: ScalanCommunityDsl with SegmentsDsl =>
    lazy val tElem = element[Seq[Int]]
    lazy val defaultRep = tElem.defaultRepValue
    lazy val empty = SSeq.empty[Int]

    lazy val t1 = fun { (t: Rep[Seq[Int]]) => t }
    lazy val t2 = fun { (in: Rep[(Seq[Int],Int)]) => val Pair(t, i) = in; i +: t }
    lazy val t3 = fun { (in: Rep[(Seq[Segment],Segment)]) => val Pair(t, i) = in; i +: t }
    lazy val t4 = fun { (in: Rep[(Seq[Segment],Int)]) =>
      val Pair(ss, i) = in;
      ss.map(fun {s => s.shift(i)})
    }
    lazy val t5 = fun { (in: Rep[(Seq[Segment],Int)]) =>
      val Pair(ss, i) = in;
      ss.map(fun {s => s.shift(i).convertTo[Interval].toData})
    }
    lazy val t6 = fun { (in: Rep[Seq[(Int,Int)]]) =>
      in.map(fun {s => Interval(s)})
    }
    lazy val t7 = fun { (in: Rep[Seq[(Int,Int)]]) =>
      in.map(fun {s => Interval(s)}).map((s: Rep[Interval]) => s.toData)
    }
    lazy val t8 = fun { (in: Rep[List[Seq[Segment]]]) =>
      in.map(seq => seq.map(fun {s => s.shift(1)}))
    }
    lazy val t9 = fun { (in: Rep[Seq[List[Segment]]]) =>
      in.map(fun {xs => xs.map(s => s.shift(1)).reverse})
    }
    lazy val t10 = fun { (in: Rep[Array[(Int,Int)]]) =>
      SSeq(in.map(p => Interval(p))).map(fun { i => i.toData })
    }

    val e = element[Seq[Segment]]
  }

  test("basicTests") {
    val ctx = new TestContext(this, "basicTests") with SeqSimple with ScalanCommunityDslExp with SegmentsDslExp {
      def test() = { }
    }
    ctx.test
    ctx.emit("defaultRep", ctx.defaultRep)
    ctx.emit("empty", ctx.empty)
    ctx.emit("t1", ctx.t1)
    ctx.emit("t2", ctx.t2)
    ctx.emit("t3", ctx.t3)
  }

  test("Seq_of_domain_type") {
    val ctx = new TestContext(this, "Seq_of_domain_type") with SeqSimple with ScalanCommunityDslExp with SegmentsDslExp {
      def test() = { }
    }
    ctx.test
    ctx.emit("t4", ctx.t4)
    ctx.emit("t5", ctx.t5)
    ctx.emit("t6", ctx.t6)
    ctx.emit("t7", ctx.t7)
    ctx.emit("t8", ctx.t8)
    ctx.emit("t9", ctx.t9)
  }

  test("simpleHashsetSeq") {
    val ctx = new ScalanCtxSeq with  SeqSimple with  ScalanCommunityDslSeq with SegmentsDslSeq {
      def test() = {
        //assert(!isInlineThunksOnForce, "precondition for tests")
      }
    }
    ctx.test
    val d = ctx.defaultRep

    {
      val res = ctx.t2((Seq.empty[Int], 10))
      assertResult(Seq(10))(res)
//      val s: Seq[Int]   = null
//      s.toArray
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
