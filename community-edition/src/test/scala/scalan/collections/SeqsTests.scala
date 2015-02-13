package scalan.collections

import scala.collection.immutable.Seq
import scala.language.reflectiveCalls
import scalan._
import scalan.common.{SegmentsDslSeq, SegmentsDslExp, SegmentsDsl}

class SeqsTests extends BaseTests { suite =>
  trait SeqSimple extends ScalanDsl { self: SeqsDsl with SegmentsDsl  =>
    lazy val tElem = element[Seq[Int]]
    lazy val defaultRep = tElem.defaultRepValue
    lazy val empty = SSeq.empty[Int]

    lazy val t1 = fun { (t: Rep[Seq[Int]]) => t }
    lazy val t2 = fun { (in: Rep[(Seq[Int],Int)]) => val Pair(t, i) = in; i +: t }
    lazy val t3 = fun { (in: Rep[(Seq[Segment],Segment)]) => val Pair(t, i) = in; i +: t }

    val e = element[Seq[Segment]]
//    lazy val t4 = fun { (t: Rep[Seq[Int]]) => t.map(fun { x => x + 1 }) }
//    lazy val t5 = fun { (in: Rep[(SSeq[Int],Int)]) => val Pair(t, i) = in; t + i }
//    lazy val t6 = fun { (t: Rep[(Seq[Int],Int)]) => {
//      t._1.map(fun { x => x + t._2 })
//    }}
//    lazy val t7 = fun { (t: Rep[(Seq[Int],Int)]) => {
//      t._1.fold(t._2)(fun { x => x._1 + x._2 })
//    }}

  }

  test("simpleHashsetStaged") {
    val ctx = new TestContext(this, "simpleHashsetStaged") with SeqSimple with SeqsDslExp with SegmentsDslExp {
      def test() = {
        //assert(!isInlineThunksOnForce, "precondition for tests")
        {
//TODO make this work (recognizer should deal with BaseElemEx)
//          val Def(Lambda(_, _, x, SThrowableMethods.getMessage(obj))) = t1
//          assert(x == obj)
        }
      }
    }

    ctx.test
    ctx.emit("defaultRep", ctx.defaultRep)
    ctx.emit("empty", ctx.empty)
    ctx.emit("t1", ctx.t1)
    ctx.emit("t2", ctx.t2)
    ctx.emit("t3", ctx.t3)
//    ctx.emit("t4", ctx.t4)
//    ctx.emit("t5", ctx.t5)
//    ctx.emit("t6", ctx.t6)
//    ctx.emit("t7", ctx.t7)
  }

  test("simpleHashsetSeq") {
    val ctx = new ScalanCtxSeq with  SeqSimple with SeqsDslSeq with SegmentsDslSeq {
      def test() = {
        //assert(!isInlineThunksOnForce, "precondition for tests")
        val s = Seq.empty[Int]
        //s.
      }
    }
    ctx.test
    val d = ctx.defaultRep

    {
      val res = ctx.t2((Seq.empty[Int], 10))
      assertResult(Seq(10))(res)
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
