package scalan.parrays

import scala.language.reflectiveCalls
import scalan._

class PArrayConverterTests extends BaseTests { suite =>

  trait ConvProg extends Scalan with ScalanCommunityDsl {
    lazy val t1 = fun { (in: Rep[PairArray[Int,Double]]) => in.convertTo[ArrayOfPairs[Int, Double]] }
    lazy val t2 = fun { (in: Rep[(Array[Int], Array[Double])]) => {
      val Pair(as, bs) = in
      val ps = PArray.fromArray(as) zip PArray.fromArray(bs)
      ps.convertTo[ArrayOfPairs[Int, Double]].toData
    } }
    lazy val t3 = fun { (in: Rep[Array[(Int,Double)]]) => {
      val ps = ArrayOfPairs(in)
      val Pair(as, bs) = ps.convertTo[PairArray[Int, Double]].toData
      Pair(as.arr, bs.arr)
    } }
    lazy val t4 = fun { (in: Rep[Array[Int]]) => BaseArray(in).convertTo[ArrayOnSeq[Int]].toData }
    lazy val t5 = fun { (in: Rep[SSeq[Int]]) => ArrayOnSeq(in).convertTo[BaseArray[Int]].toData }
  }

  class ConvProgStaged(testName: String) extends TestContext(this, testName) with  ConvProg with ScalanCommunityDslExp {
  }
  class ConvProgSeq(testName: String) extends ScalanCtxSeq with  ConvProg with ScalanCommunityDslSeq {
  }

  test("convert") {
    val ctx = new ConvProgStaged("start")
    ctx.emit("t1", ctx.t1)
    ctx.emit("t2", ctx.t2)
    ctx.emit("t3", ctx.t3)
    ctx.emit("t4", ctx.t4)
    ctx.emit("t5", ctx.t5)
  }


  test("convertSeq") {
    val ctx = new ConvProgSeq("start");
    import ctx._
    {
      val res = ctx.t2((Array(10, 20), Array(10, 20)))
      assertResult(Array((10, 10), (20, 20)))(res)
    }
    {
      val res = ctx.t3(Array((10, 10.0), (20, 20.0)))
      assertResult(Array(10, 20))(res._1)
      assertResult(Array(10.0, 20.0))(res._2)
    }
    {
      val res = ctx.t4(Array(10, 20))
      assertResult(SSeqImpl(Seq(10, 20)))(res)
    }
    {
      val res = ctx.t5(SSeqImpl(Seq(10, 20)))
      assertResult(Array(10, 20))(res)
    }
  }

}
