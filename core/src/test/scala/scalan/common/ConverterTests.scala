package scalan.common

import scala.language.reflectiveCalls
import scalan._

class ConverterTests extends BaseTests { suite =>

  trait ConvProg extends Scalan with SegmentsDsl {
    lazy val t1 = fun { (in: Rep[Interval]) => in.convertTo[Slice] }
    lazy val t2 = fun { (in: Rep[Slice]) => in.convertTo[Interval] }
    lazy val t3 = fun { (in: Rep[IntervalData]) => Interval(in).convertTo[Slice].toData }
    lazy val t4 = fun { (in: Rep[SliceData]) => Slice(in).convertTo[Interval].toData }
    lazy val t5 = fun { (in: Rep[CenteredData]) => Centered(in).convertTo[Interval].toData }
    lazy val t6 = fun { (in: Rep[IntervalData]) => Interval(in).convertTo[Centered].toData }

//    lazy val t7 = fun { (in: Rep[(Interval,Int)]) =>
//      val Pair(i, n) = in
//      Pair(i, n).convertTo[(Slice, Int)]  //TODO relax <: Reifiable[_] constraint
//    }
  }

  class ConvProgStaged(testName: String) extends TestContext(this, testName) with  ConvProg with SegmentsDslExp {
  }
  class ConvProgSeq(testName: String) extends ScalanCtxSeq with  ConvProg with SegmentsDslSeq {
  }

  test("simple converter tests") {
    val ctx = new ConvProgStaged("simple converter tests")
    ctx.emit("t1", ctx.t1)
    ctx.emit("t2", ctx.t2)
    ctx.emit("t3", ctx.t3)
    ctx.emit("t4", ctx.t4)
    ctx.emit("t5", ctx.t5)
  }

  ignore("convertToCentered") {
    val ctx = new ConvProgStaged("start")
    ctx.emit("t6", ctx.t6)
  }

  test("convertSeq") {
    val ctx = new ConvProgSeq("start")
    val res = ctx.t4((10,20))
    assertResult((10,30))(res)
  }

}
