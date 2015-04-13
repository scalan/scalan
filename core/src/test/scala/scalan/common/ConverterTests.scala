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
    lazy val t7 = fun { (in: Rep[IntervalData]) =>
      val Pair(s, l) = in
      val res = IF (s < 0) THEN { Interval(in):RSeg } ELSE { Slice(0, l):RSeg }
      res.length
    }
    lazy val t8 = fun { (in: Rep[IntervalData]) =>
      val Pair(s, l) = in
      val Pair(i, res) = IF (s < 0) THEN { Pair(1, Interval(in):RSeg) } ELSE { Pair(2, Slice(0, l):RSeg) }
      i + res.length
    }
    lazy val t9 = fun { (in: Rep[IntervalData]) =>
      val Pair(s, l) = in
      val segm = IF (s < 0) THEN { Interval(in):RSeg } ELSE { Slice(0, l):RSeg }
      val res = IF (l > 10) THEN { segm.shift(1) } ELSE { Slice(0, l):RSeg }
      res.length
    }
    lazy val t10 = fun { (in: Rep[IntervalData]) =>
      val Pair(s, l) = in
      val res = IF (s < 0) THEN { toRight(Interval(in):RSeg) } ELSE { toRight(Slice(0, l):RSeg) }
      res.fold(_ => 0, s => s.length)
    }
    lazy val t11 = fun { (in: Rep[IntervalData]) =>
      val Pair(s, l) = in
      val res = IF (s < 0) THEN {
        (Interval(in):RSeg).asLeft[Segment]
      } ELSE {
        (Slice(0, l):RSeg).asRight[Segment]
      }
      res.fold(l => l.length, r => r.length)
    }
    lazy val t12 = fun { (in: Rep[IntervalData]) =>
      val Pair(s, l) = in
      val res = IF (s < 0) THEN {
        (Interval(in):RSeg).asRight[Segment]
      } ELSE {
        (Slice(0, l):RSeg).asLeft[Segment]
      }
      res.fold(l => l.length, r => r.length)
    }

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
    val ctx = new ConvProgStaged("convertToCentered")
    ctx.emit("t6", ctx.t6)
  }

  test("convertSeq") {
    val ctx = new ConvProgSeq("convertSeq")
    val res = ctx.t4((10,20))
    assertResult((10,30))(res)
  }

  test("converIfThenElse") {
    val ctx = new ConvProgStaged("converIfThenElse")
    ctx.emit("t7", ctx.t7)
    ctx.emit("t9", ctx.t9)
  }

  test("converIfThenElseWithPair") {
    val ctx = new ConvProgStaged("converIfThenElseWithPair")
    ctx.emit("t8", ctx.t8)
  }

  test("converIfThenElseWithSum") {
    val ctx = new ConvProgStaged("converIfThenElseWithSum")
    ctx.emit("t10", ctx.t10)
    ctx.emit("t11", ctx.t11)
    ctx.emit("t12", ctx.t12)
  }

}
