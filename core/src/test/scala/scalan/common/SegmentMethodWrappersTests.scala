package scalan.common

import scala.language.reflectiveCalls
import scalan._

trait SegmentMethodWrappers extends Scalan with SegmentsDsl {
  lazy val Interval_start = fun { (in: Rep[IntervalData]) => Interval(in).start }
  lazy val Slice_start = fun { (in: Rep[SliceData]) => Slice(in).end }

  lazy val Interval_length = fun { (in: Rep[IntervalData]) => Interval(in).length }
  lazy val Slice_length = fun { (in: Rep[SliceData]) => Slice(in).length }

  lazy val Interval_end = fun { (in: Rep[IntervalData]) => Interval(in).end }
  lazy val Slice_end = fun { (in: Rep[SliceData]) => Slice(in).end }

  lazy val Interval_shift = fun { (in: Rep[(IntervalData, Int)]) => val Pair(i, o) = in; Interval(i).shift(o) }
  lazy val Slice_shift = fun { (in: Rep[(SliceData, Int)]) => val Pair(i, o) = in; Slice(i).shift(o) }
}

class SegmentMethodWrappersTests extends BaseNestedCtxTests {

  class SegmentMethodWrappersStaged(testName: String) extends TestContext(testName) with SegmentMethodWrappers with SegmentsDsl {
  }

  describe("Start") {
    it("interval") {
      val ctx = new SegmentMethodWrappersStaged("start")
      ctx.emit("Interval_start", ctx.Interval_start)
    }
    it("slice") {
      val ctx = new SegmentMethodWrappersStaged("start")
      ctx.emit("Slice_start", ctx.Slice_start)
    }
  }

  describe("Length") {
    it("interval") {
      val ctx = new SegmentMethodWrappersStaged("length")
      ctx.emit("Interval_length", ctx.Interval_length)
    }
    it("slice") {
      val ctx = new SegmentMethodWrappersStaged("length")
      ctx.emit("Slice_length", ctx.Slice_length)
    }
  }

  describe("End") {
    it("interval") {
      val ctx = new SegmentMethodWrappersStaged("end")
      ctx.emit("Interval_end", ctx.Interval_end)
    }
    it("slice") {
      val ctx = new SegmentMethodWrappersStaged("end")
      ctx.emit("Slice_end", ctx.Slice_end)
    }
  }

  it("Interval_shift") {
    val ctx = new SegmentMethodWrappersStaged("shift")
    ctx.emit("Interval_shift", ctx.Interval_shift)
  }
  it("Slice_shift") {
    val ctx = new SegmentMethodWrappersStaged("shift")
    ctx.emit("Slice_shift", ctx.Slice_shift)
  }

}
