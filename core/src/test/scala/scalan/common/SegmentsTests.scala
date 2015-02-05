package scalan.common

import scala.language.reflectiveCalls
import scalan._

trait SegmentsMethodWrappers extends Scalan with SegmentsDsl {
  lazy val Interval_start = fun { (in: Rep[IntervalData]) => Interval(in).start }
  lazy val Slice_start = fun { (in: Rep[SliceData]) => Slice(in).end }

  lazy val Interval_length = fun { (in: Rep[IntervalData]) => Interval(in).length }
  lazy val Slice_length = fun { (in: Rep[SliceData]) => Slice(in).length }

  lazy val Interval_end = fun { (in: Rep[IntervalData]) => Interval(in).end }
  lazy val Slice_end = fun { (in: Rep[SliceData]) => Slice(in).end }
}

class SegmentsTests extends BaseTests { suite =>

  class SegmentsMethodWrappersStaged(testName: String) extends TestContext(this, testName) with  SegmentsMethodWrappers with SegmentsDslExp {
  }

  test("Interval_start") {
    val ctx = new SegmentsMethodWrappersStaged("start")
    ctx.emit("Interval_start", ctx.Interval_start)
  }
  test("Slice_start") {
    val ctx = new SegmentsMethodWrappersStaged("start")
    ctx.emit("Slice_start", ctx.Slice_start)
  }

  test("Interval_length") {
    val ctx = new SegmentsMethodWrappersStaged("length")
    ctx.emit("Interval_length", ctx.Interval_length)
  }
  test("Slice_length") {
    val ctx = new SegmentsMethodWrappersStaged("length")
    ctx.emit("Slice_length", ctx.Slice_length)
  }

  test("Interval_end") {
    val ctx = new SegmentsMethodWrappersStaged("end")
    ctx.emit("Interval_end", ctx.Interval_end)
  }
  test("Slice_end") {
    val ctx = new SegmentsMethodWrappersStaged("end")
    ctx.emit("Slice_end", ctx.Slice_end)
  }

}
