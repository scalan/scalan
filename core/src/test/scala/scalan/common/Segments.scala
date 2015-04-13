package scalan.common

import scala.reflect.runtime.universe._
import scalan._

trait Segments { self: SegmentsDsl =>

  type RSeg = Rep[Segment]
  trait Segment extends Reifiable[Segment] { self =>
    def start: Rep[Int]
    def length: Rep[Int]
    def end: Rep[Int]
    def shift(ofs: Rep[Int]): Rep[Segment]
    def attach(seg: Rep[Segment]): Rep[Segment]
  }
  trait SegmentCompanion

  abstract class Interval(val start: Rep[Int], val end: Rep[Int]) extends Segment {
    def length = end - start
    def shift(ofs: Rep[Int]) = Interval(start + ofs, end + ofs)
    def attach(seg: Rep[Segment]): Rep[Segment] = matchSegment(seg) {
      ivl => 
        val startMin = Math.min(start, seg.start)
        val startMax = Math.min(start, seg.start)
        val endMin = Math.min(start, seg.start)
        val endMax = Math.min(start, seg.start)
        ???
    } {
      slc => ???
    } {
      cnt => ???
    } {
      abs => ???
    }
  }
  trait IntervalCompanion

  abstract class Slice(val start: Rep[Int], val length: Rep[Int]) extends Segment {
    def end = start + length
    def shift(ofs: Rep[Int]) = Slice(start + ofs, length)
    def attach(seg: Rep[Segment]): Rep[Segment] = matchSegment(seg) {
      ivl => ???
    } {
      slc => ???
    } {
      cnt => ???
    } {
      abs => ???
    }
  }
  trait SliceCompanion

  abstract class Centered(val center: Rep[Int], val radius: Rep[Int]) extends Segment {
    def start = center - radius
    def end = center + radius
    def length = radius * 2
    def shift(ofs: Rep[Int]) = Centered(center + ofs, radius)
    def attach(seg: Rep[Segment]): Rep[Segment] = matchSegment(seg) {
      ivl => ???
    } {
      slc => ???
    } {
      cnt => ???
    } {
      abs => ???
    }
  }
  trait CenteredCompanion
}


trait SegmentsDsl extends impl.SegmentsAbs {
}

trait SegmentsDslSeq extends impl.SegmentsSeq {
}

trait SegmentsDslExp extends impl.SegmentsExp {
}