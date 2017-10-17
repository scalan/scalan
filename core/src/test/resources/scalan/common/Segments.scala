package scalan.common

import scala.reflect.runtime.universe._
import scalan._

trait Segments { self: SegmentsModule =>

  type RSeg = Rep[Segment]
  trait Segment extends Def[Segment] { self =>
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
    def attach(seg: Rep[Segment]): Rep[Segment] = seg match {
      case Interval(start, end) =>
        seg
      case Slice(start, length) =>
        self
      case Centered(center, radius) =>
        self
      case _ => seg attach self
    }
  }
  trait IntervalCompanion

  abstract class Slice(val start: Rep[Int], val length: Rep[Int]) extends Segment {
    def end = start + length
    def shift(ofs: Rep[Int]) = Slice(start + ofs, length)
    def attach(seg: Rep[Segment]): Rep[Segment] = self
  }
  trait SliceCompanion

  abstract class Centered(val center: Rep[Int], val radius: Rep[Int]) extends Segment {
    def start = center - radius
    def end = center + radius
    def length = radius * 2
    def shift(ofs: Rep[Int]) = Centered(center + ofs, radius)
    def attach(seg: Rep[Segment]): Rep[Segment] = self
  }
  trait CenteredCompanion
}
