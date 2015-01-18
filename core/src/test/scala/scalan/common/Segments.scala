package scalan.common

import scala.reflect.runtime.universe._
import scalan._

trait Segments { self: SegmentsDsl =>

  type RSeg = Rep[Segment]
  trait Segment extends Reifiable[Segment] { self =>
    def start: Rep[Int]
    def length: Rep[Int]
    def end: Rep[Int]
  }
  trait SegmentCompanion
  implicit def defaultSegmentElem: Elem[Segment] = element[Interval].asElem[Segment]

  abstract class Interval(val start: Rep[Int], val end: Rep[Int]) extends Segment {
    def length = end - start
  }
  trait IntervalCompanion

  abstract class Slice(val start: Rep[Int], val length: Rep[Int]) extends Segment {
    def end = start + length
  }
  trait SliceCompanion
}


trait SegmentsDsl extends impl.SegmentsAbs {
}

trait SegmentsDslSeq extends impl.SegmentsSeq {
}

trait SegmentsDslExp extends impl.SegmentsExp {
}