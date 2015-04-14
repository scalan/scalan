package scalan.common

import scalan.Scalan

trait CommonExamples extends Scalan with SegmentsDsl {
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

  lazy val t13 = fun { (in: Rep[(Int|Array[Int])]) =>
    val res = in.fold(i => Interval(-1,i):RSeg, a => Slice(0, a.length):RSeg)
    res.length
  }
}
