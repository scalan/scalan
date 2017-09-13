package scalan.common

import scalan.Scalan

trait ViewExamples extends Scalan with SegmentsModule {
  lazy val v1 = fun { (in: Rep[Interval]) => in }
  lazy val v2 = fun { (in: Rep[Interval]) => Pair(in,in) }
  lazy val v3 = fun { (in: Rep[Interval]) => Pair(in, in.length) }
  lazy val v4 = fun { (in: Rep[Interval]) => Pair(in.length, in) }
  lazy val v5 = fun { (in: Rep[Interval]) => in.length }
  lazy val v6 = fun { (in: Rep[Interval]) => Pair(in.length, 1) }
  lazy val v7 = fun { (in: Rep[Interval]) => Pair(in.length, 1) }

  lazy val v8 = fun { (in: Rep[Interval]) => in.asLeft[Unit] }
  lazy val v9 = fun { (in: Rep[Interval]) => in.asLeft[Slice] }
}
