package scalan.common
package impl

import scala.reflect.runtime.universe._
import scalan._
import scala.reflect.runtime.universe._
import scalan.common.Default

// Abs -----------------------------------
trait SegmentsAbs extends Scalan with Segments {
  self: SegmentsDsl =>
  // single proxy for each type family
  implicit def proxySegment(p: Rep[Segment]): Segment =
    proxyOps[Segment](p)

  abstract class SegmentElem[From, To <: Segment](iso: Iso[From, To]) extends ViewElem[From, To]()(iso)

  trait SegmentCompanionElem extends CompanionElem[SegmentCompanionAbs]
  implicit lazy val SegmentCompanionElem: SegmentCompanionElem = new SegmentCompanionElem {
    lazy val tag = weakTypeTag[SegmentCompanionAbs]
    protected def getDefaultRep = Segment
  }

  abstract class SegmentCompanionAbs extends CompanionBase[SegmentCompanionAbs] with SegmentCompanion {
    override def toString = "Segment"
  }
  def Segment: Rep[SegmentCompanionAbs]
  implicit def proxySegmentCompanion(p: Rep[SegmentCompanion]): SegmentCompanion = {
    proxyOps[SegmentCompanion](p)
  }

  // elem for concrete class
  class IntervalElem(iso: Iso[IntervalData, Interval]) extends SegmentElem[IntervalData, Interval](iso)

  // state representation type
  type IntervalData = (Int, Int)

  // 3) Iso for concrete class
  class IntervalIso
    extends Iso[IntervalData, Interval] {
    override def from(p: Rep[Interval]) =
      unmkInterval(p) match {
        case Some((start, end)) => Pair(start, end)
        case None => !!!
      }
    override def to(p: Rep[(Int, Int)]) = {
      val Pair(start, end) = p
      Interval(start, end)
    }
    lazy val tag = {
      weakTypeTag[Interval]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[Interval]](Interval(0, 0))
    lazy val eTo = new IntervalElem(this)
  }
  // 4) constructor and deconstructor
  abstract class IntervalCompanionAbs extends CompanionBase[IntervalCompanionAbs] with IntervalCompanion {
    override def toString = "Interval"
    def apply(p: Rep[IntervalData]): Rep[Interval] =
      isoInterval.to(p)
    def apply(start: Rep[Int], end: Rep[Int]): Rep[Interval] =
      mkInterval(start, end)
    def unapply(p: Rep[Interval]) = unmkInterval(p)
  }
  def Interval: Rep[IntervalCompanionAbs]
  implicit def proxyIntervalCompanion(p: Rep[IntervalCompanionAbs]): IntervalCompanionAbs = {
    proxyOps[IntervalCompanionAbs](p)
  }

  class IntervalCompanionElem extends CompanionElem[IntervalCompanionAbs] {
    lazy val tag = weakTypeTag[IntervalCompanionAbs]
    protected def getDefaultRep = Interval
  }
  implicit lazy val IntervalCompanionElem: IntervalCompanionElem = new IntervalCompanionElem

  implicit def proxyInterval(p: Rep[Interval]): Interval =
    proxyOps[Interval](p)

  implicit class ExtendedInterval(p: Rep[Interval]) {
    def toData: Rep[IntervalData] = isoInterval.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoInterval: Iso[IntervalData, Interval] =
    new IntervalIso

  // 6) smart constructor and deconstructor
  def mkInterval(start: Rep[Int], end: Rep[Int]): Rep[Interval]
  def unmkInterval(p: Rep[Interval]): Option[(Rep[Int], Rep[Int])]

  // elem for concrete class
  class SliceElem(iso: Iso[SliceData, Slice]) extends SegmentElem[SliceData, Slice](iso)

  // state representation type
  type SliceData = (Int, Int)

  // 3) Iso for concrete class
  class SliceIso
    extends Iso[SliceData, Slice] {
    override def from(p: Rep[Slice]) =
      unmkSlice(p) match {
        case Some((start, length)) => Pair(start, length)
        case None => !!!
      }
    override def to(p: Rep[(Int, Int)]) = {
      val Pair(start, length) = p
      Slice(start, length)
    }
    lazy val tag = {
      weakTypeTag[Slice]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[Slice]](Slice(0, 0))
    lazy val eTo = new SliceElem(this)
  }
  // 4) constructor and deconstructor
  abstract class SliceCompanionAbs extends CompanionBase[SliceCompanionAbs] with SliceCompanion {
    override def toString = "Slice"
    def apply(p: Rep[SliceData]): Rep[Slice] =
      isoSlice.to(p)
    def apply(start: Rep[Int], length: Rep[Int]): Rep[Slice] =
      mkSlice(start, length)
    def unapply(p: Rep[Slice]) = unmkSlice(p)
  }
  def Slice: Rep[SliceCompanionAbs]
  implicit def proxySliceCompanion(p: Rep[SliceCompanionAbs]): SliceCompanionAbs = {
    proxyOps[SliceCompanionAbs](p)
  }

  class SliceCompanionElem extends CompanionElem[SliceCompanionAbs] {
    lazy val tag = weakTypeTag[SliceCompanionAbs]
    protected def getDefaultRep = Slice
  }
  implicit lazy val SliceCompanionElem: SliceCompanionElem = new SliceCompanionElem

  implicit def proxySlice(p: Rep[Slice]): Slice =
    proxyOps[Slice](p)

  implicit class ExtendedSlice(p: Rep[Slice]) {
    def toData: Rep[SliceData] = isoSlice.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSlice: Iso[SliceData, Slice] =
    new SliceIso

  // 6) smart constructor and deconstructor
  def mkSlice(start: Rep[Int], length: Rep[Int]): Rep[Slice]
  def unmkSlice(p: Rep[Slice]): Option[(Rep[Int], Rep[Int])]
}

// Seq -----------------------------------
trait SegmentsSeq extends SegmentsDsl with ScalanSeq {
  self: SegmentsDslSeq =>
  lazy val Segment: Rep[SegmentCompanionAbs] = new SegmentCompanionAbs with UserTypeSeq[SegmentCompanionAbs, SegmentCompanionAbs] {
    lazy val selfType = element[SegmentCompanionAbs]
  }

  case class SeqInterval
      (override val start: Rep[Int], override val end: Rep[Int])

    extends Interval(start, end)
        with UserTypeSeq[Segment, Interval] {
    lazy val selfType = element[Interval].asInstanceOf[Elem[Segment]]
  }
  lazy val Interval = new IntervalCompanionAbs with UserTypeSeq[IntervalCompanionAbs, IntervalCompanionAbs] {
    lazy val selfType = element[IntervalCompanionAbs]
  }

  def mkInterval
      (start: Rep[Int], end: Rep[Int]) =
      new SeqInterval(start, end)
  def unmkInterval(p: Rep[Interval]) =
    Some((p.start, p.end))

  case class SeqSlice
      (override val start: Rep[Int], override val length: Rep[Int])

    extends Slice(start, length)
        with UserTypeSeq[Segment, Slice] {
    lazy val selfType = element[Slice].asInstanceOf[Elem[Segment]]
  }
  lazy val Slice = new SliceCompanionAbs with UserTypeSeq[SliceCompanionAbs, SliceCompanionAbs] {
    lazy val selfType = element[SliceCompanionAbs]
  }

  def mkSlice
      (start: Rep[Int], length: Rep[Int]) =
      new SeqSlice(start, length)
  def unmkSlice(p: Rep[Slice]) =
    Some((p.start, p.length))
}

// Exp -----------------------------------
trait SegmentsExp extends SegmentsDsl with ScalanExp {
  self: SegmentsDslExp =>
  lazy val Segment: Rep[SegmentCompanionAbs] = new SegmentCompanionAbs with UserTypeDef[SegmentCompanionAbs, SegmentCompanionAbs] {
    lazy val selfType = element[SegmentCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpInterval
      (override val start: Rep[Int], override val end: Rep[Int])

    extends Interval(start, end) with UserTypeDef[Segment, Interval] {
    lazy val selfType = element[Interval].asInstanceOf[Elem[Segment]]
    override def mirror(t: Transformer) = ExpInterval(t(start), t(end))
  }

  lazy val Interval: Rep[IntervalCompanionAbs] = new IntervalCompanionAbs with UserTypeDef[IntervalCompanionAbs, IntervalCompanionAbs] {
    lazy val selfType = element[IntervalCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object IntervalMethods {
    object length {
      def unapply(d: Def[_]): Option[Rep[Interval]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IntervalElem] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[Interval]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Interval]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object shift {
      def unapply(d: Def[_]): Option[(Rep[Interval], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(ofs, _*), _) if receiver.elem.isInstanceOf[IntervalElem] && method.getName == "shift" =>
          Some((receiver, ofs)).asInstanceOf[Option[(Rep[Interval], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Interval], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object IntervalCompanionMethods {
  }

  def mkInterval
    (start: Rep[Int], end: Rep[Int]) =
    new ExpInterval(start, end)
  def unmkInterval(p: Rep[Interval]) =
    Some((p.start, p.end))

  case class ExpSlice
      (override val start: Rep[Int], override val length: Rep[Int])

    extends Slice(start, length) with UserTypeDef[Segment, Slice] {
    lazy val selfType = element[Slice].asInstanceOf[Elem[Segment]]
    override def mirror(t: Transformer) = ExpSlice(t(start), t(length))
  }

  lazy val Slice: Rep[SliceCompanionAbs] = new SliceCompanionAbs with UserTypeDef[SliceCompanionAbs, SliceCompanionAbs] {
    lazy val selfType = element[SliceCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object SliceMethods {
    object end {
      def unapply(d: Def[_]): Option[Rep[Slice]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SliceElem] && method.getName == "end" =>
          Some(receiver).asInstanceOf[Option[Rep[Slice]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Slice]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object shift {
      def unapply(d: Def[_]): Option[(Rep[Slice], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(ofs, _*), _) if receiver.elem.isInstanceOf[SliceElem] && method.getName == "shift" =>
          Some((receiver, ofs)).asInstanceOf[Option[(Rep[Slice], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Slice], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SliceCompanionMethods {
  }

  def mkSlice
    (start: Rep[Int], length: Rep[Int]) =
    new ExpSlice(start, length)
  def unmkSlice(p: Rep[Slice]) =
    Some((p.start, p.length))

  object SegmentMethods {
    object start {
      def unapply(d: Def[_]): Option[Rep[Segment]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SegmentElem[_, _]] && method.getName == "start" =>
          Some(receiver).asInstanceOf[Option[Rep[Segment]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Segment]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[Segment]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SegmentElem[_, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[Segment]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Segment]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object end {
      def unapply(d: Def[_]): Option[Rep[Segment]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SegmentElem[_, _]] && method.getName == "end" =>
          Some(receiver).asInstanceOf[Option[Rep[Segment]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Segment]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object shift {
      def unapply(d: Def[_]): Option[(Rep[Segment], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(ofs, _*), _) if receiver.elem.isInstanceOf[SegmentElem[_, _]] && method.getName == "shift" =>
          Some((receiver, ofs)).asInstanceOf[Option[(Rep[Segment], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Segment], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SegmentCompanionMethods {
  }
}
