package scalan.common
package impl

import scala.reflect.runtime.universe._
import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._
import scalan.common.Default

// Abs -----------------------------------
trait SegmentsAbs extends Scalan with Segments {
  self: SegmentsDsl =>
  // single proxy for each type family
  implicit def proxySegment(p: Rep[Segment]): Segment = {
    proxyOps[Segment](p)(classTag[Segment])
  }

  class SegmentElem[To <: Segment]
    extends EntityElem[To] {
    def isEntityType = true
    def tag = {
      weakTypeTag[Segment].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = convertSegment(x.asRep[Segment])
    def convertSegment(x : Rep[Segment]): Rep[To] = {
      assert(x.selfType1.isInstanceOf[SegmentElem[_]])
      x.asRep[To]
    }
    def getDefaultRep: Rep[To] = ???
  }

  def segmentElement =
    new SegmentElem[Segment]()

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
  class IntervalElem(val iso: Iso[IntervalData, Interval])
    extends SegmentElem[Interval]
    with ViewElem[IntervalData, Interval] {
    override def convertSegment(x: Rep[Segment]) = Interval(x.start, x.end)
    override def getDefaultRep = super[ViewElem].getDefaultRep
    override lazy val tag = super[ViewElem].tag
  }

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
  class SliceElem(val iso: Iso[SliceData, Slice])
    extends SegmentElem[Slice]
    with ViewElem[SliceData, Slice] {
    override def convertSegment(x: Rep[Segment]) = Slice(x.start, x.length)
    override def getDefaultRep = super[ViewElem].getDefaultRep
    override lazy val tag = super[ViewElem].tag
  }

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

  // elem for concrete class
  class CenteredElem(val iso: Iso[CenteredData, Centered])
    extends SegmentElem[Centered]
    with ViewElem[CenteredData, Centered] {
    override def convertSegment(x: Rep[Segment]) = // Converter is not generated by meta
!!!("Cannot convert from Segment to Centered: missing fields List(center, radius)")
    override def getDefaultRep = super[ViewElem].getDefaultRep
    override lazy val tag = super[ViewElem].tag
  }

  // state representation type
  type CenteredData = (Int, Int)

  // 3) Iso for concrete class
  class CenteredIso
    extends Iso[CenteredData, Centered] {
    override def from(p: Rep[Centered]) =
      unmkCentered(p) match {
        case Some((center, radius)) => Pair(center, radius)
        case None => !!!
      }
    override def to(p: Rep[(Int, Int)]) = {
      val Pair(center, radius) = p
      Centered(center, radius)
    }
    lazy val tag = {
      weakTypeTag[Centered]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[Centered]](Centered(0, 0))
    lazy val eTo = new CenteredElem(this)
  }
  // 4) constructor and deconstructor
  abstract class CenteredCompanionAbs extends CompanionBase[CenteredCompanionAbs] with CenteredCompanion {
    override def toString = "Centered"
    def apply(p: Rep[CenteredData]): Rep[Centered] =
      isoCentered.to(p)
    def apply(center: Rep[Int], radius: Rep[Int]): Rep[Centered] =
      mkCentered(center, radius)
    def unapply(p: Rep[Centered]) = unmkCentered(p)
  }
  def Centered: Rep[CenteredCompanionAbs]
  implicit def proxyCenteredCompanion(p: Rep[CenteredCompanionAbs]): CenteredCompanionAbs = {
    proxyOps[CenteredCompanionAbs](p)
  }

  class CenteredCompanionElem extends CompanionElem[CenteredCompanionAbs] {
    lazy val tag = weakTypeTag[CenteredCompanionAbs]
    protected def getDefaultRep = Centered
  }
  implicit lazy val CenteredCompanionElem: CenteredCompanionElem = new CenteredCompanionElem

  implicit def proxyCentered(p: Rep[Centered]): Centered =
    proxyOps[Centered](p)

  implicit class ExtendedCentered(p: Rep[Centered]) {
    def toData: Rep[CenteredData] = isoCentered.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoCentered: Iso[CenteredData, Centered] =
    new CenteredIso

  // 6) smart constructor and deconstructor
  def mkCentered(center: Rep[Int], radius: Rep[Int]): Rep[Centered]
  def unmkCentered(p: Rep[Centered]): Option[(Rep[Int], Rep[Int])]
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
      (start: Rep[Int], end: Rep[Int]): Rep[Interval] =
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
      (start: Rep[Int], length: Rep[Int]): Rep[Slice] =
      new SeqSlice(start, length)
  def unmkSlice(p: Rep[Slice]) =
    Some((p.start, p.length))

  case class SeqCentered
      (override val center: Rep[Int], override val radius: Rep[Int])

    extends Centered(center, radius)
        with UserTypeSeq[Segment, Centered] {
    lazy val selfType = element[Centered].asInstanceOf[Elem[Segment]]
  }
  lazy val Centered = new CenteredCompanionAbs with UserTypeSeq[CenteredCompanionAbs, CenteredCompanionAbs] {
    lazy val selfType = element[CenteredCompanionAbs]
  }

  def mkCentered
      (center: Rep[Int], radius: Rep[Int]): Rep[Centered] =
      new SeqCentered(center, radius)
  def unmkCentered(p: Rep[Centered]) =
    Some((p.center, p.radius))
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
    (start: Rep[Int], end: Rep[Int]): Rep[Interval] =
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
    (start: Rep[Int], length: Rep[Int]): Rep[Slice] =
    new ExpSlice(start, length)
  def unmkSlice(p: Rep[Slice]) =
    Some((p.start, p.length))

  case class ExpCentered
      (override val center: Rep[Int], override val radius: Rep[Int])

    extends Centered(center, radius) with UserTypeDef[Segment, Centered] {
    lazy val selfType = element[Centered].asInstanceOf[Elem[Segment]]
    override def mirror(t: Transformer) = ExpCentered(t(center), t(radius))
  }

  lazy val Centered: Rep[CenteredCompanionAbs] = new CenteredCompanionAbs with UserTypeDef[CenteredCompanionAbs, CenteredCompanionAbs] {
    lazy val selfType = element[CenteredCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object CenteredMethods {
    object start {
      def unapply(d: Def[_]): Option[Rep[Centered]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CenteredElem] && method.getName == "start" =>
          Some(receiver).asInstanceOf[Option[Rep[Centered]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Centered]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object end {
      def unapply(d: Def[_]): Option[Rep[Centered]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CenteredElem] && method.getName == "end" =>
          Some(receiver).asInstanceOf[Option[Rep[Centered]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Centered]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[Centered]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CenteredElem] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[Centered]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Centered]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object shift {
      def unapply(d: Def[_]): Option[(Rep[Centered], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(ofs, _*), _) if receiver.elem.isInstanceOf[CenteredElem] && method.getName == "shift" =>
          Some((receiver, ofs)).asInstanceOf[Option[(Rep[Centered], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Centered], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CenteredCompanionMethods {
  }

  def mkCentered
    (center: Rep[Int], radius: Rep[Int]): Rep[Centered] =
    new ExpCentered(center, radius)
  def unmkCentered(p: Rep[Centered]) =
    Some((p.center, p.radius))

  object SegmentMethods {
    object start {
      def unapply(d: Def[_]): Option[Rep[Segment]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SegmentElem[_]] && method.getName == "start" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SegmentElem[_]] && method.getName == "length" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SegmentElem[_]] && method.getName == "end" =>
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
        case MethodCall(receiver, method, Seq(ofs, _*), _) if receiver.elem.isInstanceOf[SegmentElem[_]] && method.getName == "shift" =>
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
