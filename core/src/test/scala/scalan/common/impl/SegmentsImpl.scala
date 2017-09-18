package scalan.common

import scala.reflect.runtime.universe._
import scalan._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait SegmentsDefs extends scalan.Scalan with Segments {
  self: SegmentsModule =>

  // entityProxy: single proxy for each type family
  implicit def proxySegment(p: Rep[Segment]): Segment = {
    proxyOps[Segment](p)(scala.reflect.classTag[Segment])
  }

  // familyElem
  class SegmentElem[To <: Segment]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    lazy val typeArgs = TypeArgs()
    override lazy val tag = {
      weakTypeTag[Segment].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Segment] => convertSegment(x) }
      tryConvert(element[Segment], this, x, conv)
    }

    def convertSegment(x: Rep[Segment]): Rep[To] = {
      x.elem match {
        case _: SegmentElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have SegmentElem[_], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def segmentElement: Elem[Segment] =
    cachedElem[SegmentElem[Segment]]()

  implicit case object SegmentCompanionElem extends CompanionElem[SegmentCompanionCtor] {
    lazy val tag = weakTypeTag[SegmentCompanionCtor]
    protected def getDefaultRep = Segment
  }

  abstract class SegmentCompanionCtor extends CompanionDef[SegmentCompanionCtor] with SegmentCompanion {
    def selfType = SegmentCompanionElem
    override def toString = "Segment"
  }
  implicit def proxySegmentCompanionCtor(p: Rep[SegmentCompanionCtor]): SegmentCompanionCtor =
    proxyOps[SegmentCompanionCtor](p)

  case class IntervalCtor
      (override val start: Rep[Int], override val end: Rep[Int])
    extends Interval(start, end) with Def[Interval] {
    lazy val selfType = element[Interval]
  }
  // elem for concrete class
  class IntervalElem(val iso: Iso[IntervalData, Interval])
    extends SegmentElem[Interval]
    with ConcreteElem[IntervalData, Interval] {
    override lazy val parent: Option[Elem[_]] = Some(segmentElement)
    override lazy val typeArgs = TypeArgs()

    override def convertSegment(x: Rep[Segment]) = Interval(x.start, x.end)
    override def getDefaultRep = Interval(0, 0)
    override lazy val tag = {
      weakTypeTag[Interval]
    }
  }

  // state representation type
  type IntervalData = (Int, Int)

  // 3) Iso for concrete class
  class IntervalIso
    extends EntityIso[IntervalData, Interval] with Def[IntervalIso] {
    override def from(p: Rep[Interval]) =
      (p.start, p.end)
    override def to(p: Rep[(Int, Int)]) = {
      val Pair(start, end) = p
      Interval(start, end)
    }
    lazy val eFrom = pairElement(element[Int], element[Int])
    lazy val eTo = new IntervalElem(self)
    lazy val selfType = new IntervalIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class IntervalIsoElem() extends Elem[IntervalIso] {
    def getDefaultRep = reifyObject(new IntervalIso())
    lazy val tag = {
      weakTypeTag[IntervalIso]
    }
    lazy val typeArgs = TypeArgs()
  }
  // 4) constructor and deconstructor
  class IntervalCompanionCtor extends CompanionDef[IntervalCompanionCtor] with IntervalCompanion {
    def selfType = IntervalCompanionElem
    override def toString = "IntervalCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[IntervalData]): Rep[Interval] = {
      isoInterval.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(start: Rep[Int], end: Rep[Int]): Rep[Interval] =
      mkInterval(start, end)

    def unapply(p: Rep[Segment]) = unmkInterval(p)
  }
  lazy val IntervalRep: Rep[IntervalCompanionCtor] = new IntervalCompanionCtor
  lazy val Interval: IntervalCompanionCtor = proxyIntervalCompanion(IntervalRep)
  implicit def proxyIntervalCompanion(p: Rep[IntervalCompanionCtor]): IntervalCompanionCtor = {
    proxyOps[IntervalCompanionCtor](p)
  }

  implicit case object IntervalCompanionElem extends CompanionElem[IntervalCompanionCtor] {
    lazy val tag = weakTypeTag[IntervalCompanionCtor]
    protected def getDefaultRep = IntervalRep
  }

  implicit def proxyInterval(p: Rep[Interval]): Interval =
    proxyOps[Interval](p)

  implicit class ExtendedInterval(p: Rep[Interval]) {
    def toData: Rep[IntervalData] = isoInterval.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoInterval: Iso[IntervalData, Interval] =
    reifyObject(new IntervalIso())

  case class SliceCtor
      (override val start: Rep[Int], override val length: Rep[Int])
    extends Slice(start, length) with Def[Slice] {
    lazy val selfType = element[Slice]
  }
  // elem for concrete class
  class SliceElem(val iso: Iso[SliceData, Slice])
    extends SegmentElem[Slice]
    with ConcreteElem[SliceData, Slice] {
    override lazy val parent: Option[Elem[_]] = Some(segmentElement)
    override lazy val typeArgs = TypeArgs()

    override def convertSegment(x: Rep[Segment]) = Slice(x.start, x.length)
    override def getDefaultRep = Slice(0, 0)
    override lazy val tag = {
      weakTypeTag[Slice]
    }
  }

  // state representation type
  type SliceData = (Int, Int)

  // 3) Iso for concrete class
  class SliceIso
    extends EntityIso[SliceData, Slice] with Def[SliceIso] {
    override def from(p: Rep[Slice]) =
      (p.start, p.length)
    override def to(p: Rep[(Int, Int)]) = {
      val Pair(start, length) = p
      Slice(start, length)
    }
    lazy val eFrom = pairElement(element[Int], element[Int])
    lazy val eTo = new SliceElem(self)
    lazy val selfType = new SliceIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class SliceIsoElem() extends Elem[SliceIso] {
    def getDefaultRep = reifyObject(new SliceIso())
    lazy val tag = {
      weakTypeTag[SliceIso]
    }
    lazy val typeArgs = TypeArgs()
  }
  // 4) constructor and deconstructor
  class SliceCompanionCtor extends CompanionDef[SliceCompanionCtor] with SliceCompanion {
    def selfType = SliceCompanionElem
    override def toString = "SliceCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[SliceData]): Rep[Slice] = {
      isoSlice.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(start: Rep[Int], length: Rep[Int]): Rep[Slice] =
      mkSlice(start, length)

    def unapply(p: Rep[Segment]) = unmkSlice(p)
  }
  lazy val SliceRep: Rep[SliceCompanionCtor] = new SliceCompanionCtor
  lazy val Slice: SliceCompanionCtor = proxySliceCompanion(SliceRep)
  implicit def proxySliceCompanion(p: Rep[SliceCompanionCtor]): SliceCompanionCtor = {
    proxyOps[SliceCompanionCtor](p)
  }

  implicit case object SliceCompanionElem extends CompanionElem[SliceCompanionCtor] {
    lazy val tag = weakTypeTag[SliceCompanionCtor]
    protected def getDefaultRep = SliceRep
  }

  implicit def proxySlice(p: Rep[Slice]): Slice =
    proxyOps[Slice](p)

  implicit class ExtendedSlice(p: Rep[Slice]) {
    def toData: Rep[SliceData] = isoSlice.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSlice: Iso[SliceData, Slice] =
    reifyObject(new SliceIso())

  case class CenteredCtor
      (override val center: Rep[Int], override val radius: Rep[Int])
    extends Centered(center, radius) with Def[Centered] {
    lazy val selfType = element[Centered]
  }
  // elem for concrete class
  class CenteredElem(val iso: Iso[CenteredData, Centered])
    extends SegmentElem[Centered]
    with ConcreteElem[CenteredData, Centered] {
    override lazy val parent: Option[Elem[_]] = Some(segmentElement)
    override lazy val typeArgs = TypeArgs()

    override def convertSegment(x: Rep[Segment]) = // Converter is not generated by meta
!!!("Cannot convert from Segment to Centered: missing fields List(center, radius)")
    override def getDefaultRep = Centered(0, 0)
    override lazy val tag = {
      weakTypeTag[Centered]
    }
  }

  // state representation type
  type CenteredData = (Int, Int)

  // 3) Iso for concrete class
  class CenteredIso
    extends EntityIso[CenteredData, Centered] with Def[CenteredIso] {
    override def from(p: Rep[Centered]) =
      (p.center, p.radius)
    override def to(p: Rep[(Int, Int)]) = {
      val Pair(center, radius) = p
      Centered(center, radius)
    }
    lazy val eFrom = pairElement(element[Int], element[Int])
    lazy val eTo = new CenteredElem(self)
    lazy val selfType = new CenteredIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class CenteredIsoElem() extends Elem[CenteredIso] {
    def getDefaultRep = reifyObject(new CenteredIso())
    lazy val tag = {
      weakTypeTag[CenteredIso]
    }
    lazy val typeArgs = TypeArgs()
  }
  // 4) constructor and deconstructor
  class CenteredCompanionCtor extends CompanionDef[CenteredCompanionCtor] with CenteredCompanion {
    def selfType = CenteredCompanionElem
    override def toString = "CenteredCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[CenteredData]): Rep[Centered] = {
      isoCentered.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(center: Rep[Int], radius: Rep[Int]): Rep[Centered] =
      mkCentered(center, radius)

    def unapply(p: Rep[Segment]) = unmkCentered(p)
  }
  lazy val CenteredRep: Rep[CenteredCompanionCtor] = new CenteredCompanionCtor
  lazy val Centered: CenteredCompanionCtor = proxyCenteredCompanion(CenteredRep)
  implicit def proxyCenteredCompanion(p: Rep[CenteredCompanionCtor]): CenteredCompanionCtor = {
    proxyOps[CenteredCompanionCtor](p)
  }

  implicit case object CenteredCompanionElem extends CompanionElem[CenteredCompanionCtor] {
    lazy val tag = weakTypeTag[CenteredCompanionCtor]
    protected def getDefaultRep = CenteredRep
  }

  implicit def proxyCentered(p: Rep[Centered]): Centered =
    proxyOps[Centered](p)

  implicit class ExtendedCentered(p: Rep[Centered]) {
    def toData: Rep[CenteredData] = isoCentered.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoCentered: Iso[CenteredData, Centered] =
    reifyObject(new CenteredIso())

  registerModule(SegmentsModule)

  lazy val Segment: Rep[SegmentCompanionCtor] = new SegmentCompanionCtor {
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

    object attach {
      def unapply(d: Def[_]): Option[(Rep[Interval], Rep[Segment])] = d match {
        case MethodCall(receiver, method, Seq(seg, _*), _) if receiver.elem.isInstanceOf[IntervalElem] && method.getName == "attach" =>
          Some((receiver, seg)).asInstanceOf[Option[(Rep[Interval], Rep[Segment])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Interval], Rep[Segment])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object IntervalCompanionMethods {
  }

  def mkInterval
    (start: Rep[Int], end: Rep[Int]): Rep[Interval] = {
    new IntervalCtor(start, end)
  }
  def unmkInterval(p: Rep[Segment]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IntervalElem @unchecked =>
      Some((p.asRep[Interval].start, p.asRep[Interval].end))
    case _ =>
      None
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

    object attach {
      def unapply(d: Def[_]): Option[(Rep[Slice], Rep[Segment])] = d match {
        case MethodCall(receiver, method, Seq(seg, _*), _) if receiver.elem.isInstanceOf[SliceElem] && method.getName == "attach" =>
          Some((receiver, seg)).asInstanceOf[Option[(Rep[Slice], Rep[Segment])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Slice], Rep[Segment])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SliceCompanionMethods {
  }

  def mkSlice
    (start: Rep[Int], length: Rep[Int]): Rep[Slice] = {
    new SliceCtor(start, length)
  }
  def unmkSlice(p: Rep[Segment]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SliceElem @unchecked =>
      Some((p.asRep[Slice].start, p.asRep[Slice].length))
    case _ =>
      None
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

    object attach {
      def unapply(d: Def[_]): Option[(Rep[Centered], Rep[Segment])] = d match {
        case MethodCall(receiver, method, Seq(seg, _*), _) if receiver.elem.isInstanceOf[CenteredElem] && method.getName == "attach" =>
          Some((receiver, seg)).asInstanceOf[Option[(Rep[Centered], Rep[Segment])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Centered], Rep[Segment])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CenteredCompanionMethods {
  }

  def mkCentered
    (center: Rep[Int], radius: Rep[Int]): Rep[Centered] = {
    new CenteredCtor(center, radius)
  }
  def unmkCentered(p: Rep[Segment]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CenteredElem @unchecked =>
      Some((p.asRep[Centered].center, p.asRep[Centered].radius))
    case _ =>
      None
  }

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

    object attach {
      def unapply(d: Def[_]): Option[(Rep[Segment], Rep[Segment])] = d match {
        case MethodCall(receiver, method, Seq(seg, _*), _) if receiver.elem.isInstanceOf[SegmentElem[_]] && method.getName == "attach" =>
          Some((receiver, seg)).asInstanceOf[Option[(Rep[Segment], Rep[Segment])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Segment], Rep[Segment])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SegmentCompanionMethods {
  }
}

object SegmentsModule extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWTYgcRRSumdndmdnZDDvrDyuKxnHEIDKzGkICg4RlfyQy2R3SiQtjUGq6a2c7VneX3TVrjyzxloPeRDyIHgKKlyCIN5UgJoJIyMGbeJSAIErIweBB8VV1dU/PT++Pkj401V2v3vfe9716VVd+R5Oeiw57OqbYrlqE46omx4ser2inHaNLyTLZ/PAv/Y1m7kQ+jeZaaGoLe8sebaF8MFjxWTTWuNFApVXTNlZsbvJexZIuOKo2AoyawKiNw6jEVtUbaF58briYMeIO+Tq2P1+Di8FlHts68bjjehw9Hvio6Q6lROemY9dMy+py3Kak1jA9DvYTbcfovY4uokwDzeqOrbuEE22JYs8jnvqfI8K9GX3n5XdvnfUxRuM862KTQ5iAMRvYnyFM69mO3bM4KqrQ1pkIC2wKxGfA7SmLUQkz2UBZ02KOy0PULCBsOUb4OWFj+IHmGhfwNq4Baqemcde0O8IZw/pruEPWwESYT0EOHqGbZ3uMKOcFjxsDeD5DCDEGlfKcDK3aZ60asVYVrFU04pqYmm9iMdl0Hb+HgieVQcgXLp7Zw0XogazYRuXt8/rLd7WClRaLfRFMXoaUA0ePJVStFAjY/f7Mu96dFy4fT6PpFpo2vcW2x12s83ghKMIK2LYdLmOOOMRuBzQsJ2koURbBZqhQ8rpjMWyDJ8XmDEhFTd3kwlj8KyqBEtjPckZC04zPUlG+SbtUrF1kjPau7Vzd+eWRn0rpoDB95sbcZsDtLunIklzClEI6aR6CA+p0IJfmWKRUvmO+cvkdnkapBkr5gwW23r4ActZ9F80EK4Ly/cc8/vfPxU2eVuonJhHif5395ttfb52cSKP0IE95SECDXuOGwXGU1UjHgg0k60K8HopGBVUsyYiRgE/99ofx3QI6L9OSsofZ76vSwMXciQ++fJI0P0ujXEtuzVWKO7LmhADLxNNbKOdsEzf4n93GVIzG1l3WIJu4S7mSLc5AIOHhRAkZEfTU5W5NhekXAjXWHJtUVpuVP7Uf3rsixBDzD3Jo/xyrPjLPUQb6UJT1E0lSMdJ0TQu63jY5du2rc7evrk1KteZU6C9h2iVBw1GR97MQsqUWAOlUIFvBl3iPRiGLVxnmiW2MRiVeR/ryFmId4f5ddAr77eeXLj1w++NX75MdIdc2uYVZZeEA/SDcvvdwv6OIhSCvh/vfkhrgrwTcEReKaCkOXR5ew1EuNByaPxQ2FfEuKfoU2SN7qj6ypyREzMOIeElFFZNv/MIpSuwO3zqg8OL97D54K2qgC9mLtElp9X8Y0w7O2JROhFT/hTIXG2bXu0eUlZZkXMTYs9RCw/58LOqcAkwmMQPbMzY7jtYgAzqe3KFU9p/hrIIZk+DQCVMeQt6SP6PmU01oPstEpxiIEdcpInwFbeXo+yc3XpzfOCdPnRlDGgUzUYsffzk9jVldHqVHdrlIgVFlxWK8JwZHrz//41s3Pv1EnsF9BkA0lR1AHlLRQzezVNMTSZUTktJUDwPVLt79aO3pm1/ckkfAtOiGcNTY0dU03voHBS6G6MG1PcYuRxOiScaUlITv/AsgeR8hrwwAAA=="
}
}

trait SegmentsModule extends scalan.common.impl.SegmentsDefs
