package scalan.common

import scala.reflect.runtime.universe._
import scalan._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait SegmentsAbs extends Segments with scalan.Scalan {
  self: SegmentsDsl =>

  // single proxy for each type family
  implicit def proxySegment(p: Rep[Segment]): Segment = {
    proxyOps[Segment](p)(scala.reflect.classTag[Segment])
  }

  // familyElem
  class SegmentElem[To <: Segment]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    lazy val entityDef: STraitOrClassDef = {
      val module = getModules("Segments")
      module.entities.find(_.name == "Segment").get
    }
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }
    override def isEntityType = true
    override lazy val tag = {
      weakTypeTag[Segment].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Segment] => convertSegment(x) }
      tryConvert(element[Segment], this, x, conv)
    }

    def convertSegment(x : Rep[Segment]): Rep[To] = {
      assert(x.selfType1 match { case _: SegmentElem[_] => true; case _ => false })
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def segmentElement: Elem[Segment] =
    cachedElem[SegmentElem[Segment]]()

  implicit case object SegmentCompanionElem extends CompanionElem[SegmentCompanionAbs] {
    lazy val tag = weakTypeTag[SegmentCompanionAbs]
    protected def getDefaultRep = Segment
  }

  abstract class SegmentCompanionAbs extends CompanionBase[SegmentCompanionAbs] with SegmentCompanion {
    override def toString = "Segment"
  }
  def Segment: Rep[SegmentCompanionAbs]
  implicit def proxySegmentCompanion(p: Rep[SegmentCompanion]): SegmentCompanion =
    proxyOps[SegmentCompanion](p)

  // elem for concrete class
  class IntervalElem(val iso: Iso[IntervalData, Interval])
    extends SegmentElem[Interval]
    with ConcreteElem[IntervalData, Interval] {
    override lazy val parent: Option[Elem[_]] = Some(segmentElement)
    override lazy val entityDef = {
      val module = getModules("Segments")
      module.concreteSClasses.find(_.name == "Interval").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }

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
    extends Iso[IntervalData, Interval]()(pairElement(implicitly[Elem[Int]], implicitly[Elem[Int]])) {
    override def from(p: Rep[Interval]) =
      (p.start, p.end)
    override def to(p: Rep[(Int, Int)]) = {
      val Pair(start, end) = p
      Interval(start, end)
    }
    lazy val eTo = new IntervalElem(this)
  }
  // 4) constructor and deconstructor
  abstract class IntervalCompanionAbs extends CompanionBase[IntervalCompanionAbs] with IntervalCompanion {
    override def toString = "Interval"
    def apply(p: Rep[IntervalData]): Rep[Interval] =
      isoInterval.to(p)
    def apply(start: Rep[Int], end: Rep[Int]): Rep[Interval] =
      mkInterval(start, end)
  }
  object IntervalMatcher {
    def unapply(p: Rep[Segment]) = unmkInterval(p)
  }
  def Interval: Rep[IntervalCompanionAbs]
  implicit def proxyIntervalCompanion(p: Rep[IntervalCompanionAbs]): IntervalCompanionAbs = {
    proxyOps[IntervalCompanionAbs](p)
  }

  implicit case object IntervalCompanionElem extends CompanionElem[IntervalCompanionAbs] {
    lazy val tag = weakTypeTag[IntervalCompanionAbs]
    protected def getDefaultRep = Interval
  }

  implicit def proxyInterval(p: Rep[Interval]): Interval =
    proxyOps[Interval](p)

  implicit class ExtendedInterval(p: Rep[Interval]) {
    def toData: Rep[IntervalData] = isoInterval.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoInterval: Iso[IntervalData, Interval] =
    cachedIso[IntervalIso]()

  // 6) smart constructor and deconstructor
  def mkInterval(start: Rep[Int], end: Rep[Int]): Rep[Interval]
  def unmkInterval(p: Rep[Segment]): Option[(Rep[Int], Rep[Int])]

  // elem for concrete class
  class SliceElem(val iso: Iso[SliceData, Slice])
    extends SegmentElem[Slice]
    with ConcreteElem[SliceData, Slice] {
    override lazy val parent: Option[Elem[_]] = Some(segmentElement)
    override lazy val entityDef = {
      val module = getModules("Segments")
      module.concreteSClasses.find(_.name == "Slice").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }

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
    extends Iso[SliceData, Slice]()(pairElement(implicitly[Elem[Int]], implicitly[Elem[Int]])) {
    override def from(p: Rep[Slice]) =
      (p.start, p.length)
    override def to(p: Rep[(Int, Int)]) = {
      val Pair(start, length) = p
      Slice(start, length)
    }
    lazy val eTo = new SliceElem(this)
  }
  // 4) constructor and deconstructor
  abstract class SliceCompanionAbs extends CompanionBase[SliceCompanionAbs] with SliceCompanion {
    override def toString = "Slice"
    def apply(p: Rep[SliceData]): Rep[Slice] =
      isoSlice.to(p)
    def apply(start: Rep[Int], length: Rep[Int]): Rep[Slice] =
      mkSlice(start, length)
  }
  object SliceMatcher {
    def unapply(p: Rep[Segment]) = unmkSlice(p)
  }
  def Slice: Rep[SliceCompanionAbs]
  implicit def proxySliceCompanion(p: Rep[SliceCompanionAbs]): SliceCompanionAbs = {
    proxyOps[SliceCompanionAbs](p)
  }

  implicit case object SliceCompanionElem extends CompanionElem[SliceCompanionAbs] {
    lazy val tag = weakTypeTag[SliceCompanionAbs]
    protected def getDefaultRep = Slice
  }

  implicit def proxySlice(p: Rep[Slice]): Slice =
    proxyOps[Slice](p)

  implicit class ExtendedSlice(p: Rep[Slice]) {
    def toData: Rep[SliceData] = isoSlice.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSlice: Iso[SliceData, Slice] =
    cachedIso[SliceIso]()

  // 6) smart constructor and deconstructor
  def mkSlice(start: Rep[Int], length: Rep[Int]): Rep[Slice]
  def unmkSlice(p: Rep[Segment]): Option[(Rep[Int], Rep[Int])]

  // elem for concrete class
  class CenteredElem(val iso: Iso[CenteredData, Centered])
    extends SegmentElem[Centered]
    with ConcreteElem[CenteredData, Centered] {
    override lazy val parent: Option[Elem[_]] = Some(segmentElement)
    override lazy val entityDef = {
      val module = getModules("Segments")
      module.concreteSClasses.find(_.name == "Centered").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }

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
    extends Iso[CenteredData, Centered]()(pairElement(implicitly[Elem[Int]], implicitly[Elem[Int]])) {
    override def from(p: Rep[Centered]) =
      (p.center, p.radius)
    override def to(p: Rep[(Int, Int)]) = {
      val Pair(center, radius) = p
      Centered(center, radius)
    }
    lazy val eTo = new CenteredElem(this)
  }
  // 4) constructor and deconstructor
  abstract class CenteredCompanionAbs extends CompanionBase[CenteredCompanionAbs] with CenteredCompanion {
    override def toString = "Centered"
    def apply(p: Rep[CenteredData]): Rep[Centered] =
      isoCentered.to(p)
    def apply(center: Rep[Int], radius: Rep[Int]): Rep[Centered] =
      mkCentered(center, radius)
  }
  object CenteredMatcher {
    def unapply(p: Rep[Segment]) = unmkCentered(p)
  }
  def Centered: Rep[CenteredCompanionAbs]
  implicit def proxyCenteredCompanion(p: Rep[CenteredCompanionAbs]): CenteredCompanionAbs = {
    proxyOps[CenteredCompanionAbs](p)
  }

  implicit case object CenteredCompanionElem extends CompanionElem[CenteredCompanionAbs] {
    lazy val tag = weakTypeTag[CenteredCompanionAbs]
    protected def getDefaultRep = Centered
  }

  implicit def proxyCentered(p: Rep[Centered]): Centered =
    proxyOps[Centered](p)

  implicit class ExtendedCentered(p: Rep[Centered]) {
    def toData: Rep[CenteredData] = isoCentered.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoCentered: Iso[CenteredData, Centered] =
    cachedIso[CenteredIso]()

  // 6) smart constructor and deconstructor
  def mkCentered(center: Rep[Int], radius: Rep[Int]): Rep[Centered]
  def unmkCentered(p: Rep[Segment]): Option[(Rep[Int], Rep[Int])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(Segments_Module.dump))
}

// Seq -----------------------------------
trait SegmentsSeq extends SegmentsDsl with scalan.ScalanSeq {
  self: SegmentsDslSeq =>
  lazy val Segment: Rep[SegmentCompanionAbs] = new SegmentCompanionAbs with UserTypeSeq[SegmentCompanionAbs] {
    lazy val selfType = element[SegmentCompanionAbs]
  }

  case class SeqInterval
      (override val start: Rep[Int], override val end: Rep[Int])

    extends Interval(start, end)
        with UserTypeSeq[Interval] {
    lazy val selfType = element[Interval]
  }
  lazy val Interval = new IntervalCompanionAbs with UserTypeSeq[IntervalCompanionAbs] {
    lazy val selfType = element[IntervalCompanionAbs]
  }

  def mkInterval
      (start: Rep[Int], end: Rep[Int]): Rep[Interval] =
      new SeqInterval(start, end)
  def unmkInterval(p: Rep[Segment]) = p match {
    case p: Interval @unchecked =>
      Some((p.start, p.end))
    case _ => None
  }

  case class SeqSlice
      (override val start: Rep[Int], override val length: Rep[Int])

    extends Slice(start, length)
        with UserTypeSeq[Slice] {
    lazy val selfType = element[Slice]
  }
  lazy val Slice = new SliceCompanionAbs with UserTypeSeq[SliceCompanionAbs] {
    lazy val selfType = element[SliceCompanionAbs]
  }

  def mkSlice
      (start: Rep[Int], length: Rep[Int]): Rep[Slice] =
      new SeqSlice(start, length)
  def unmkSlice(p: Rep[Segment]) = p match {
    case p: Slice @unchecked =>
      Some((p.start, p.length))
    case _ => None
  }

  case class SeqCentered
      (override val center: Rep[Int], override val radius: Rep[Int])

    extends Centered(center, radius)
        with UserTypeSeq[Centered] {
    lazy val selfType = element[Centered]
  }
  lazy val Centered = new CenteredCompanionAbs with UserTypeSeq[CenteredCompanionAbs] {
    lazy val selfType = element[CenteredCompanionAbs]
  }

  def mkCentered
      (center: Rep[Int], radius: Rep[Int]): Rep[Centered] =
      new SeqCentered(center, radius)
  def unmkCentered(p: Rep[Segment]) = p match {
    case p: Centered @unchecked =>
      Some((p.center, p.radius))
    case _ => None
  }
}

// Exp -----------------------------------
trait SegmentsExp extends SegmentsDsl with scalan.ScalanExp {
  self: SegmentsDslExp =>
  lazy val Segment: Rep[SegmentCompanionAbs] = new SegmentCompanionAbs with UserTypeDef[SegmentCompanionAbs] {
    lazy val selfType = element[SegmentCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpInterval
      (override val start: Rep[Int], override val end: Rep[Int])

    extends Interval(start, end) with UserTypeDef[Interval] {
    lazy val selfType = element[Interval]
    override def mirror(t: Transformer) = ExpInterval(t(start), t(end))
  }

  lazy val Interval: Rep[IntervalCompanionAbs] = new IntervalCompanionAbs with UserTypeDef[IntervalCompanionAbs] {
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
    (start: Rep[Int], end: Rep[Int]): Rep[Interval] =
    new ExpInterval(start, end)
  def unmkInterval(p: Rep[Segment]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IntervalElem @unchecked =>
      Some((p.asRep[Interval].start, p.asRep[Interval].end))
    case _ =>
      None
  }

  case class ExpSlice
      (override val start: Rep[Int], override val length: Rep[Int])

    extends Slice(start, length) with UserTypeDef[Slice] {
    lazy val selfType = element[Slice]
    override def mirror(t: Transformer) = ExpSlice(t(start), t(length))
  }

  lazy val Slice: Rep[SliceCompanionAbs] = new SliceCompanionAbs with UserTypeDef[SliceCompanionAbs] {
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
    (start: Rep[Int], length: Rep[Int]): Rep[Slice] =
    new ExpSlice(start, length)
  def unmkSlice(p: Rep[Segment]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SliceElem @unchecked =>
      Some((p.asRep[Slice].start, p.asRep[Slice].length))
    case _ =>
      None
  }

  case class ExpCentered
      (override val center: Rep[Int], override val radius: Rep[Int])

    extends Centered(center, radius) with UserTypeDef[Centered] {
    lazy val selfType = element[Centered]
    override def mirror(t: Transformer) = ExpCentered(t(center), t(radius))
  }

  lazy val Centered: Rep[CenteredCompanionAbs] = new CenteredCompanionAbs with UserTypeDef[CenteredCompanionAbs] {
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
    (center: Rep[Int], radius: Rep[Int]): Rep[Centered] =
    new ExpCentered(center, radius)
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

object Segments_Module {
  val packageName = "scalan.common"
  val name = "Segments"
  val dump = "H4sIAAAAAAAAALVWQWwbRRQdbxI7a4c0idRKyaUhGFoK2BES6iGHKrguquQmUbYg5FZI492xM2V2drMzjmwOPXCEG+q1qnrvjQsSEheEhDhwQoDEmVMBoQraE1X/zM6u12m2sVDrw2h25u//77/3/1/f+xPNiAi9JlzMMK/5ROKao/ebQladJpdUDq8EXp+Ri6T76amv3Cv8XWGhE21U3MPiomBtZMeb5iBM9w7ZbyEbc5cIGURCopdbOkLdDRgjrqQBr1Pf70vcYaTeokJutNB0J/CG++gmKrTQghtwNyKSOA2GhSDCnM8ShYimz7Z+Hm6Hoxi8rrKoZ7K4GmEqAT7EWIjtd0noDHnAh75E8wbadqhggU2J+mEQySRECdztBV7yOM0xHKCl1g18gOsQold3ZER5D96shNj9GPfIFpgo82kALAjrXh2G+nmqhcqC7ANBl/2Q6ZNBiBACBd7WIGojfmopPzXFT9UhEcWMfoLV5U4UDIYo/hWmEBqE4OLNY1wkHkiTe9XPrrvXHjkV31IvDxSUks6wCI5O51SDlgJ4/H73C/HgvbvnLVRuozIVmx0hI+zKrOSGrQrmPJAac0ogjnqg1lqeWjrKJtgcKgnbDfwQc/BkqJwDnRh1qVTG6mzOqJNDfUmGJDEtDMJCmu9qTr66bhqYsZ37y2+9+kfzQwtZ4yFscOlA4UeJU4lKDun5UGWaUrXYht38OGnGZ+7/5X23jq5bKU/G7WTSgIsZ8evPlZ9ev2Ch2bYu5EsM99pAlWgy4m9HjYDLNpoNDkgU35QOMFO7I6UqeaSL+0waArOZT0HmEq3mtlxIFC0burwLCQGVuEK3Ak6ql3aqD50fbt1TBRihufgm7sHH9Px/v813pa5NCQNKYtOOJySagt5N6XglT7mQ7ETUh0lxQN759uv3//5ma0aLt2Qy+gCzPon71iQ0Sk7FLKxDpMuxivZAxzuZZqKWFbgn3HsalVpWR7qP1C/HKTqBTxbXHtCP7n4utc6Fwfg02e7cgPbd0O8tP0PyZKr92163/ln+5Y6FbFC2Q6WPw+r6hL34AvsLpVyNlhWgdhFoJRGUXSMbb2U0gpb0VqLZxPDQfSXpXMN9bsdpZxnbpxTMq6yMhke/WGSE9+TexOqr9axe38ijZd4BrslxnMxoqxdGSNElivP/w0iEPdoXz5ORxYYGQ7xjCyUxHN1noBZNlHGO7F1Cu1R9GyfizsCeGPuCcXME9EPfiJVDka6NH6r0jDV8NF8yswBa1TddfBZGxFrOiHBMg8KUuPno9ta5H7/8XU/Bsmp1GMI8/ZOTnX7jhFSS6PCvJQNVomnV/hrsE5NwAOFFCgAA"
}
}

trait SegmentsDsl extends impl.SegmentsAbs {self: SegmentsDsl =>}
trait SegmentsDslSeq extends impl.SegmentsSeq {self: SegmentsDslSeq =>}
trait SegmentsDslExp extends impl.SegmentsExp {self: SegmentsDslExp =>}
