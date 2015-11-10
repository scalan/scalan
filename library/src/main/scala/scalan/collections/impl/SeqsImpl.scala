package scalan.collections

import scala.collection.Seq
import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait SeqsAbs extends scalan.Scalan with Seqs {
  self: ScalanCommunityDsl =>

  // single proxy for each type family
  implicit def proxySSeq[A](p: Rep[SSeq[A]]): SSeq[A] = {
    proxyOps[SSeq[A]](p)(scala.reflect.classTag[SSeq[A]])
  }

  // TypeWrapper proxy
  //implicit def proxySeq[A:Elem](p: Rep[Seq[A]]): SSeq[A] =
  //  proxyOps[SSeq[A]](p.asRep[SSeq[A]])

  implicit def unwrapValueOfSSeq[A](w: Rep[SSeq[A]]): Rep[Seq[A]] = w.wrappedValue

  implicit def seqElement[A:Elem]: Elem[Seq[A]] =
    element[SSeq[A]].asInstanceOf[WrapperElem1[_, _, CBase, CW] forSome { type CBase[_]; type CW[_] }].baseElem.asInstanceOf[Elem[Seq[A]]]

  implicit def castSSeqElement[A](elem: Elem[SSeq[A]]): SSeqElem[A, SSeq[A]] =
    elem.asInstanceOf[SSeqElem[A, SSeq[A]]]

  implicit lazy val containerSeq: Cont[Seq] = new Cont[Seq] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[Seq[A]]
    def lift[A](implicit evA: Elem[A]) = element[Seq[A]]
  }

  implicit lazy val containerSSeq: Functor[SSeq] = new Functor[SSeq] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[SSeq[A]]
    def lift[A](implicit evA: Elem[A]) = element[SSeq[A]]
    def map[A:Elem,B:Elem](xs: Rep[SSeq[A]])(f: Rep[A] => Rep[B]) = xs.map(fun(f))
  }

  case class SSeqIso[A, B](innerIso: Iso[A, B]) extends Iso1UR[A, B, SSeq] {
    lazy val selfType = new ConcreteIsoElem[SSeq[A], SSeq[B], SSeqIso[A, B]](eFrom, eTo).
      asInstanceOf[Elem[IsoUR[SSeq[A], SSeq[B]]]]
    def cC = container[SSeq]
    def from(x: Rep[SSeq[B]]) = x.map(innerIso.fromFun)
    def to(x: Rep[SSeq[A]]) = x.map(innerIso.toFun)
  }

  def sSeqIso[A, B](innerIso: Iso[A, B]) =
    reifyObject(SSeqIso[A, B](innerIso)).asInstanceOf[Iso1[A, B, SSeq]]

  // familyElem
  class SSeqElem[A, To <: SSeq[A]](implicit _eA: Elem[A])
    extends WrapperElem1[A, To, Seq, SSeq](_eA, container[Seq], container[SSeq]) {
    def eA = _eA
    lazy val parent: Option[Elem[_]] = None
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[SSeq[A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[SSeq[A]] => convertSSeq(x) }
      tryConvert(element[SSeq[A]], this, x, conv)
    }

    def convertSSeq(x: Rep[SSeq[A]]): Rep[To] = {
      x.selfType1 match {
        case _: SSeqElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have SSeqElem[_, _], but got $e")
      }
    }
    lazy val baseElem =
      new BaseTypeElem1[A, Seq, SSeq[A]](this.asInstanceOf[Elem[SSeq[A]]])(
        element[A], container[Seq], DefaultOfSeq[A])
    lazy val eTo: Elem[_] = new SSeqImplElem[A](isoSSeqImpl(eA))(eA)
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sSeqElement[A](implicit eA: Elem[A]): Elem[SSeq[A]] =
    elemCache.getOrElseUpdate(
      (classOf[SSeqElem[A, SSeq[A]]], Seq(eA)),
      new SSeqElem[A, SSeq[A]]).asInstanceOf[Elem[SSeq[A]]]

  implicit case object SSeqCompanionElem extends CompanionElem[SSeqCompanionAbs] {
    lazy val tag = weakTypeTag[SSeqCompanionAbs]
    protected def getDefaultRep = SSeq
  }

  abstract class SSeqCompanionAbs extends CompanionDef[SSeqCompanionAbs] with SSeqCompanion {
    def selfType = SSeqCompanionElem
    override def toString = "SSeq"
  }
  def SSeq: Rep[SSeqCompanionAbs]
  implicit def proxySSeqCompanionAbs(p: Rep[SSeqCompanionAbs]): SSeqCompanionAbs =
    proxyOps[SSeqCompanionAbs](p)

  // default wrapper implementation
  abstract class SSeqImpl[A](val wrappedValue: Rep[Seq[A]])(implicit val eA: Elem[A]) extends SSeq[A] with Def[SSeqImpl[A]] {
    lazy val selfType = element[SSeqImpl[A]]

    def size: Rep[Int] =
      methodCallEx[Int](self,
        this.getClass.getMethod("size"),
        List())

    def apply(idx: Rep[Int]): Rep[A] =
      methodCallEx[A](self,
        this.getClass.getMethod("apply", classOf[AnyRef]),
        List(idx.asInstanceOf[AnyRef]))

    def slice(unc_from: Rep[Int], unc_until: Rep[Int]): Rep[SSeq[A]] =
      methodCallEx[SSeq[A]](self,
        this.getClass.getMethod("slice", classOf[AnyRef], classOf[AnyRef]),
        List(unc_from.asInstanceOf[AnyRef], unc_until.asInstanceOf[AnyRef]))

    def isEmpty: Rep[Boolean] =
      methodCallEx[Boolean](self,
        this.getClass.getMethod("isEmpty"),
        List())

    def map[B:Elem](f: Rep[A => B]): Rep[SSeq[B]] =
      methodCallEx[SSeq[B]](self,
        this.getClass.getMethod("map", classOf[AnyRef], classOf[Elem[B]]),
        List(f.asInstanceOf[AnyRef], element[B]))

    def reduce(op: Rep[((A, A)) => A]): Rep[A] =
      methodCallEx[A](self,
        this.getClass.getMethod("reduce", classOf[AnyRef]),
        List(op.asInstanceOf[AnyRef]))

    def filter(p: Rep[A => Boolean]): Rep[SSeq[A]] =
      methodCallEx[SSeq[A]](self,
        this.getClass.getMethod("filter", classOf[AnyRef]),
        List(p.asInstanceOf[AnyRef]))

    def $plus$colon(elem: Rep[A]): Rep[SSeq[A]] =
      methodCallEx[SSeq[A]](self,
        this.getClass.getMethod("$plus$colon", classOf[AnyRef]),
        List(elem.asInstanceOf[AnyRef]))

    def diff(that: Rep[SSeq[A]]): Rep[SSeq[A]] =
      methodCallEx[SSeq[A]](self,
        this.getClass.getMethod("diff", classOf[AnyRef]),
        List(that.asInstanceOf[AnyRef]))

    def toArray: Rep[Array[A]] =
      methodCallEx[Array[A]](self,
        this.getClass.getMethod("toArray"),
        List())

    def toList: Rep[List[A]] =
      methodCallEx[List[A]](self,
        this.getClass.getMethod("toList"),
        List())
  }
  trait SSeqImplCompanion
  // elem for concrete class
  class SSeqImplElem[A](val iso: Iso[SSeqImplData[A], SSeqImpl[A]])(implicit eA: Elem[A])
    extends SSeqElem[A, SSeqImpl[A]]
    with ConcreteElem1[A, SSeqImplData[A], SSeqImpl[A], SSeq] {
    override lazy val parent: Option[Elem[_]] = Some(sSeqElement(element[A]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA))
    }
    override lazy val eTo: Elem[_] = this
    override def convertSSeq(x: Rep[SSeq[A]]) = // Converter is not generated by meta
!!!("Cannot convert from SSeq to SSeqImpl: missing fields List(wrappedValue)")
    override def getDefaultRep = SSeqImpl(DefaultOfSeq[A].value)
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[SSeqImpl[A]]
    }
  }

  // state representation type
  type SSeqImplData[A] = Seq[A]

  // 3) Iso for concrete class
  class SSeqImplIso[A](implicit eA: Elem[A])
    extends IsoUR[SSeqImplData[A], SSeqImpl[A]] with Def[SSeqImplIso[A]] {
    override def from(p: Rep[SSeqImpl[A]]) =
      p.wrappedValue
    override def to(p: Rep[Seq[A]]) = {
      val wrappedValue = p
      SSeqImpl(wrappedValue)
    }
    lazy val eFrom = element[Seq[A]]
    lazy val eTo = new SSeqImplElem[A](self)
    lazy val selfType = new SSeqImplIsoElem[A](eA)
    def productArity = 1
    def productElement(n: Int) = eA
  }
  case class SSeqImplIsoElem[A](eA: Elem[A]) extends Elem[SSeqImplIso[A]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new SSeqImplIso[A]()(eA))
    lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[SSeqImplIso[A]]
    }
  }
  // 4) constructor and deconstructor
  class SSeqImplCompanionAbs extends CompanionDef[SSeqImplCompanionAbs] {
    def selfType = SSeqImplCompanionElem
    override def toString = "SSeqImpl"

    def apply[A](wrappedValue: Rep[Seq[A]])(implicit eA: Elem[A]): Rep[SSeqImpl[A]] =
      mkSSeqImpl(wrappedValue)
  }
  object SSeqImplMatcher {
    def unapply[A](p: Rep[SSeq[A]]) = unmkSSeqImpl(p)
  }
  lazy val SSeqImpl: Rep[SSeqImplCompanionAbs] = new SSeqImplCompanionAbs
  implicit def proxySSeqImplCompanion(p: Rep[SSeqImplCompanionAbs]): SSeqImplCompanionAbs = {
    proxyOps[SSeqImplCompanionAbs](p)
  }

  implicit case object SSeqImplCompanionElem extends CompanionElem[SSeqImplCompanionAbs] {
    lazy val tag = weakTypeTag[SSeqImplCompanionAbs]
    protected def getDefaultRep = SSeqImpl
  }

  implicit def proxySSeqImpl[A](p: Rep[SSeqImpl[A]]): SSeqImpl[A] =
    proxyOps[SSeqImpl[A]](p)

  implicit class ExtendedSSeqImpl[A](p: Rep[SSeqImpl[A]])(implicit eA: Elem[A]) {
    def toData: Rep[SSeqImplData[A]] = isoSSeqImpl(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSSeqImpl[A](implicit eA: Elem[A]): Iso[SSeqImplData[A], SSeqImpl[A]] =
    reifyObject(new SSeqImplIso[A]()(eA))

  // 6) smart constructor and deconstructor
  def mkSSeqImpl[A](wrappedValue: Rep[Seq[A]])(implicit eA: Elem[A]): Rep[SSeqImpl[A]]
  def unmkSSeqImpl[A](p: Rep[SSeq[A]]): Option[(Rep[Seq[A]])]

  registerModule(Seqs_Module)
}

// Seq -----------------------------------
trait SeqsSeq extends scalan.ScalanSeq with SeqsDsl {
  self: ScalanCommunityDslSeq =>
  lazy val SSeq: Rep[SSeqCompanionAbs] = new SSeqCompanionAbs {
    override def apply[A:Elem](arr: Rep[Array[A]]): Rep[SSeq[A]] =
      SSeqImpl(Seq.apply[A](arr: _*))

    override def empty[A:Elem]: Rep[SSeq[A]] =
      SSeqImpl(Seq.empty[A])

    override def single[A:Elem](elem: Rep[A]): Rep[SSeq[A]] =
      SSeqImpl(Seq.single[A](elem))

    override def fromList[A:Elem](list: Rep[List[A]]): Rep[SSeq[A]] =
      SSeqImpl(Seq.fromList[A](list))
  }

  // override proxy if we deal with TypeWrapper
  //override def proxySeq[A:Elem](p: Rep[Seq[A]]): SSeq[A] =
  //  proxyOpsEx[Seq[A], SSeq[A], SeqSSeqImpl[A]](p, bt => SeqSSeqImpl(bt))

  case class SeqSSeqImpl[A]
      (override val wrappedValue: Rep[Seq[A]])(implicit eA: Elem[A])
    extends SSeqImpl[A](wrappedValue) with SeqSSeq[A] {
    override def size: Rep[Int] =
      wrappedValue.size

    override def apply(idx: Rep[Int]): Rep[A] =
      wrappedValue.apply(idx)

    override def slice(unc_from: Rep[Int], unc_until: Rep[Int]): Rep[SSeq[A]] =
      SSeqImpl(wrappedValue.slice(unc_from, unc_until))

    override def isEmpty: Rep[Boolean] =
      wrappedValue.isEmpty

    override def reduce(op: Rep[((A, A)) => A]): Rep[A] =
      wrappedValue.reduce(scala.Function.untupled(op))

    override def filter(p: Rep[A => Boolean]): Rep[SSeq[A]] =
      SSeqImpl(wrappedValue.filter(p))

    override def $plus$colon(elem: Rep[A]): Rep[SSeq[A]] =
      SSeqImpl(wrappedValue.$plus$colon(elem))

    override def diff(that: Rep[SSeq[A]]): Rep[SSeq[A]] =
      SSeqImpl(wrappedValue.diff(that))

    override def toList: Rep[List[A]] =
      wrappedValue.toList
  }

  def mkSSeqImpl[A]
    (wrappedValue: Rep[Seq[A]])(implicit eA: Elem[A]): Rep[SSeqImpl[A]] =
    new SeqSSeqImpl[A](wrappedValue)
  def unmkSSeqImpl[A](p: Rep[SSeq[A]]) = p match {
    case p: SSeqImpl[A] @unchecked =>
      Some((p.wrappedValue))
    case _ => None
  }

  implicit def wrapSeqToSSeq[A:Elem](v: Seq[A]): SSeq[A] = SSeqImpl(v)
}

// Exp -----------------------------------
trait SeqsExp extends scalan.ScalanExp with SeqsDsl {
  self: ScalanCommunityDslExp =>
  lazy val SSeq: Rep[SSeqCompanionAbs] = new SSeqCompanionAbs {
    def apply[A:Elem](arr: Rep[Array[A]]): Rep[SSeq[A]] =
      methodCallEx[SSeq[A]](self,
        this.getClass.getMethod("apply", classOf[AnyRef], classOf[Elem[A]]),
        List(arr.asInstanceOf[AnyRef], element[A]))

    def empty[A:Elem]: Rep[SSeq[A]] =
      methodCallEx[SSeq[A]](self,
        this.getClass.getMethod("empty", classOf[Elem[A]]),
        List(element[A]))

    def single[A:Elem](elem: Rep[A]): Rep[SSeq[A]] =
      methodCallEx[SSeq[A]](self,
        this.getClass.getMethod("single", classOf[AnyRef], classOf[Elem[A]]),
        List(elem.asInstanceOf[AnyRef], element[A]))

    def fromList[A:Elem](list: Rep[List[A]]): Rep[SSeq[A]] =
      methodCallEx[SSeq[A]](self,
        this.getClass.getMethod("fromList", classOf[AnyRef], classOf[Elem[A]]),
        List(list.asInstanceOf[AnyRef], element[A]))
  }

  case class ViewSSeq[A, B](source: Rep[SSeq[A]], override val innerIso: Iso[A, B])
    extends View1[A, B, SSeq](sSeqIso(innerIso)) {
    override def toString = s"ViewSSeq[${innerIso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewSSeq[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
      case _ => false
    }
  }

  case class ExpSSeqImpl[A]
      (override val wrappedValue: Rep[Seq[A]])(implicit eA: Elem[A])
    extends SSeqImpl[A](wrappedValue)

  object SSeqImplMethods {
  }

  def mkSSeqImpl[A]
    (wrappedValue: Rep[Seq[A]])(implicit eA: Elem[A]): Rep[SSeqImpl[A]] =
    new ExpSSeqImpl[A](wrappedValue)
  def unmkSSeqImpl[A](p: Rep[SSeq[A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SSeqImplElem[A] @unchecked =>
      Some((p.asRep[SSeqImpl[A]].wrappedValue))
    case _ =>
      None
  }

  object SSeqMethods {
    object size {
      def unapply(d: Def[_]): Option[Rep[SSeq[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SSeqElem[_, _]] && method.getName == "size" =>
          Some(receiver).asInstanceOf[Option[Rep[SSeq[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SSeq[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[SSeq[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(idx, _*), _) if receiver.elem.isInstanceOf[SSeqElem[_, _]] && method.getName == "apply" =>
          Some((receiver, idx)).asInstanceOf[Option[(Rep[SSeq[A]], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SSeq[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object slice {
      def unapply(d: Def[_]): Option[(Rep[SSeq[A]], Rep[Int], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(unc_from, unc_until, _*), _) if receiver.elem.isInstanceOf[SSeqElem[_, _]] && method.getName == "slice" =>
          Some((receiver, unc_from, unc_until)).asInstanceOf[Option[(Rep[SSeq[A]], Rep[Int], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SSeq[A]], Rep[Int], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isEmpty {
      def unapply(d: Def[_]): Option[Rep[SSeq[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SSeqElem[_, _]] && method.getName == "isEmpty" =>
          Some(receiver).asInstanceOf[Option[Rep[SSeq[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SSeq[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object map {
      def unapply(d: Def[_]): Option[(Rep[SSeq[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[SSeqElem[_, _]] && method.getName == "map" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[SSeq[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SSeq[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[SSeq[A]], Rep[((A, A)) => A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(op, _*), _) if receiver.elem.isInstanceOf[SSeqElem[_, _]] && method.getName == "reduce" =>
          Some((receiver, op)).asInstanceOf[Option[(Rep[SSeq[A]], Rep[((A, A)) => A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SSeq[A]], Rep[((A, A)) => A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filter {
      def unapply(d: Def[_]): Option[(Rep[SSeq[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(p, _*), _) if receiver.elem.isInstanceOf[SSeqElem[_, _]] && method.getName == "filter" =>
          Some((receiver, p)).asInstanceOf[Option[(Rep[SSeq[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SSeq[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object +: {
      def unapply(d: Def[_]): Option[(Rep[SSeq[A]], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(elem, _*), _) if receiver.elem.isInstanceOf[SSeqElem[_, _]] && method.getName == "$plus$colon" =>
          Some((receiver, elem)).asInstanceOf[Option[(Rep[SSeq[A]], Rep[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SSeq[A]], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object diff {
      def unapply(d: Def[_]): Option[(Rep[SSeq[A]], Rep[SSeq[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(that, _*), _) if receiver.elem.isInstanceOf[SSeqElem[_, _]] && method.getName == "diff" =>
          Some((receiver, that)).asInstanceOf[Option[(Rep[SSeq[A]], Rep[SSeq[A]]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SSeq[A]], Rep[SSeq[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toArray {
      def unapply(d: Def[_]): Option[Rep[SSeq[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SSeqElem[_, _]] && method.getName == "toArray" =>
          Some(receiver).asInstanceOf[Option[Rep[SSeq[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SSeq[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toList {
      def unapply(d: Def[_]): Option[Rep[SSeq[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SSeqElem[_, _]] && method.getName == "toList" =>
          Some(receiver).asInstanceOf[Option[Rep[SSeq[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SSeq[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SSeqCompanionMethods {
    object apply {
      def unapply(d: Def[_]): Option[Rep[Array[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(arr, _*), _) if receiver.elem == SSeqCompanionElem && method.getName == "apply" =>
          Some(arr).asInstanceOf[Option[Rep[Array[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Array[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object empty {
      def unapply(d: Def[_]): Option[Unit forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem == SSeqCompanionElem && method.getName == "empty" =>
          Some(()).asInstanceOf[Option[Unit forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object single {
      def unapply(d: Def[_]): Option[Rep[A] forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(elem, _*), _) if receiver.elem == SSeqCompanionElem && method.getName == "single" =>
          Some(elem).asInstanceOf[Option[Rep[A] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[A] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromList {
      def unapply(d: Def[_]): Option[Rep[List[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(list, _*), _) if receiver.elem == SSeqCompanionElem && method.getName == "fromList" =>
          Some(list).asInstanceOf[Option[Rep[List[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[List[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object UserTypeSSeq {
    def unapply(s: Exp[_]): Option[Iso[_, _]] = {
      s.elem match {
        case e: SSeqElem[a,to] => e.eItem match {
          case UnpackableElem(iso) => Some(iso)
          case _ => None
        }
        case _ => None
      }
    }
  }

  override def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
    case Def(view: ViewSSeq[_, _]) =>
      Some((view.source, view.iso))
    case UserTypeSSeq(iso: Iso[a, b]) =>
      val newIso = sSeqIso(iso)
      val repr = reifyObject(UnpackView(s.asRep[SSeq[b]], newIso))
      Some((repr, newIso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  override def rewriteDef[T](d: Def[T]) = d match {
    case SSeqMethods.map(xs, Def(IdentityLambda())) => xs

    case view1@ViewSSeq(Def(view2@ViewSSeq(arr, innerIso2)), innerIso1) =>
      val compIso = composeIso(innerIso1, innerIso2)
      implicit val eAB = compIso.eTo
      ViewSSeq(arr, compIso)

    // Rule: W(a).m(args) ==> iso.to(a.m(unwrap(args)))
    case mc @ MethodCall(Def(wrapper: ExpSSeqImpl[_]), m, args, neverInvoke) if !isValueAccessor(m) =>
      val resultElem = mc.selfType
      val wrapperIso = getIsoByElem(resultElem)
      wrapperIso match {
        case iso: Iso[base,ext] =>
          val eRes = iso.eFrom
          val newCall = unwrapMethodCall(mc, wrapper.wrappedValue, eRes)
          iso.to(newCall)
      }

    case SSeqMethods.map(xs, f) => (xs, f) match {
      case (xs: RSeq[a] @unchecked, LambdaResultHasViews(f, iso: Iso[b, c])) =>
        val f1 = f.asRep[a => c]
        implicit val eB = iso.eFrom
        val s = xs.map(f1 >> iso.fromFun)
        val res = ViewSSeq(s, iso)
        res
      case (HasViews(source, Def(contIso: SSeqIso[a, b])), f: Rep[Function1[_, c] @unchecked]) =>
        val f1 = f.asRep[b => c]
        val iso = contIso.innerIso
        implicit val eC = f1.elem.eRange
        source.asRep[SSeq[a]].map(iso.toFun >> f1)
      case _ =>
        super.rewriteDef(d)
    }

    case _ => super.rewriteDef(d)
  }
}

object Seqs_Module extends scalan.ModuleInfo {
  val dump = "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"
}
}

trait SeqsDsl extends impl.SeqsAbs {self: ScalanCommunityDsl =>}
