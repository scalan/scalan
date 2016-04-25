package scalan.collections

import scala.collection.Seq
import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait SeqsAbs extends scalan.ScalanDsl with Seqs {
  self: SeqsDsl =>

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
    def unlift[A](implicit eFT: Elem[Seq[A]]) =
      castSSeqElement(eFT.asInstanceOf[Elem[SSeq[A]]]).eA
    def getElem[A](fa: Rep[Seq[A]]) = !!!("Operation is not supported by Seq container " + fa)
    def unapply[T](e: Elem[_]) = e match {
      case e: BaseTypeElem1[_,_,_] if e.wrapperElem.isInstanceOf[SSeqElem[_,_]] => Some(e.asElem[Seq[T]])
      case _ => None
    }
  }

  implicit lazy val containerSSeq: Functor[SSeq] = new Functor[SSeq] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[SSeq[A]]
    def lift[A](implicit evA: Elem[A]) = element[SSeq[A]]
    def unlift[A](implicit eFT: Elem[SSeq[A]]) =
      castSSeqElement(eFT).eA
    def getElem[A](fa: Rep[SSeq[A]]) = fa.selfType1
    def unapply[T](e: Elem[_]) = e match {
      case e: SSeqElem[_,_] => Some(e.asElem[SSeq[T]])
      case _ => None
    }
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
        case e => !!!(s"Expected $x to have SSeqElem[_, _], but got $e", x)
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
  class SSeqImplElem[A](val iso: Iso[SSeqImplData[A], SSeqImpl[A]])(implicit override val eA: Elem[A])
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
    extends EntityIso[SSeqImplData[A], SSeqImpl[A]] with Def[SSeqImplIso[A]] {
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

    @scalan.OverloadId("fromFields")
    def apply[A](wrappedValue: Rep[Seq[A]])(implicit eA: Elem[A]): Rep[SSeqImpl[A]] =
      mkSSeqImpl(wrappedValue)

    def unapply[A](p: Rep[SSeq[A]]) = unmkSSeqImpl(p)
  }
  lazy val SSeqImplRep: Rep[SSeqImplCompanionAbs] = new SSeqImplCompanionAbs
  lazy val SSeqImpl: SSeqImplCompanionAbs = proxySSeqImplCompanion(SSeqImplRep)
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

// Std -----------------------------------
trait SeqsStd extends scalan.ScalanDslStd with SeqsDsl {
  self: SeqsDslStd =>
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
  //  proxyOpsEx[Seq[A], SSeq[A], StdSSeqImpl[A]](p, bt => StdSSeqImpl(bt))

  case class StdSSeqImpl[A]
      (override val wrappedValue: Rep[Seq[A]])(implicit eA: Elem[A])
    extends SSeqImpl[A](wrappedValue) with SSeqImplDecls[A] {
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
    new StdSSeqImpl[A](wrappedValue)
  def unmkSSeqImpl[A](p: Rep[SSeq[A]]) = p match {
    case p: SSeqImpl[A] @unchecked =>
      Some((p.wrappedValue))
    case _ => None
  }

  implicit def wrapSeqToSSeq[A:Elem](v: Seq[A]): SSeq[A] = SSeqImpl(v)
}

// Exp -----------------------------------
trait SeqsExp extends scalan.ScalanDslExp with SeqsDsl {
  self: SeqsDslExp =>
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

trait SeqsDsl extends impl.SeqsAbs
