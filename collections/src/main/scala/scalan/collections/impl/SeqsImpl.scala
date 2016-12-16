package scalan.collections

import scala.collection.Seq
import scalan._
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
    lazy val typeArgs = TypeArgs("A" -> (eA -> scalan.util.Invariant))
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
        element[A], container[Seq])
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
    override lazy val typeArgs = TypeArgs("A" -> (eA -> scalan.util.Invariant))
    override lazy val eTo: Elem[_] = this
    override def convertSSeq(x: Rep[SSeq[A]]) = // Converter is not generated by meta
!!!("Cannot convert from SSeq to SSeqImpl: missing fields List(wrappedValue)")
    override def getDefaultRep = SSeqImpl(DefaultOfSeq[A])
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
    def getDefaultRep = reifyObject(new SSeqImplIso[A]()(eA))
    lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[SSeqImplIso[A]]
    }
    lazy val typeArgs = TypeArgs("A" -> (eA -> scalan.util.Invariant))
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
    case view1@ViewSSeq(Def(view2@ViewSSeq(arr, innerIso2)), innerIso1) =>
      val compIso = composeIso(innerIso1, innerIso2)
      implicit val eAB = compIso.eTo
      ViewSSeq(arr, compIso)

    case SSeqMethods.map(xs, f) => (xs, f) match {
      case (_, Def(IdentityLambda())) =>
        xs
      case (xs: RSeq[a] @unchecked, LambdaResultHasViews(f, iso: Iso[b, c])) =>
        val f1 = f.asRep[a => c]
        implicit val eB = iso.eFrom
        val s = xs.map(f1 >> iso.fromFun)
        val res = ViewSSeq(s, iso)
        res
      case (HasViews(source, Def(contIso: SSeqIso[a, b])), f: RFunc[_, c]@unchecked) =>
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
