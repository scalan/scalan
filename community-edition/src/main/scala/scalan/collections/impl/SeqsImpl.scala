package scalan.collections
package impl

import scala.collection.Seq
import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe._
import scala.reflect._
import scalan.common.Default

// Abs -----------------------------------
trait SeqsAbs extends Scalan with Seqs {
  self: ScalanCommunityDsl =>
  // single proxy for each type family
  implicit def proxySSeq[A](p: Rep[SSeq[A]]): SSeq[A] = {
    implicit val tag = weakTypeTag[SSeq[A]]
    proxyOps[SSeq[A]](p)(TagImplicits.typeTagToClassTag[SSeq[A]])
  }
  // BaseTypeEx proxy
  //implicit def proxySeq[A:Elem](p: Rep[Seq[A]]): SSeq[A] =
  //  proxyOps[SSeq[A]](p.asRep[SSeq[A]])

  implicit def unwrapValueOfSSeq[A](w: Rep[SSeq[A]]): Rep[Seq[A]] = w.wrappedValueOfBaseType

  implicit def defaultSSeqElem[A:Elem]: Elem[SSeq[A]] = element[SSeqImpl[A]].asElem[SSeq[A]]
  implicit def SeqElement[A:Elem:WeakTypeTag]: Elem[Seq[A]]

  implicit def castSSeqElement[A](elem: Elem[SSeq[A]]): SSeqElem[A, _,SSeq[A]] = elem.asInstanceOf[SSeqElem[A, _,SSeq[A]]]
  implicit val containerSSeq: Cont[SSeq] = new Container[SSeq] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[SSeq[A]]
    def lift[A](implicit evA: Elem[A]) = element[SSeq[A]]
  }
  case class SSeqIso[A,B](iso: Iso[A,B]) extends Iso1[A, B, SSeq](iso) {
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    def from(x: Rep[SSeq[B]]) = x.map(iso.from _)
    def to(x: Rep[SSeq[A]]) = x.map(iso.to _)
    lazy val defaultRepTo = Default.defaultVal(SSeq.empty[B])
  }
  abstract class SSeqElem[A, From, To <: SSeq[A]](iso: Iso[From, To])(implicit eA: Elem[A])
    extends ViewElem1[A, From, To, SSeq](iso) {
    override def convert(x: Rep[Reifiable[_]]) = convertSSeq(x.asRep[SSeq[A]])
    def convertSSeq(x : Rep[SSeq[A]]): Rep[To]
  }
  trait SSeqCompanionElem extends CompanionElem[SSeqCompanionAbs]
  implicit lazy val SSeqCompanionElem: SSeqCompanionElem = new SSeqCompanionElem {
    lazy val tag = weakTypeTag[SSeqCompanionAbs]
    protected def getDefaultRep = SSeq
  }

  abstract class SSeqCompanionAbs extends CompanionBase[SSeqCompanionAbs] with SSeqCompanion {
    override def toString = "SSeq"

    def apply[A:Elem](arr: Rep[Array[A]]): Rep[SSeq[A]] =
      methodCallEx[SSeq[A]](self,
        this.getClass.getMethod("apply", classOf[AnyRef], classOf[Elem[A]]),
        List(arr.asInstanceOf[AnyRef], element[A]))

    def empty[A:Elem]: Rep[SSeq[A]] =
      methodCallEx[SSeq[A]](self,
        this.getClass.getMethod("empty", classOf[Elem[A]]),
        List(element[A]))

    def fromList[A:Elem](list: Rep[List[A]]): Rep[SSeq[A]] =
      methodCallEx[SSeq[A]](self,
        this.getClass.getMethod("fromList", classOf[AnyRef], classOf[Elem[A]]),
        List(list.asInstanceOf[AnyRef], element[A]))
  }
  def SSeq: Rep[SSeqCompanionAbs]
  implicit def proxySSeqCompanion(p: Rep[SSeqCompanion]): SSeqCompanion = {
    proxyOps[SSeqCompanion](p)
  }

  // default wrapper implementation
  abstract class SSeqImpl[A](val wrappedValueOfBaseType: Rep[Seq[A]])(implicit val eA: Elem[A]) extends SSeq[A] {
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

    def reduce(op: Rep[((A,A)) => A]): Rep[A] =
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
  }
  trait SSeqImplCompanion
  // elem for concrete class
  class SSeqImplElem[A](iso: Iso[SSeqImplData[A], SSeqImpl[A]])(implicit val eA: Elem[A])
    extends SSeqElem[A, SSeqImplData[A], SSeqImpl[A]](iso) {
    def convertSSeq(x: Rep[SSeq[A]]) = SSeqImpl(x.wrappedValueOfBaseType)
  }

  // state representation type
  type SSeqImplData[A] = Seq[A]

  // 3) Iso for concrete class
  class SSeqImplIso[A](implicit eA: Elem[A])
    extends Iso[SSeqImplData[A], SSeqImpl[A]] {
    override def from(p: Rep[SSeqImpl[A]]) =
      unmkSSeqImpl(p) match {
        case Some((wrappedValueOfBaseType)) => wrappedValueOfBaseType
        case None => !!!
      }
    override def to(p: Rep[Seq[A]]) = {
      val wrappedValueOfBaseType = p
      SSeqImpl(wrappedValueOfBaseType)
    }
    lazy val tag = {
      weakTypeTag[SSeqImpl[A]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[SSeqImpl[A]]](SSeqImpl(DefaultOfSeq[A].value))
    lazy val eTo = new SSeqImplElem[A](this)
  }
  // 4) constructor and deconstructor
  abstract class SSeqImplCompanionAbs extends CompanionBase[SSeqImplCompanionAbs] with SSeqImplCompanion {
    override def toString = "SSeqImpl"

    def apply[A](wrappedValueOfBaseType: Rep[Seq[A]])(implicit eA: Elem[A]): Rep[SSeqImpl[A]] =
      mkSSeqImpl(wrappedValueOfBaseType)
    def unapply[A:Elem](p: Rep[SSeqImpl[A]]) = unmkSSeqImpl(p)
  }
  def SSeqImpl: Rep[SSeqImplCompanionAbs]
  implicit def proxySSeqImplCompanion(p: Rep[SSeqImplCompanionAbs]): SSeqImplCompanionAbs = {
    proxyOps[SSeqImplCompanionAbs](p)
  }

  class SSeqImplCompanionElem extends CompanionElem[SSeqImplCompanionAbs] {
    lazy val tag = weakTypeTag[SSeqImplCompanionAbs]
    protected def getDefaultRep = SSeqImpl
  }
  implicit lazy val SSeqImplCompanionElem: SSeqImplCompanionElem = new SSeqImplCompanionElem

  implicit def proxySSeqImpl[A](p: Rep[SSeqImpl[A]]): SSeqImpl[A] =
    proxyOps[SSeqImpl[A]](p)

  implicit class ExtendedSSeqImpl[A](p: Rep[SSeqImpl[A]])(implicit eA: Elem[A]) {
    def toData: Rep[SSeqImplData[A]] = isoSSeqImpl(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSSeqImpl[A](implicit eA: Elem[A]): Iso[SSeqImplData[A], SSeqImpl[A]] =
    new SSeqImplIso[A]

  // 6) smart constructor and deconstructor
  def mkSSeqImpl[A](wrappedValueOfBaseType: Rep[Seq[A]])(implicit eA: Elem[A]): Rep[SSeqImpl[A]]
  def unmkSSeqImpl[A:Elem](p: Rep[SSeqImpl[A]]): Option[(Rep[Seq[A]])]
}

// Seq -----------------------------------
trait SeqsSeq extends SeqsDsl with ScalanSeq {
  self: ScalanCommunityDslSeq =>
  lazy val SSeq: Rep[SSeqCompanionAbs] = new SSeqCompanionAbs with UserTypeSeq[SSeqCompanionAbs, SSeqCompanionAbs] {
    lazy val selfType = element[SSeqCompanionAbs]
    override def apply[A:Elem](arr: Rep[Array[A]]): Rep[SSeq[A]] =
      SSeqImpl(Seq.apply[A](arr: _*))

    override def empty[A:Elem]: Rep[SSeq[A]] =
      SSeqImpl(Seq.empty[A])

    override def fromList[A:Elem](list: Rep[List[A]]): Rep[SSeq[A]] =
      SSeqImpl(Seq.fromList[A](list))
  }

    // override proxy if we deal with BaseTypeEx
  //override def proxySeq[A:Elem](p: Rep[Seq[A]]): SSeq[A] =
  //  proxyOpsEx[Seq[A],SSeq[A], SeqSSeqImpl[A]](p, bt => SeqSSeqImpl(bt))

    implicit def SeqElement[A:Elem:WeakTypeTag]: Elem[Seq[A]] = new SeqBaseElemEx[Seq[A], SSeq[A]](element[SSeq[A]])(weakTypeTag[Seq[A]], DefaultOfSeq[A])

  case class SeqSSeqImpl[A]
      (override val wrappedValueOfBaseType: Rep[Seq[A]])
      (implicit eA: Elem[A])
    extends SSeqImpl[A](wrappedValueOfBaseType)
       with SeqSSeq[A] with UserTypeSeq[SSeq[A], SSeqImpl[A]] {
    lazy val selfType = element[SSeqImpl[A]].asInstanceOf[Elem[SSeq[A]]]
    override def size: Rep[Int] =
      wrappedValueOfBaseType.size

    override def apply(idx: Rep[Int]): Rep[A] =
      wrappedValueOfBaseType.apply(idx)

    override def slice(unc_from: Rep[Int], unc_until: Rep[Int]): Rep[SSeq[A]] =
      SSeqImpl(wrappedValueOfBaseType.slice(unc_from, unc_until))

    override def isEmpty: Rep[Boolean] =
      wrappedValueOfBaseType.isEmpty

    override def reduce(op: Rep[((A,A)) => A]): Rep[A] =
      wrappedValueOfBaseType.reduce(scala.Function.untupled(op))

    override def filter(p: Rep[A => Boolean]): Rep[SSeq[A]] =
      SSeqImpl(wrappedValueOfBaseType.filter(p))

    override def $plus$colon(elem: Rep[A]): Rep[SSeq[A]] =
      SSeqImpl(wrappedValueOfBaseType.$plus$colon(elem))

    override def diff(that: Rep[SSeq[A]]): Rep[SSeq[A]] =
      SSeqImpl(wrappedValueOfBaseType.diff(that))
  }
  lazy val SSeqImpl = new SSeqImplCompanionAbs with UserTypeSeq[SSeqImplCompanionAbs, SSeqImplCompanionAbs] {
    lazy val selfType = element[SSeqImplCompanionAbs]
  }

  def mkSSeqImpl[A]
      (wrappedValueOfBaseType: Rep[Seq[A]])(implicit eA: Elem[A]) =
      new SeqSSeqImpl[A](wrappedValueOfBaseType)
  def unmkSSeqImpl[A:Elem](p: Rep[SSeqImpl[A]]) =
    Some((p.wrappedValueOfBaseType))

  implicit def wrapSeqToSSeq[A:Elem](v: Seq[A]): SSeq[A] = SSeqImpl(v)
}

// Exp -----------------------------------
trait SeqsExp extends SeqsDsl with ScalanExp {
  self: ScalanCommunityDslExp =>
  lazy val SSeq: Rep[SSeqCompanionAbs] = new SSeqCompanionAbs with UserTypeDef[SSeqCompanionAbs, SSeqCompanionAbs] {
    lazy val selfType = element[SSeqCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ViewSSeq[A, B](source: Rep[SSeq[A]])(iso: Iso1[A, B, SSeq])
    extends View1[A, B, SSeq](iso) {
    def copy(source: Rep[SSeq[A]]) = ViewSSeq(source)(iso)
    override def toString = s"ViewSSeq[${innerIso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewSSeq[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
      case _ => false
    }
  }
  implicit def SeqElement[A:Elem:WeakTypeTag]: Elem[Seq[A]] = new ExpBaseElemEx[Seq[A], SSeq[A]](element[SSeq[A]])(weakTypeTag[Seq[A]], DefaultOfSeq[A])
  case class ExpSSeqImpl[A]
      (override val wrappedValueOfBaseType: Rep[Seq[A]])
      (implicit eA: Elem[A])
    extends SSeqImpl[A](wrappedValueOfBaseType) with UserTypeDef[SSeq[A], SSeqImpl[A]] {
    lazy val selfType = element[SSeqImpl[A]].asInstanceOf[Elem[SSeq[A]]]
    override def mirror(t: Transformer) = ExpSSeqImpl[A](t(wrappedValueOfBaseType))
  }

  lazy val SSeqImpl: Rep[SSeqImplCompanionAbs] = new SSeqImplCompanionAbs with UserTypeDef[SSeqImplCompanionAbs, SSeqImplCompanionAbs] {
    lazy val selfType = element[SSeqImplCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object SSeqImplMethods {
  }

  def mkSSeqImpl[A]
    (wrappedValueOfBaseType: Rep[Seq[A]])(implicit eA: Elem[A]) =
    new ExpSSeqImpl[A](wrappedValueOfBaseType)
  def unmkSSeqImpl[A:Elem](p: Rep[SSeqImpl[A]]) =
    Some((p.wrappedValueOfBaseType))

  object SSeqMethods {
    object wrappedValueOfBaseType {
      def unapply(d: Def[_]): Option[Rep[SSeq[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SSeqElem[_, _, _]] && method.getName == "wrappedValueOfBaseType" =>
          Some(receiver).asInstanceOf[Option[Rep[SSeq[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SSeq[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object size {
      def unapply(d: Def[_]): Option[Rep[SSeq[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SSeqElem[_, _, _]] && method.getName == "size" =>
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
        case MethodCall(receiver, method, Seq(idx, _*), _) if receiver.elem.isInstanceOf[SSeqElem[_, _, _]] && method.getName == "apply" =>
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
        case MethodCall(receiver, method, Seq(unc_from, unc_until, _*), _) if receiver.elem.isInstanceOf[SSeqElem[_, _, _]] && method.getName == "slice" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SSeqElem[_, _, _]] && method.getName == "isEmpty" =>
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
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[SSeqElem[_, _, _]] && method.getName == "map" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[SSeq[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SSeq[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[SSeq[A]], Rep[((A,A)) => A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(op, _*), _) if receiver.elem.isInstanceOf[SSeqElem[_, _, _]] && method.getName == "reduce" =>
          Some((receiver, op)).asInstanceOf[Option[(Rep[SSeq[A]], Rep[((A,A)) => A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SSeq[A]], Rep[((A,A)) => A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filter {
      def unapply(d: Def[_]): Option[(Rep[SSeq[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(p, _*), _) if receiver.elem.isInstanceOf[SSeqElem[_, _, _]] && method.getName == "filter" =>
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
        case MethodCall(receiver, method, Seq(elem, _*), _) if receiver.elem.isInstanceOf[SSeqElem[_, _, _]] && method.getName == "$plus$colon" =>
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
        case MethodCall(receiver, method, Seq(that, _*), _) if receiver.elem.isInstanceOf[SSeqElem[_, _, _]] && method.getName == "diff" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SSeqElem[_, _, _]] && method.getName == "toArray" =>
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
        case MethodCall(receiver, method, Seq(arr, _*), _) if receiver.elem.isInstanceOf[SSeqCompanionElem] && method.getName == "apply" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SSeqCompanionElem] && method.getName == "empty" =>
          Some(()).asInstanceOf[Option[Unit forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromList {
      def unapply(d: Def[_]): Option[Rep[List[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(list, _*), _) if receiver.elem.isInstanceOf[SSeqCompanionElem] && method.getName == "fromList" =>
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
        case e: SSeqElem[a,from,to] => e.eItem match {
          case UnpackableElem(iso) => Some(iso)
          case _ => None
        }
        case _ => None
      }
    }
  }

  override def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
    case Def(view: ViewSSeq[_, _]) =>
      Some((view.source, SSeqIso(view.iso)))
    case UserTypeSSeq(iso: Iso[a, b]) =>
      val newIso = SSeqIso(iso)
      val repr = reifyObject(UnpackView(s.asRep[SSeq[b]])(newIso))
      Some((repr, newIso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  type SSeqMapArgs[A,B] = (Rep[SSeq[A]], Rep[A => B])

  override def rewriteDef[T](d: Def[T]) = d match {
    case SSeqMethods.map(xs, Def(l: Lambda[_, _])) if l.isIdentity => xs
    case SSeqMethods.map(t: SSeqMapArgs[_,c] @unchecked) => t match {
      case (xs: RSeq[a]@unchecked, f @ Def(Lambda(_, _, _, UnpackableExp(_, iso: Iso[b, c])))) => {
        val f1 = f.asRep[a => c]
        implicit val eA = xs.elem.eItem
        implicit val eB = iso.eFrom
        val s = xs.map( fun { x =>
          val tmp = f1(x)
          iso.from(tmp)
        })
        val res = ViewSSeq(s)(SSeqIso(iso))
        res
      }
      case (Def(view: ViewSSeq[a, b]), _) => {
        val iso = view.innerIso
        val ff = t._2.asRep[b => c]
        implicit val eA = iso.eFrom
        implicit val eB = iso.eTo
        implicit val eC = ff.elem.eRange
        view.source.map(fun { x => ff(iso.to(x))})
      }
      case _ =>
        super.rewriteDef(d)
    }
    case view1@ViewSSeq(Def(view2@ViewSSeq(arr))) => {
      val compIso = composeIso(view2.innerIso, view1.innerIso)
      implicit val eAB = compIso.eTo
      ViewSSeq(arr)(SSeqIso(compIso))
    }

    case _ => super.rewriteDef(d)
  }
}
