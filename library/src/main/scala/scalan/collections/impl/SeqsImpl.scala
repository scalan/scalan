package scalan.collections

import scala.collection.Seq
import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait SeqsAbs extends Seqs with scalan.Scalan {
  self: ScalanCommunityDsl =>

  // single proxy for each type family
  implicit def proxySSeq[A](p: Rep[SSeq[A]]): SSeq[A] = {
    proxyOps[SSeq[A]](p)(scala.reflect.classTag[SSeq[A]])
  }

  // TypeWrapper proxy
  //implicit def proxySeq[A:Elem](p: Rep[Seq[A]]): SSeq[A] =
  //  proxyOps[SSeq[A]](p.asRep[SSeq[A]])

  implicit def unwrapValueOfSSeq[A](w: Rep[SSeq[A]]): Rep[Seq[A]] = w.wrappedValueOfBaseType

  implicit def seqElement[A:Elem]: Elem[Seq[A]]

  implicit def castSSeqElement[A](elem: Elem[SSeq[A]]): SSeqElem[A, SSeq[A]] = elem.asInstanceOf[SSeqElem[A, SSeq[A]]]

  implicit val containerSeq: Cont[Seq] = new Container[Seq] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[Seq[A]]
    def lift[A](implicit evA: Elem[A]) = element[Seq[A]]
  }

  implicit val containerSSeq: Cont[SSeq] with Functor[SSeq] = new Container[SSeq] with Functor[SSeq] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[SSeq[A]]
    def lift[A](implicit evA: Elem[A]) = element[SSeq[A]]
    def map[A:Elem,B:Elem](xs: Rep[SSeq[A]])(f: Rep[A] => Rep[B]) = xs.map(fun(f))
  }
  case class SSeqIso[A,B](iso: Iso[A,B]) extends Iso1[A, B, SSeq](iso) {
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    def from(x: Rep[SSeq[B]]) = x.map(iso.from _)
    def to(x: Rep[SSeq[A]]) = x.map(iso.to _)
    lazy val defaultRepTo = SSeq.empty[B]
  }

  // familyElem
  abstract class SSeqElem[A, To <: SSeq[A]](implicit val eA: Elem[A])
    extends WrapperElem1[A, To, Seq, SSeq]()(eA, container[Seq], container[SSeq]) {
    lazy val parent: Option[Elem[_]] = None
    lazy val entityDef: STraitOrClassDef = {
      val module = getModules("Seqs")
      module.entities.find(_.name == "SSeq").get
    }
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[SSeq[A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[SSeq[A]] => convertSSeq(x) }
      tryConvert(element[SSeq[A]], this, x, conv)
    }

    def convertSSeq(x : Rep[SSeq[A]]): Rep[To] = {
      assert(x.selfType1 match { case _: SSeqElem[_, _] => true; case _ => false })
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sSeqElement[A](implicit eA: Elem[A]): Elem[SSeq[A]] =
    new SSeqElem[A, SSeq[A]] {
      lazy val eTo = element[SSeqImpl[A]]
    }

  implicit case object SSeqCompanionElem extends CompanionElem[SSeqCompanionAbs] {
    lazy val tag = weakTypeTag[SSeqCompanionAbs]
    protected def getDefaultRep = SSeq
  }

  abstract class SSeqCompanionAbs extends CompanionBase[SSeqCompanionAbs] with SSeqCompanion {
    override def toString = "SSeq"
  }
  def SSeq: Rep[SSeqCompanionAbs]
  implicit def proxySSeqCompanion(p: Rep[SSeqCompanion]): SSeqCompanion =
    proxyOps[SSeqCompanion](p)

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
    override lazy val entityDef = {
      val module = getModules("Seqs")
      module.concreteSClasses.find(_.name == "SSeqImpl").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA))
    }
    lazy val eTo = this
    override def convertSSeq(x: Rep[SSeq[A]]) = SSeqImpl(x.wrappedValueOfBaseType)
    override def getDefaultRep = super[ConcreteElem1].getDefaultRep
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[SSeqImpl[A]]
    }
  }

  // state representation type
  type SSeqImplData[A] = Seq[A]

  // 3) Iso for concrete class
  class SSeqImplIso[A](implicit eA: Elem[A])
    extends Iso[SSeqImplData[A], SSeqImpl[A]] {
    override def from(p: Rep[SSeqImpl[A]]) =
      p.wrappedValueOfBaseType
    override def to(p: Rep[Seq[A]]) = {
      val wrappedValueOfBaseType = p
      SSeqImpl(wrappedValueOfBaseType)
    }
    lazy val defaultRepTo: Rep[SSeqImpl[A]] = SSeqImpl(DefaultOfSeq[A].value)
    lazy val eTo = new SSeqImplElem[A](this)
  }
  // 4) constructor and deconstructor
  abstract class SSeqImplCompanionAbs extends CompanionBase[SSeqImplCompanionAbs] {
    override def toString = "SSeqImpl"

    def apply[A](wrappedValueOfBaseType: Rep[Seq[A]])(implicit eA: Elem[A]): Rep[SSeqImpl[A]] =
      mkSSeqImpl(wrappedValueOfBaseType)
  }
  object SSeqImplMatcher {
    def unapply[A](p: Rep[SSeq[A]]) = unmkSSeqImpl(p)
  }
  def SSeqImpl: Rep[SSeqImplCompanionAbs]
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
    new SSeqImplIso[A]

  // 6) smart constructor and deconstructor
  def mkSSeqImpl[A](wrappedValueOfBaseType: Rep[Seq[A]])(implicit eA: Elem[A]): Rep[SSeqImpl[A]]
  def unmkSSeqImpl[A](p: Rep[SSeq[A]]): Option[(Rep[Seq[A]])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(Seqs_Module.dump))
}

// Seq -----------------------------------
trait SeqsSeq extends SeqsDsl with scalan.ScalanSeq {
  self: ScalanCommunityDslSeq =>
  lazy val SSeq: Rep[SSeqCompanionAbs] = new SSeqCompanionAbs with UserTypeSeq[SSeqCompanionAbs] {
    lazy val selfType = element[SSeqCompanionAbs]
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
  //  proxyOpsEx[Seq[A],SSeq[A], SeqSSeqImpl[A]](p, bt => SeqSSeqImpl(bt))

    implicit def seqElement[A:Elem]: Elem[Seq[A]] =
      new SeqBaseElemEx1[A, SSeq[A], Seq](
           element[SSeq[A]])(element[A], container[Seq], DefaultOfSeq[A])

  case class SeqSSeqImpl[A]
      (override val wrappedValueOfBaseType: Rep[Seq[A]])
      (implicit eA: Elem[A])
    extends SSeqImpl[A](wrappedValueOfBaseType)
       with SeqSSeq[A] with UserTypeSeq[SSeqImpl[A]] {
    lazy val selfType = element[SSeqImpl[A]]
    override def size: Rep[Int] =
      wrappedValueOfBaseType.size

    override def apply(idx: Rep[Int]): Rep[A] =
      wrappedValueOfBaseType.apply(idx)

    override def slice(unc_from: Rep[Int], unc_until: Rep[Int]): Rep[SSeq[A]] =
      SSeqImpl(wrappedValueOfBaseType.slice(unc_from, unc_until))

    override def isEmpty: Rep[Boolean] =
      wrappedValueOfBaseType.isEmpty

    override def reduce(op: Rep[((A, A)) => A]): Rep[A] =
      wrappedValueOfBaseType.reduce(scala.Function.untupled(op))

    override def filter(p: Rep[A => Boolean]): Rep[SSeq[A]] =
      SSeqImpl(wrappedValueOfBaseType.filter(p))

    override def $plus$colon(elem: Rep[A]): Rep[SSeq[A]] =
      SSeqImpl(wrappedValueOfBaseType.$plus$colon(elem))

    override def diff(that: Rep[SSeq[A]]): Rep[SSeq[A]] =
      SSeqImpl(wrappedValueOfBaseType.diff(that))

    override def toList: Rep[List[A]] =
      wrappedValueOfBaseType.toList
  }
  lazy val SSeqImpl = new SSeqImplCompanionAbs with UserTypeSeq[SSeqImplCompanionAbs] {
    lazy val selfType = element[SSeqImplCompanionAbs]
  }

  def mkSSeqImpl[A]
      (wrappedValueOfBaseType: Rep[Seq[A]])(implicit eA: Elem[A]): Rep[SSeqImpl[A]] =
      new SeqSSeqImpl[A](wrappedValueOfBaseType)
  def unmkSSeqImpl[A](p: Rep[SSeq[A]]) = p match {
    case p: SSeqImpl[A] @unchecked =>
      Some((p.wrappedValueOfBaseType))
    case _ => None
  }

  implicit def wrapSeqToSSeq[A:Elem](v: Seq[A]): SSeq[A] = SSeqImpl(v)
}

// Exp -----------------------------------
trait SeqsExp extends SeqsDsl with scalan.ScalanExp {
  self: ScalanCommunityDslExp =>
  lazy val SSeq: Rep[SSeqCompanionAbs] = new SSeqCompanionAbs with UserTypeDef[SSeqCompanionAbs] {
    lazy val selfType = element[SSeqCompanionAbs]
    override def mirror(t: Transformer) = this

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

  case class ViewSSeq[A, B](source: Rep[SSeq[A]])(iso: Iso1[A, B, SSeq])
    extends View1[A, B, SSeq](iso) {
    def copy(source: Rep[SSeq[A]]) = ViewSSeq(source)(iso)
    override def toString = s"ViewSSeq[${innerIso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewSSeq[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
      case _ => false
    }
  }

  implicit def seqElement[A:Elem]: Elem[Seq[A]] =
      new ExpBaseElemEx1[A, SSeq[A], Seq](
           element[SSeq[A]])(element[A], container[Seq], DefaultOfSeq[A])

  case class ExpSSeqImpl[A]
      (override val wrappedValueOfBaseType: Rep[Seq[A]])
      (implicit eA: Elem[A])
    extends SSeqImpl[A](wrappedValueOfBaseType) with UserTypeDef[SSeqImpl[A]] {
    lazy val selfType = element[SSeqImpl[A]]
    override def mirror(t: Transformer) = ExpSSeqImpl[A](t(wrappedValueOfBaseType))
  }

  lazy val SSeqImpl: Rep[SSeqImplCompanionAbs] = new SSeqImplCompanionAbs with UserTypeDef[SSeqImplCompanionAbs] {
    lazy val selfType = element[SSeqImplCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object SSeqImplMethods {
  }

  def mkSSeqImpl[A]
    (wrappedValueOfBaseType: Rep[Seq[A]])(implicit eA: Elem[A]): Rep[SSeqImpl[A]] =
    new ExpSSeqImpl[A](wrappedValueOfBaseType)
  def unmkSSeqImpl[A](p: Rep[SSeq[A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SSeqImplElem[A] @unchecked =>
      Some((p.asRep[SSeqImpl[A]].wrappedValueOfBaseType))
    case _ =>
      None
  }

  object SSeqMethods {
    object wrappedValueOfBaseType {
      def unapply(d: Def[_]): Option[Rep[SSeq[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SSeqElem[_, _]] && method.getName == "wrappedValueOfBaseType" =>
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
      val newIso = SSeqIso(iso)
      val repr = reifyObject(UnpackView(s.asRep[SSeq[b]])(newIso))
      Some((repr, newIso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  override def rewriteDef[T](d: Def[T]) = d match {
    case SSeqMethods.map(xs, Def(IdentityLambda())) => xs

    case view1@ViewSSeq(Def(view2@ViewSSeq(arr))) =>
      val compIso = composeIso(view1.innerIso, view2.innerIso)
      implicit val eAB = compIso.eTo
      ViewSSeq(arr)(SSeqIso(compIso))

    // Rule: W(a).m(args) ==> iso.to(a.m(unwrap(args)))
    case mc @ MethodCall(Def(wrapper: ExpSSeqImpl[_]), m, args, neverInvoke) if !isValueAccessor(m) =>
      val resultElem = mc.selfType
      val wrapperIso = getIsoByElem(resultElem)
      wrapperIso match {
        case iso: Iso[base,ext] =>
          val eRes = iso.eFrom
          val newCall = unwrapMethodCall(mc, wrapper.wrappedValueOfBaseType, eRes)
          iso.to(newCall)
      }

    case SSeqMethods.map(xs, f) => (xs, f) match {
      case (xs: RSeq[a] @unchecked, LambdaResultHasViews(f, iso: Iso[b, c])) =>
        val f1 = f.asRep[a => c]
        implicit val eA = xs.elem.eItem
        implicit val eB = iso.eFrom
        val s = xs.map(fun { x =>
          val tmp = f1(x)
          iso.from(tmp)
        })
        val res = ViewSSeq(s)(SSeqIso(iso))
        res
      case (HasViews(source, contIso: SSeqIso[a, b]), f: Rep[Function1[_, c] @unchecked]) =>
        val f1 = f.asRep[b => c]
        val iso = contIso.iso
        implicit val eA = iso.eFrom
        implicit val eB = iso.eTo
        implicit val eC = f1.elem.eRange
        source.asRep[SSeq[a]].map(fun { x => f1(iso.to(x)) })
      case _ =>
        super.rewriteDef(d)
    }

    case _ => super.rewriteDef(d)
  }
}

object Seqs_Module {
  val packageName = "scalan.collections"
  val name = "Seqs"
  val dump = "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"
}
}

trait SeqsDsl extends impl.SeqsAbs {self: ScalanCommunityDsl =>}
