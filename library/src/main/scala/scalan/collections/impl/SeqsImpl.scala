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

  implicit def unwrapValueOfSSeq[A](w: Rep[SSeq[A]]): Rep[Seq[A]] = w.wrappedValue

  implicit def seqElement[A:Elem]: Elem[Seq[A]] =
    element[SSeq[A]].asInstanceOf[WrapperElem1[_, _, CBase, CW] forSome { type CBase[_]; type CW[_] }].baseElem.asInstanceOf[Elem[Seq[A]]]

  implicit def castSSeqElement[A](elem: Elem[SSeq[A]]): SSeqElem[A, SSeq[A]] =
    elem.asInstanceOf[SSeqElem[A, SSeq[A]]]

  implicit lazy val containerSeq: Container[Seq] = new Container[Seq] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[Seq[A]]
    def lift[A](implicit evA: Elem[A]) = element[Seq[A]]
  }

  implicit lazy val containerSSeq: Container[SSeq] with Functor[SSeq] = new Container[SSeq] with Functor[SSeq] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[SSeq[A]]
    def lift[A](implicit evA: Elem[A]) = element[SSeq[A]]
    def map[A:Elem,B:Elem](xs: Rep[SSeq[A]])(f: Rep[A] => Rep[B]) = xs.map(fun(f))
  }

  case class SSeqIso[A, B](iso: Iso[A, B]) extends Iso1[A, B, SSeq](iso) {
    def from(x: Rep[SSeq[B]]) = x.map(iso.fromFun)
    def to(x: Rep[SSeq[A]]) = x.map(iso.toFun)
  }

  // familyElem
  class SSeqElem[A, To <: SSeq[A]](implicit _eA: Elem[A])
    extends WrapperElem1[A, To, Seq, SSeq](_eA, container[Seq], container[SSeq]) {
    def eA = _eA
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

    def convertSSeq(x: Rep[SSeq[A]]): Rep[To] = {
      x.selfType1 match {
        case _: SSeqElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have SSeqElem[_, _], but got $e")
      }
    }
    lazy val baseElem =
      new BaseTypeElem1[A, Seq, SSeq[A]](this.asInstanceOf[Element[SSeq[A]]])(
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

  abstract class SSeqCompanionAbs extends CompanionBase[SSeqCompanionAbs] with SSeqCompanion {
    override def toString = "SSeq"
  }
  def SSeq: Rep[SSeqCompanionAbs]
  implicit def proxySSeqCompanion(p: Rep[SSeqCompanion]): SSeqCompanion =
    proxyOps[SSeqCompanion](p)

  // default wrapper implementation
  abstract class SSeqImpl[A](val wrappedValue: Rep[Seq[A]])(implicit val eA: Elem[A]) extends SSeq[A] {
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
    override lazy val eTo: Elem[_] = this
    override def convertSSeq(x: Rep[SSeq[A]]) = SSeqImpl(x.wrappedValue)
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
    extends Iso[SSeqImplData[A], SSeqImpl[A]] {
    override def from(p: Rep[SSeqImpl[A]]) =
      p.wrappedValue
    override def to(p: Rep[Seq[A]]) = {
      val wrappedValue = p
      SSeqImpl(wrappedValue)
    }
    lazy val eTo = new SSeqImplElem[A](this)
  }
  // 4) constructor and deconstructor
  abstract class SSeqImplCompanionAbs extends CompanionBase[SSeqImplCompanionAbs] {
    override def toString = "SSeqImpl"

    def apply[A](wrappedValue: Rep[Seq[A]])(implicit eA: Elem[A]): Rep[SSeqImpl[A]] =
      mkSSeqImpl(wrappedValue)
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
    cachedIso[SSeqImplIso[A]](eA)

  // 6) smart constructor and deconstructor
  def mkSSeqImpl[A](wrappedValue: Rep[Seq[A]])(implicit eA: Elem[A]): Rep[SSeqImpl[A]]
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
  //  proxyOpsEx[Seq[A], SSeq[A], SeqSSeqImpl[A]](p, bt => SeqSSeqImpl(bt))

  case class SeqSSeqImpl[A]
      (override val wrappedValue: Rep[Seq[A]])
      (implicit eA: Elem[A])
    extends SSeqImpl[A](wrappedValue)
       with SeqSSeq[A] with UserTypeSeq[SSeqImpl[A]] {
    lazy val selfType = element[SSeqImpl[A]]
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
  lazy val SSeqImpl = new SSeqImplCompanionAbs with UserTypeSeq[SSeqImplCompanionAbs] {
    lazy val selfType = element[SSeqImplCompanionAbs]
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
trait SeqsExp extends SeqsDsl with scalan.ScalanExp {
  self: ScalanCommunityDslExp =>
  lazy val SSeq: Rep[SSeqCompanionAbs] = new SSeqCompanionAbs with UserTypeDef[SSeqCompanionAbs] {
    lazy val selfType = element[SSeqCompanionAbs]

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
    override def toString = s"ViewSSeq[${innerIso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewSSeq[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
      case _ => false
    }
  }

  case class ExpSSeqImpl[A]
      (override val wrappedValue: Rep[Seq[A]])
      (implicit eA: Elem[A])
    extends SSeqImpl[A](wrappedValue) with UserTypeDef[SSeqImpl[A]] {
    lazy val selfType = element[SSeqImpl[A]]
  }

  lazy val SSeqImpl: Rep[SSeqImplCompanionAbs] = new SSeqImplCompanionAbs with UserTypeDef[SSeqImplCompanionAbs] {
    lazy val selfType = element[SSeqImplCompanionAbs]
  }

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
    object wrappedValue {
      def unapply(d: Def[_]): Option[Rep[SSeq[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SSeqElem[_, _]] && method.getName == "wrappedValue" =>
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
          val newCall = unwrapMethodCall(mc, wrapper.wrappedValue, eRes)
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
