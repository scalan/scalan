package scalan.collections
package impl

import scala.collection.immutable.HashSet
import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe._
import scala.reflect._
import scalan.common.Default

// Abs -----------------------------------
trait HashSetsAbs extends Scalan with HashSets {
  self: ScalanCommunityDsl =>
  // single proxy for each type family
  implicit def proxySHashSet[A](p: Rep[SHashSet[A]]): SHashSet[A] = {
    implicit val tag = weakTypeTag[SHashSet[A]]
    proxyOps[SHashSet[A]](p)(TagImplicits.typeTagToClassTag[SHashSet[A]])
  }
  // BaseTypeEx proxy
  //implicit def proxyHashSet[A:Elem](p: Rep[HashSet[A]]): SHashSet[A] =
  //  proxyOps[SHashSet[A]](p.asRep[SHashSet[A]])

  implicit def unwrapValueOfSHashSet[A](w: Rep[SHashSet[A]]): Rep[HashSet[A]] = w.wrappedValueOfBaseType

  implicit def defaultSHashSetElem[A:Elem]: Elem[SHashSet[A]] = element[SHashSetImpl[A]].asElem[SHashSet[A]]
  implicit def HashSetElement[A:Elem:WeakTypeTag]: Elem[HashSet[A]]

  implicit def castSHashSetElement[A](elem: Elem[SHashSet[A]]): SHashSetElem[A, _,SHashSet[A]] = elem.asInstanceOf[SHashSetElem[A, _,SHashSet[A]]]
  implicit val containerSHashSet: Cont[SHashSet] = new Container[SHashSet] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[SHashSet[A]]
    def lift[A](implicit evA: Elem[A]) = element[SHashSet[A]]
  }
  case class SHashSetIso[A,B](iso: Iso[A,B]) extends Iso1[A, B, SHashSet](iso) {
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    def from(x: Rep[SHashSet[B]]) = x.map(iso.from _)
    def to(x: Rep[SHashSet[A]]) = x.map(iso.to _)
    lazy val defaultRepTo = Default.defaultVal(SHashSet.empty[B])
  }
  abstract class SHashSetElem[A, From, To <: SHashSet[A]](iso: Iso[From, To])(implicit eA: Elem[A])
    extends ViewElem1[A, From, To, SHashSet](iso) {
    override def convert(x: Rep[Reifiable[_]]) = convertSHashSet(x.asRep[SHashSet[A]])
    def convertSHashSet(x : Rep[SHashSet[A]]): Rep[To]
  }
  trait SHashSetCompanionElem extends CompanionElem[SHashSetCompanionAbs]
  implicit lazy val SHashSetCompanionElem: SHashSetCompanionElem = new SHashSetCompanionElem {
    lazy val tag = weakTypeTag[SHashSetCompanionAbs]
    protected def getDefaultRep = SHashSet
  }

  abstract class SHashSetCompanionAbs extends CompanionBase[SHashSetCompanionAbs] with SHashSetCompanion {
    override def toString = "SHashSet"

    def empty[A:Elem]: Rep[SHashSet[A]] =
      methodCallEx[SHashSet[A]](self,
        this.getClass.getMethod("empty", classOf[Elem[A]]),
        List(element[A]))
  }
  def SHashSet: Rep[SHashSetCompanionAbs]
  implicit def proxySHashSetCompanion(p: Rep[SHashSetCompanion]): SHashSetCompanion = {
    proxyOps[SHashSetCompanion](p)
  }

  // default wrapper implementation
  abstract class SHashSetImpl[A](val wrappedValueOfBaseType: Rep[HashSet[A]])(implicit val eA: Elem[A]) extends SHashSet[A] {
    def $plus(elem: Rep[A]): Rep[SHashSet[A]] =
      methodCallEx[SHashSet[A]](self,
        this.getClass.getMethod("$plus", classOf[AnyRef]),
        List(elem.asInstanceOf[AnyRef]))

    def map[B:Elem](f: Rep[A => B]): Rep[SHashSet[B]] =
      methodCallEx[SHashSet[B]](self,
        this.getClass.getMethod("map", classOf[AnyRef], classOf[Elem[B]]),
        List(f.asInstanceOf[AnyRef], element[B]))

    def fold(z: Rep[A])(f: Rep[((A,A)) => A]): Rep[A] =
      methodCallEx[A](self,
        this.getClass.getMethod("fold", classOf[AnyRef], classOf[AnyRef]),
        List(z.asInstanceOf[AnyRef], f.asInstanceOf[AnyRef]))
  }
  trait SHashSetImplCompanion
  // elem for concrete class
  class SHashSetImplElem[A](iso: Iso[SHashSetImplData[A], SHashSetImpl[A]])(implicit val eA: Elem[A])
    extends SHashSetElem[A, SHashSetImplData[A], SHashSetImpl[A]](iso) {
    def convertSHashSet(x: Rep[SHashSet[A]]) = SHashSetImpl(x.wrappedValueOfBaseType)
  }

  // state representation type
  type SHashSetImplData[A] = HashSet[A]

  // 3) Iso for concrete class
  class SHashSetImplIso[A](implicit eA: Elem[A])
    extends Iso[SHashSetImplData[A], SHashSetImpl[A]] {
    override def from(p: Rep[SHashSetImpl[A]]) =
      unmkSHashSetImpl(p) match {
        case Some((wrappedValueOfBaseType)) => wrappedValueOfBaseType
        case None => !!!
      }
    override def to(p: Rep[HashSet[A]]) = {
      val wrappedValueOfBaseType = p
      SHashSetImpl(wrappedValueOfBaseType)
    }
    lazy val tag = {
      weakTypeTag[SHashSetImpl[A]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[SHashSetImpl[A]]](SHashSetImpl(DefaultOfHashSet[A].value))
    lazy val eTo = new SHashSetImplElem[A](this)
  }
  // 4) constructor and deconstructor
  abstract class SHashSetImplCompanionAbs extends CompanionBase[SHashSetImplCompanionAbs] with SHashSetImplCompanion {
    override def toString = "SHashSetImpl"

    def apply[A](wrappedValueOfBaseType: Rep[HashSet[A]])(implicit eA: Elem[A]): Rep[SHashSetImpl[A]] =
      mkSHashSetImpl(wrappedValueOfBaseType)
    def unapply[A:Elem](p: Rep[SHashSetImpl[A]]) = unmkSHashSetImpl(p)
  }
  def SHashSetImpl: Rep[SHashSetImplCompanionAbs]
  implicit def proxySHashSetImplCompanion(p: Rep[SHashSetImplCompanionAbs]): SHashSetImplCompanionAbs = {
    proxyOps[SHashSetImplCompanionAbs](p)
  }

  class SHashSetImplCompanionElem extends CompanionElem[SHashSetImplCompanionAbs] {
    lazy val tag = weakTypeTag[SHashSetImplCompanionAbs]
    protected def getDefaultRep = SHashSetImpl
  }
  implicit lazy val SHashSetImplCompanionElem: SHashSetImplCompanionElem = new SHashSetImplCompanionElem

  implicit def proxySHashSetImpl[A](p: Rep[SHashSetImpl[A]]): SHashSetImpl[A] =
    proxyOps[SHashSetImpl[A]](p)

  implicit class ExtendedSHashSetImpl[A](p: Rep[SHashSetImpl[A]])(implicit eA: Elem[A]) {
    def toData: Rep[SHashSetImplData[A]] = isoSHashSetImpl(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSHashSetImpl[A](implicit eA: Elem[A]): Iso[SHashSetImplData[A], SHashSetImpl[A]] =
    new SHashSetImplIso[A]

  // 6) smart constructor and deconstructor
  def mkSHashSetImpl[A](wrappedValueOfBaseType: Rep[HashSet[A]])(implicit eA: Elem[A]): Rep[SHashSetImpl[A]]
  def unmkSHashSetImpl[A:Elem](p: Rep[SHashSetImpl[A]]): Option[(Rep[HashSet[A]])]
}

// Seq -----------------------------------
trait HashSetsSeq extends HashSetsDsl with ScalanSeq {
  self: ScalanCommunityDslSeq =>
  lazy val SHashSet: Rep[SHashSetCompanionAbs] = new SHashSetCompanionAbs with UserTypeSeq[SHashSetCompanionAbs, SHashSetCompanionAbs] {
    lazy val selfType = element[SHashSetCompanionAbs]
    override def empty[A:Elem]: Rep[SHashSet[A]] =
      SHashSetImpl(HashSet.empty[A])
  }

    // override proxy if we deal with BaseTypeEx
  //override def proxyHashSet[A:Elem](p: Rep[HashSet[A]]): SHashSet[A] =
  //  proxyOpsEx[HashSet[A],SHashSet[A], SeqSHashSetImpl[A]](p, bt => SeqSHashSetImpl(bt))

    implicit def HashSetElement[A:Elem:WeakTypeTag]: Elem[HashSet[A]] = new SeqBaseElemEx[HashSet[A], SHashSet[A]](element[SHashSet[A]])(weakTypeTag[HashSet[A]], DefaultOfHashSet[A])

  case class SeqSHashSetImpl[A]
      (override val wrappedValueOfBaseType: Rep[HashSet[A]])
      (implicit eA: Elem[A])
    extends SHashSetImpl[A](wrappedValueOfBaseType)
       with SeqSHashSet[A] with UserTypeSeq[SHashSet[A], SHashSetImpl[A]] {
    lazy val selfType = element[SHashSetImpl[A]].asInstanceOf[Elem[SHashSet[A]]]
    override def $plus(elem: Rep[A]): Rep[SHashSet[A]] =
      SHashSetImpl(wrappedValueOfBaseType.$plus(elem))

    override def fold(z: Rep[A])(f: Rep[((A,A)) => A]): Rep[A] =
      wrappedValueOfBaseType.fold(z)(scala.Function.untupled(f))
  }
  lazy val SHashSetImpl = new SHashSetImplCompanionAbs with UserTypeSeq[SHashSetImplCompanionAbs, SHashSetImplCompanionAbs] {
    lazy val selfType = element[SHashSetImplCompanionAbs]
  }

  def mkSHashSetImpl[A]
      (wrappedValueOfBaseType: Rep[HashSet[A]])(implicit eA: Elem[A]): Rep[SHashSetImpl[A]] =
      new SeqSHashSetImpl[A](wrappedValueOfBaseType)
  def unmkSHashSetImpl[A:Elem](p: Rep[SHashSetImpl[A]]) =
    Some((p.wrappedValueOfBaseType))

  implicit def wrapHashSetToSHashSet[A:Elem](v: HashSet[A]): SHashSet[A] = SHashSetImpl(v)
}

// Exp -----------------------------------
trait HashSetsExp extends HashSetsDsl with ScalanExp {
  self: ScalanCommunityDslExp =>
  lazy val SHashSet: Rep[SHashSetCompanionAbs] = new SHashSetCompanionAbs with UserTypeDef[SHashSetCompanionAbs, SHashSetCompanionAbs] {
    lazy val selfType = element[SHashSetCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ViewSHashSet[A, B](source: Rep[SHashSet[A]])(iso: Iso1[A, B, SHashSet])
    extends View1[A, B, SHashSet](iso) {
    def copy(source: Rep[SHashSet[A]]) = ViewSHashSet(source)(iso)
    override def toString = s"ViewSHashSet[${innerIso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewSHashSet[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
      case _ => false
    }
  }
  implicit def HashSetElement[A:Elem:WeakTypeTag]: Elem[HashSet[A]] = new ExpBaseElemEx[HashSet[A], SHashSet[A]](element[SHashSet[A]])(weakTypeTag[HashSet[A]], DefaultOfHashSet[A])
  case class ExpSHashSetImpl[A]
      (override val wrappedValueOfBaseType: Rep[HashSet[A]])
      (implicit eA: Elem[A])
    extends SHashSetImpl[A](wrappedValueOfBaseType) with UserTypeDef[SHashSet[A], SHashSetImpl[A]] {
    lazy val selfType = element[SHashSetImpl[A]].asInstanceOf[Elem[SHashSet[A]]]
    override def mirror(t: Transformer) = ExpSHashSetImpl[A](t(wrappedValueOfBaseType))
  }

  lazy val SHashSetImpl: Rep[SHashSetImplCompanionAbs] = new SHashSetImplCompanionAbs with UserTypeDef[SHashSetImplCompanionAbs, SHashSetImplCompanionAbs] {
    lazy val selfType = element[SHashSetImplCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object SHashSetImplMethods {
  }

  def mkSHashSetImpl[A]
    (wrappedValueOfBaseType: Rep[HashSet[A]])(implicit eA: Elem[A]) =
    new ExpSHashSetImpl[A](wrappedValueOfBaseType)
  def unmkSHashSetImpl[A:Elem](p: Rep[SHashSetImpl[A]]) =
    Some((p.wrappedValueOfBaseType))

  object SHashSetMethods {
    object wrappedValueOfBaseType {
      def unapply(d: Def[_]): Option[Rep[SHashSet[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SHashSetElem[_, _, _]] && method.getName == "wrappedValueOfBaseType" =>
          Some(receiver).asInstanceOf[Option[Rep[SHashSet[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SHashSet[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object + {
      def unapply(d: Def[_]): Option[(Rep[SHashSet[A]], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(elem, _*), _) if receiver.elem.isInstanceOf[SHashSetElem[_, _, _]] && method.getName == "$plus" =>
          Some((receiver, elem)).asInstanceOf[Option[(Rep[SHashSet[A]], Rep[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SHashSet[A]], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object map {
      def unapply(d: Def[_]): Option[(Rep[SHashSet[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[SHashSetElem[_, _, _]] && method.getName == "map" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[SHashSet[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SHashSet[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fold {
      def unapply(d: Def[_]): Option[(Rep[SHashSet[A]], Rep[A], Rep[((A,A)) => A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(z, f, _*), _) if receiver.elem.isInstanceOf[SHashSetElem[_, _, _]] && method.getName == "fold" =>
          Some((receiver, z, f)).asInstanceOf[Option[(Rep[SHashSet[A]], Rep[A], Rep[((A,A)) => A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SHashSet[A]], Rep[A], Rep[((A,A)) => A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SHashSetCompanionMethods {
    object empty {
      def unapply(d: Def[_]): Option[Unit forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SHashSetCompanionElem] && method.getName == "empty" =>
          Some(()).asInstanceOf[Option[Unit forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object UserTypeSHashSet {
    def unapply(s: Exp[_]): Option[Iso[_, _]] = {
      s.elem match {
        case e: SHashSetElem[a,from,to] => e.eItem match {
          case UnpackableElem(iso) => Some(iso)
          case _ => None
        }
        case _ => None
      }
    }
  }

  override def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
    case Def(view: ViewSHashSet[_, _]) =>
      Some((view.source, view.iso))
    case UserTypeSHashSet(iso: Iso[a, b]) =>
      val newIso = SHashSetIso(iso)
      val repr = reifyObject(UnpackView(s.asRep[SHashSet[b]])(newIso))
      Some((repr, newIso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  type SHashSetMapArgs[A,B] = (Rep[SHashSet[A]], Rep[A => B])

  override def rewriteDef[T](d: Def[T]) = d match {
    case SHashSetMethods.map(xs, Def(l: Lambda[_, _])) if l.isIdentity => xs
    case SHashSetMethods.map(t: SHashSetMapArgs[_,c] @unchecked) => t match {
      case (xs: RHS[a]@unchecked, f @ Def(Lambda(_, _, _, UnpackableExp(_, iso: Iso[b, c])))) => {
        val f1 = f.asRep[a => c]
        implicit val eA = xs.elem.eItem
        implicit val eB = iso.eFrom
        val s = xs.map( fun { x =>
          val tmp = f1(x)
          iso.from(tmp)
        })
        val res = ViewSHashSet(s)(SHashSetIso(iso))
        res
      }
      case (Def(view: ViewSHashSet[a, b]), _) => {
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
    case view1@ViewSHashSet(Def(view2@ViewSHashSet(arr))) => {
      val compIso = composeIso(view2.innerIso, view1.innerIso)
      implicit val eAB = compIso.eTo
      ViewSHashSet(arr)(SHashSetIso(compIso))
    }

    case _ => super.rewriteDef(d)
  }
}
