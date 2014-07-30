package scalan

import scalan.common.Default
import scala.language.higherKinds
import scalan.common.Lazy
import scala.reflect.runtime.universe._
import scalan.staged.BaseExp

trait Views extends Elems { self: Scalan =>

  // eFrom0 is used to avoid making eFrom implicit in subtypes
  // and support recursive types
  abstract class Iso[From, To](implicit eFrom0: Elem[From]) {
    def eFrom: Elem[From] = eFrom0
    def eTo: Elem[To]
    def tag: TypeTag[To]
    def defaultRepTo: Default[Rep[To]]
    def from(p: Rep[To]): Rep[From]
    def to(p: Rep[From]): Rep[To]
    override def toString = s"${eFrom.name} <-> ${eTo.name}"
    override def equals(other: Any) = other match {
      case i: Iso[_, _] => eFrom == i.eFrom && eTo == i.eTo
      case _ => false
    }
  }

  implicit def viewElement[From, To /*<: UserType[_]*/](implicit iso: Iso[From, To]): Elem[To] = iso.eTo // always ask elem from Iso

  abstract class ViewElem[From, To](implicit val iso: Iso[From, To]) extends Elem[To] {
    lazy val tag: TypeTag[To] = iso.tag
    lazy val defaultRep = iso.defaultRepTo    
  }

  trait CompanionElem[T] extends Elem[T] {
  }

  trait UserType[T] {
    def selfType: Elem[T]
    def self: Rep[T]
  }

  trait TypeFamily1[F[_]] {
    def defaultOf[A](implicit ea: Elem[A]): Default[Rep[F[A]]]
  }
  trait TypeFamily2[F[_, _]] {
    def defaultOf[A, B](implicit ea: Elem[A], eb: Elem[B]): Default[Rep[F[A, B]]]
  }

  trait ConcreteClass0[C] {
    def defaultOf: Default[Rep[C]]
  }
  trait ConcreteClass1[C[_]] {
    def defaultOf[A](implicit ea: Elem[A]): Default[Rep[C[A]]]
  }
  trait ConcreteClass2[C[_, _]] {
    def defaultOf[A, B](implicit ea: Elem[A], eb: Elem[B]): Default[Rep[C[A, B]]]
  }
  trait ConcreteClass3[T[_, _, _]] {
    def defaultOf[A, B, C](implicit ea: Elem[A], eb: Elem[B], ec: Elem[C]): Default[Rep[T[A, B, C]]]
  }
  trait ConcreteClass4[T[_, _, _, _]] {
    def defaultOf[A, B, C, D](implicit ea: Elem[A], eb: Elem[B], ec: Elem[C], ed : Elem[D]): Default[Rep[T[A, B, C, D]]]
  }

  def identityIso[A](implicit elem: Elem[A]): Iso[A, A] =
    new Iso[A, A] {
      def eTo = elem
      def tag = elem.tag
      def defaultRepTo = elem.defaultRep
      def from(x: Rep[A]) = x
      def to(x: Rep[A]) = x
    }

  def pairIso[A1, B1, A2, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2]): Iso[(A1, A2), (B1, B2)] = {
    implicit val eA1 = iso1.eFrom
    implicit val eA2 = iso2.eFrom
    implicit val eB1 = iso1.eTo
    implicit val eB2 = iso2.eTo
    val eBB = element[(B1, B2)]
    new Iso[(A1, A2), (B1, B2)] {
      def eTo = eBB
      def from(b: Rep[(B1, B2)]) = (iso1.from(b._1), iso2.from(b._2))
      def to(a: Rep[(A1, A2)]) = (iso1.to(a._1), iso2.to(a._2))
      def tag = eBB.tag
      def defaultRepTo = eBB.defaultRep
    }
  }

  //TODO ICFP implement sumIso

  def composeIso[A, B, C](iso2: Iso[B, C], iso1: Iso[A, B]): Iso[A, C] = {
    new Iso[A, C]()(iso1.eFrom) {
      def eTo = iso2.eTo
      def from(c: Rep[C]) = iso1.from(iso2.from(c))
      def to(a: Rep[A]) = iso2.to(iso1.to(a))
      def tag = iso2.tag
      def defaultRepTo = iso2.defaultRepTo
    }
  }

  def funcIso[A, B, C, D](iso1: Iso[A, B], iso2: Iso[C, D]): Iso[A => C, B => D] = {
    implicit val eA = iso1.eFrom
    implicit val eB = iso1.eTo
    implicit val eC = iso2.eFrom
    implicit val eD = iso2.eTo
    new Iso[A => C, B => D] {
      lazy val eTo = funcElement(eB, eD)
      def from(f: Rep[B => D]): Rep[A => C] = {
        fun { b => iso2.from(f(iso1.to(b))) }
      }
      def to(f: Rep[A => C]): Rep[B => D] = {
        fun { a => iso2.to(f(iso1.from(a))) }
      }
      def tag = eTo.tag
      def defaultRepTo = eTo.defaultRep
    }
  }
}

trait ViewsSeq extends Views { self: ScalanSeq =>
  class SeqViewElem[From, To](implicit iso: Iso[From, To]) extends ViewElem[From, To]

  trait UserTypeSeq[T, TImpl <: T] extends UserType[T] { thisType: T =>
    override def self = this
  }
}

trait ViewsExp extends Views with BaseExp { self: ScalanStaged =>
  class StagedViewElem[From, To](implicit iso: Iso[From, To]) extends ViewElem[From, To]

  trait UserTypeDef[T, TImpl <: T] extends ReifiableObject[T, TImpl] {
    override def self = reifyObject(this)
    def uniqueOpId = selfType.name
  }
  object UserTypeDef {
    def unapply[T](d: Def[T]): Option[Iso[_, T]] = {
      val eT = d.selfType
      eT match {
        case e: ViewElem[_, _] => Some(e.iso)
        case _ => None
      }
    }
  }
  object UserTypeSym {
    def unapply[T](s: Exp[T]): Option[Iso[_, T]] = {
      val eT = s.elem
      eT match {
        case e: ViewElem[_, _] => Some(e.iso)
        case _ => None
      }
    }
  }

  def hasViews(s: Exp[_]): Boolean = s match {
    case Def(Tup(s1, s2)) => hasViews(s1) || hasViews(s2)
    case UserTypeSym(_) => true
    case Def(UserTypeDef(_)) => true
    case _ => false
  }

  def eliminateViews(s: Exp[_]): (Exp[_], Iso[_, _]) = s match {
    case Def(Tup(s1, s2)) =>
      val (sv1, iso1) = eliminateViews(s1)
      val (sv2, iso2) = eliminateViews(s2)
      ((sv1, sv2), pairIso(iso1, iso2))
    case Def(UserTypeDef(iso: Iso[a, b])) =>
      val repr = iso.from(s.asRep[b])
      (repr, iso)
    case UserTypeSym(iso: Iso[a, b]) =>
      val repr = iso.from(s.asRep[b])
      (repr, iso)
    case s =>
      (s, identityIso(s.elem))
  }

  implicit class IsoOps[From, To](iso: Iso[From, To]) {
    def toFunTo: Rep[From => To] = fun(iso.to _)(Lazy(iso.eFrom))
    def toFunFrom: Rep[To => From] = fun(iso.from _)(Lazy(iso.eTo))
  }

  def MethodCallFromExp(clazzUT: Class[_], methodName: String) = new {
    def unapply[T](d: Def[T]): Option[(Exp[_], List[Exp[_]])] = {
      d match {
        case MethodCall(obj, m, Exps(args)) =>
          m.getName == methodName match {
            case true => Some((obj, args))
            case _ => None
          }
        case _ => None
      }
    }
  }

  object UnpackableDef {
    def unapply[T](d: Def[T]): Option[(Rep[Source], Iso[Source, T]) forSome { type Source }] =
      d match {
        case view: View[a, T] => Some((view.source, view.iso))
        // TODO make UserTypeDef extend View with lazy iso/source?
        case UserTypeDef(iso: Iso[a, T]) => Some((iso.from(d.self), iso))
        case _ => None
      }
  }

  object UnpackableExp {
    def unapply[T](e: Exp[T]): Option[(Rep[Source], Iso[Source, T]) forSome { type Source }] =
      e match {
        case Def(d) => d match {
          case view: View[a, T] => Some((view.source, view.iso))
          case UserTypeDef(iso: Iso[a, T]) => Some((iso.from(e), iso))
          case _ => None
        }
        case _ => e.elem match {
          case v: ViewElem[a, T] @unchecked => Some((v.iso.from(e), v.iso))
          case _ => None
        }
      }
  }

  abstract class View[From, To] extends Def[To] {
    def source: Rep[From]
    def iso: Iso[From, To]
    implicit def selfType = iso.eTo
    lazy val self: Rep[To] = this
    def copy(source: Rep[From]): View[From, To]
    def mirror(t: Transformer) = copy(t(source))
    lazy val uniqueOpId = name(iso.eFrom, iso.eTo)
  }

  case class UnpackView[A, B](view: Rep[B])(implicit iso: Iso[A, B]) extends Def[A] {
    implicit def selfType = iso.eFrom
    lazy val self: Rep[A] = this
    override def mirror(f: Transformer) = UnpackView[A, B](f(view))
    lazy val uniqueOpId = name(selfType, view.elem)
  }

  abstract class View1[A, B, C[_]](implicit val innerIso: Iso[A, B]) extends View[C[A], C[B]]

  abstract class View2[A1, A2, B1, B2, C[_, _]](implicit val iso1: Iso[A1, B1], val iso2: Iso[A2, B2]) extends View[C[A1, A2], C[B1, B2]]

  //  type Identity[T] = T
  //
  //  case class ViewVar[A, B](source: Rep[A])(implicit innerIso: Iso[A, B]) extends View1[A, B, Identity] {
  //    def iso = innerIso
  //    def copy(source: Rep[A]) = ViewVar(source)
  //  }

  case class ViewPair[A1, A2, B1, B2](source: Rep[(A1, A2)])(implicit iso1: Iso[A1, B1], iso2: Iso[A2, B2]) extends View2[A1, A2, B1, B2, Tuple2] {
    lazy val iso = pairIso(iso1, iso2)
    def copy(source: Rep[(A1, A2)]) = ViewPair(source)
  }

  //TODO ICFP implement ViewSum and corresponding rewrite rules

  override def rewrite[T](d: Exp[T])(implicit eT: LElem[T]) = d match {
    case Def(d1) => d1 match {
      //      case ViewPair(Def(ViewPair(a, iso1)), iso2) =>
      //        ViewPair(a, composeIso(iso2, iso1))
      case Tup(Def(UnpackableDef(a, iso1: Iso[a, c])), Def(UnpackableDef(b, iso2: Iso[b, d]))) =>
        ViewPair((a.asRep[a], b.asRep[b]))(iso1, iso2)
      case Tup(Def(UnpackableDef(a, iso1: Iso[a, c])), b: Rep[b]) =>
        ViewPair((a.asRep[a], b))(iso1, identityIso(b.elem)).self
      case Tup(a: Rep[a], Def(UnpackableDef(b, iso2: Iso[b, d]))) =>
        ViewPair((a, b.asRep[b]))(identityIso(a.elem), iso2).self
      case First(Def(view @ ViewPair(source))) =>
        view.iso1.to(source._1)
      case Second(Def(view @ ViewPair(source))) =>
        view.iso2.to(source._2)
      // case UnpackableDef(Def(uv @ UnpackView(view)), iso) if iso.eTo == view.iso.eTo => view
      case UnpackView(Def(UnpackableDef(source, iso))) => source
      // case UnpackView(view @ UnpackableExp(iso)) => iso.from(view)
      //      case LoopUntil(start, step, isMatch) if hasViews(start) => {
      //        eliminateViews(start) match {
      //          case (startWithoutViews, iso: Iso[a, b]) =>
      //            val start1 = startWithoutViews.asRep[a]
      //            implicit val eA = iso.eFrom
      //            implicit val eB = iso.eTo
      //            val step1 = fun { (x: Rep[a]) =>
      //              val x_viewed = iso.to(x)
      //              val res_viewed = mirrorApply(step.asRep[b => b], x_viewed)
      //              val res = iso.from(res_viewed)
      //              res
      //            }(eA, eA)
      //            val isMatch1 = fun { (x: Rep[a]) =>
      //              val x_viewed = iso.to(x)
      //              val res = mirrorApply(isMatch.asRep[b => Boolean], x_viewed)
      //              res
      //            }(eA, element[Boolean])
      //            val loopRes = LoopUntil(start1, step1, isMatch1)
      //            iso.to(loopRes)
      //        }
      //      }
      case _ => super.rewrite(d)
    }
    case Var(UserTypeSym(iso: Iso[a, _])) =>
      iso.to(fresh[a](Lazy(iso.eFrom)))
    case _ => super.rewrite(d)
  }
}
