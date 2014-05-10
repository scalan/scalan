package scalan

import scala.text._
import Document._
import scalan.common.Default
import scala.language.higherKinds
import scalan.common.Lazy
import scala.reflect.runtime.universe._

trait Views extends Elems { self: Scalan =>

  // eFrom0 is used to avoid making eFrom implicit in subtypes
  // and support recursive types
  abstract class Iso[From,To](implicit eFrom0: Elem[From]) {
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
  
  implicit def viewElement[From, To <: UserType[_]](implicit iso: Iso[From,To]): Elem[To] = iso.eTo  // always ask elem from Iso

  abstract class ViewElem[From,To](val iso: Iso[From, To]) extends Elem[To]

  trait CompanionElem[T] extends Elem[T] {
  }

  trait UserType[T] {
    def selfType: Elem[T]
    def self: Rep[T]
  }

  trait TypeFamily1[F[_]] {
    def defaultOf[A](implicit ea: Elem[A]): Default[Rep[F[A]]]
  }
  trait TypeFamily2[F[_,_]] {
    def defaultOf[A,B](implicit ea: Elem[A], eb: Elem[B]): Default[Rep[F[A,B]]]
  }

  trait ConcreteClass1[C[_]] {
    def defaultOf[A](implicit ea: Elem[A]): Default[Rep[C[A]]]
  }
  trait ConcreteClass2[C[_,_]] {
    def defaultOf[A,B](implicit ea: Elem[A], eb: Elem[B]): Default[Rep[C[A,B]]]
  }

  def identityIso[A](implicit elem: Elem[A]): Iso[A, A] =
    new Iso[A,A] {
      def eTo = elem
      def tag = elem.tag
      def defaultRepTo = elem.defaultRep
      def from(x: Rep[A]) = x
      def to(x: Rep[A]) = x
    }

  def pairIso[A1,B1,A2,B2](iso1: Iso[A1,B1], iso2: Iso[A2,B2]): Iso[(A1, A2), (B1,B2)] = {
    implicit val eA1 = iso1.eFrom
    implicit val eA2 = iso2.eFrom
    implicit val eB1 = iso1.eTo
    implicit val eB2 = iso2.eTo
    val eBB = element[(B1,B2)]
    new Iso[(A1, A2), (B1,B2)] {
      def eTo = eBB
      def from(b: Rep[(B1,B2)]) = (iso1.from(b._1), iso2.from(b._2))
      def to(a: Rep[(A1, A2)]) = (iso1.to(a._1), iso2.to(a._2))
      def tag = eBB.tag
      def defaultRepTo = eBB.defaultRep
    }
  }

  def composeIso[A,B,C](iso2: Iso[B,C], iso1: Iso[A,B]): Iso[A,C] = {
    new Iso[A,C]()(iso1.eFrom) {
      def eTo = iso2.eTo
      def from(c: Rep[C]) = iso1.from(iso2.from(c))
      def to(a: Rep[A]) = iso2.to(iso1.to(a))
      def tag = iso2.tag
      def defaultRepTo = iso2.defaultRepTo
    }
  }
  
  def funcIso[A, B, C, D](iso1: Iso[A, B], iso2: Iso[C, D]): Iso[B => C, A => D] = {
    implicit val eA = iso1.eFrom
    implicit val eB = iso1.eTo
    implicit val eC = iso2.eFrom
    implicit val eD = iso2.eTo
    new Iso[B => C, A => D] {
      lazy val eTo = funcElement(eA, eD)
      def from(f: Rep[A => D]): Rep[B => C] = {
        fun { b => iso2.from(f(iso1.from(b))) }
      }
      def to(f: Rep[B => C]): Rep[A => D] = {
        fun { a => iso2.to(f(iso1.to(a))) }
      }
      def tag = eTo.tag
      def defaultRepTo = eTo.defaultRep
    }
  }
}

trait ViewsSeq extends Views { self: ScalanSeq =>
  class SeqViewElem[From,To](implicit iso: Iso[From, To]) extends ViewElem[From,To](iso) with SeqElement[To] {
    implicit val elemTo = this
    //implicit private def eFrom = iso.eFrom
    implicit private def eTo = iso.eTo
    lazy val tag: TypeTag[To] = iso.tag
    lazy val defaultRep = iso.defaultRepTo
  }

  trait UserTypeSeq[T, TImpl <: T] extends UserType[T] { thisType: T =>
    override def self = this
  }
}

trait ViewsExp extends Views with OptionsExp { self: ScalanStaged =>
  class StagedViewElem[From,To](implicit iso: Iso[From, To]) extends ViewElem[From,To](iso) with StagedElement[To] {
    implicit lazy val tag = iso.tag
    lazy val defaultRep = iso.defaultRepTo
  }

  trait UserTypeDef[T, TImpl <: T] extends ReifiableObject[T, TImpl] {
    override def self = reifyObject(this)(Lazy(selfType.asInstanceOf[Elem[TImpl]]))
    def uniqueOpId = selfType.prettyName
  }
  object UserTypeDef {
    def unapply[T](d: Def[T]): Option[Iso[_,T]] = {
      val eT = d.selfType
      eT match {
        case e: ViewElem[_,_] => Some(e.iso)
        case _ => None
      }
    }
  }
  object UserTypeSym {
    def unapply[T](s: Exp[T]): Option[Iso[_,T]] = {
      val eT = s.elem
      eT match {
        case e: ViewElem[_,_] => Some(e.iso)
        case _ => None
      }
    }
  }

  def symbolHasViews(s: Exp[Any]): Boolean = s match {
    case UserTypeSym(_) => true
    case Def(UserTypeDef(_)) => true
    case _ => false
  }

  implicit class IsoOps[From,To](iso: Iso[From,To]) {
    def toFunTo: Rep[From => To] = fun(iso.to)(Lazy(iso.eFrom))
    def toFunFrom: Rep[To => From] = fun(iso.from)(Lazy(iso.eTo))
  }

  def MethodCallFromExp(clazzUT: Class[_], methodName: String) = new {
    def unapply[T](d:Def[T]): Option[(Exp[_], List[Exp[_]])] = {
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
  
  object View {
    def unapply[T](d: Def[T]): Option[(Rep[Source], Iso[Source,T]) forSome { type Source }] =
      d match {
        case view: View[a, T] => Some((view.source, view.iso))
        // TODO make UserTypeDef extend View with lazy iso/source?
        case UserTypeDef(iso: Iso[a, T]) => Some((iso.from(d.self), iso))
        case _ => None
      }
  }
  
  abstract class View[From, To] extends Def[To] {
    def source: Rep[From]
    def iso: Iso[From, To]
    implicit def selfType = iso.eTo
    lazy val self: Rep[To] = this
    def copy(source: Rep[From]): View[From, To]
    def mirror(t: Transformer) = reifyObject(copy(t(source)))(Lazy(selfType))
    lazy val uniqueOpId = name(iso.eFrom, iso.eTo)
  }
  
  case class UnpackView[A,B](view: Rep[B])(implicit val iso: Iso[A,B]) extends Def[A] {
    implicit def selfType = iso.eFrom
    lazy val self: Rep[A] = this
    override def mirror(f: Transformer) = UnpackView[A, B](f(view))
    lazy val uniqueOpId = name(iso.eFrom, iso.eTo)
  }
  
  abstract class View1[A, B, C[_]](implicit val innerIso: Iso[A, B]) extends View[C[A], C[B]]
  
  abstract class View2[A1, A2, B1, B2, C[_, _]](implicit val iso1: Iso[A1, B1], val iso2: Iso[A2, B2]) extends View[C[A1, A2], C[B1, B2]]
  
//  type Identity[T] = T
//
//  case class ViewVar[A, B](source: Rep[A])(implicit innerIso: Iso[A, B]) extends View1[A, B, Identity] {
//    def iso = innerIso
//    def copy(source: Rep[A]) = ViewVar(source)
//  }
  
  case class ViewPair[A1, A2, B1, B2](source: Rep[(A1, A2)])
    (implicit iso1: Iso[A1, B1], iso2: Iso[A2, B2]) extends View2[A1, A2, B1, B2, Tuple2] {
    lazy val iso = pairIso(iso1, iso2)
    def copy(source: Rep[(A1, A2)]) = ViewPair(source)
  }
  
  override def rewrite[T](d: Exp[T])(implicit eT: LElem[T]) = d match {
    case Def(d1) => d1 match {
      case Tup(Def(View(a, iso1: Iso[a, c])), Def(View(b, iso2: Iso[b, d]))) => 
        ViewPair((a.asRep[a], b.asRep[b]))(iso1, iso2)
      case Tup(Def(View(a, iso1: Iso[a, c])), b: Rep[b]) => 
        ViewPair((a.asRep[a], b))(iso1, identityIso(b.elem)).self
      case Tup(a: Rep[a], Def(View(b, iso2: Iso[b, d]))) => 
        ViewPair((a, b.asRep[b]))(identityIso(a.elem), iso2).self
      case First(Def(view @ ViewPair(source))) =>
        view.iso1.to(source._1)
      case Second(Def(view @ ViewPair(source))) =>
        view.iso2.to(source._2)
//      case View(Def(uv @ UnpackView(view)), iso) if iso == uv.iso => view
//      case uv @ UnpackView(Def(View(source, iso))) if iso == uv.iso => source
      case _ => super.rewrite(d)
    }
    case Var(UserTypeSym(iso: Iso[a, _])) =>
      iso.to(fresh[a](Lazy(iso.eFrom)))
    case _ => super.rewrite(d)
  }
}
