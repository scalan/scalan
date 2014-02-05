package scalan

import scala.text._
import Document._
import scalan.common.DefaultOf
import scala.language.higherKinds

trait Views extends Base { self: Scalan =>

  trait Iso[From,To] {
    //def eFrom: Elem[From]
    def eTo: Elem[To]
    def manifest: Manifest[To]        // constructed in each concrete iso instance
    def defaultOf: DefaultOf[Rep[To]] // constructed in each concrete iso instance
    def from(p: Rep[To]  ): Rep[From]
    def to  (p: Rep[From]): Rep[To]
  }

  abstract class IsoBase[From,To] extends Iso[From,To] {
    //------
    // eB should be lazy val as it is used recursively in ViewElem
    // override it in concrete isos to create hierarchy of Elem classes
    lazy val eTo: Elem[To] = defaultViewElem(this)
  }

  protected[scalan] def defaultViewElem[From,To](implicit iso: Iso[From,To]): Elem[To]

  implicit def viewElement[From,To](implicit iso: Iso[From,To], ut: To <:< UserType[_]): Elem[To] = iso.eTo  // always ask elem from Iso

  trait ViewElem[From,To] extends Elem[To] {
    def iso: Iso[From,To]
  }

  trait UserType[T] {
    def selfType: Elem[T]
    def self: Rep[T] = !!!("should not be called")
  }

  trait TypeFamily1[F[_]] {
    def defaultOf[A](implicit ea: Elem[A]): DefaultOf[Rep[F[A]]]
  }
  trait TypeFamily2[F[_,_]] {
    def defaultOf[A,B](implicit ea: Elem[A], eb: Elem[B]): DefaultOf[Rep[F[A,B]]]
  }

  trait ConcreteClass1[C[_]] {
    def defaultOf[A](implicit ea: Elem[A]): DefaultOf[Rep[C[A]]]
  }
  trait ConcreteClass2[C[_,_]] {
    def defaultOf[A,B](implicit ea: Elem[A], eb: Elem[B]): DefaultOf[Rep[C[A,B]]]
  }

}

trait ViewsSeq extends Views { self: ScalanSeq =>

  trait SeqIso[From,To] extends Iso[From,To] {
  }

  protected[scalan] def defaultViewElem[From,To](implicit i: Iso[From,To]) = new SeqViewElem[From,To] { val iso = i }

  trait SeqViewElem[From,To] extends ViewElem[From,To] with SeqElement[To] {
    implicit val elemTo = this
    //implicit private def eFrom = iso.eFrom
    implicit private def eTo = iso.eTo
    private lazy val m: Manifest[To] = iso.manifest
    private lazy val z = iso.defaultOf
    def manifest: Manifest[To] = m
    def defaultOf = z
  }

  trait UserTypeSeq[T, TImpl <: T] extends UserType[T] { thisType: T =>
    override def self = this
  }

}

trait ViewsExp extends Views with OptionsExp { self: ScalanStaged =>

  trait StagedIso[From,To] extends Iso[From,To] {
  }

  protected[scalan] def defaultViewElem[From,To](implicit i: Iso[From,To]) = new StagedViewElem[From,To] { val iso = i }

  trait StagedViewElem[From,To] extends ViewElem[From,To] with StagedElement[To] {
    implicit val elemTo = this
    //implicit private def eFrom = iso.eFrom
    implicit private def eTo = iso.eTo
    implicit private lazy val m = iso.manifest
    private lazy val z = iso.defaultOf
    def manifest: Manifest[To] = m
    def defaultOf = z
  }

  case class UserTypeDescriptor[T](manifest: Manifest[T])
  var userTypes = List.empty[UserTypeDescriptor[_]]
  def addUserType(m: Manifest[_]) {
    userTypes = userTypes :+ UserTypeDescriptor(m)
  }

  def isUserTypeConstr[T](d: Def[T]): Boolean = {
    val clazz = d.getClass
    userTypes.exists(c => c.manifest.runtimeClass == clazz)
  }
  def isUserTypeSym[T](s: Exp[T]): Boolean = {
    val symClazz = s.elem.manifest.runtimeClass

    //manifest[UserType[_]].erasure.isAssignableFrom(symClazz)
    //userTypes.find(c => c.manifest.erasure.isAssignableFrom(symClazz)).isDefined
    userTypes.exists(c => symClazz.isAssignableFrom(c.manifest.runtimeClass))
  }

  trait UserTypeExp[T, TImpl <: T] extends ReifiableObject[T, TImpl] {
    override def self = reifyObject(this)(() => selfType.asInstanceOf[Elem[TImpl]])
  }
  object UserTypeExp {
    def unapply[T](d: Def[T]): Option[Iso[_,T]] = {
      val s = d.self
      val eT = s.elem
      eT match {
        case e: ViewElem[_,_] if isUserTypeConstr(d) => Some(e.asInstanceOf[ViewElem[_,T]].iso)
        case _ => None
      }
    }
  }
  object UserTypeSym {
    def unapply[T](s: Exp[T]): Option[Iso[_,T]] = {
      val eT = s.elem
      eT match {
        case e: ViewElem[_,_] if isUserTypeSym(s) => Some(e.asInstanceOf[ViewElem[_,T]].iso)
        case _ => None
      }
    }
  }

  implicit class IsoOps[From: LElem,To](iso: Iso[From,To]) {
    def toFunTo: Rep[From => To] = fun(iso.to)
    def toFunFrom: Rep[To => From] = fun(iso.from)(() => iso.eTo)
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



}
