package scalan

import scala.text._
import Document._
import scalan.common.Zero

trait Views extends Base with Reification { self: Scalan =>

  trait GenIso[A,B] {
    def eA: Elem[A]
    def eB: Elem[B]
    def from: B => A
    def to: A => B
    def manifest: Manifest[B]
    def zero: Zero[Rep[B]]
    def fromStaged: Rep[B] => Rep[A]
    def toStaged: Rep[A] => Rep[B]
  }
  type Iso[A,B] = GenIso[A,B]

  abstract class IsoBase[A,B](implicit ea: Elem[A]) extends GenIso[A,B] { ///self: Iso[A,B] =>
    lazy val eA = ea

    // eB should be lazy val as it is used recursively in ViewElem
    // override it in concrete isos to create hierarchy of Elem classes
    lazy val eB: Elem[B] = defaultViewElem(this)
//    def from: B => A = (x: B) => ???
//    def to: A => B = (x: A) => ???
    def fromStaged: Rep[B] => Rep[A] = (x: Rep[B]) => ???("Not implemented ", x)
    def toStaged: Rep[A] => Rep[B] = (x:Rep[A]) => ???("Not implemented ", x)
  }

  protected[scalan] def defaultViewElem[A,B](implicit iso: Iso[A,B]): Elem[B]

  implicit def viewElement[A,B](implicit iso: Iso[A,B]): Elem[B] = iso.eB  // always ask elem from Iso

  trait ViewElem[A,B] extends Element[B] {
    def iso: Iso[A,B]
  }

  trait IdentityIso[A] extends GenIso[A,A] { //self: Iso[A,A] =>
    override def from = (x: A) => x
    override def to = (x: A) => x
    override def fromStaged = (x: Rep[A]) => x
    override def toStaged = (x: Rep[A]) => x
  }

  def iso[A,B](implicit i: Iso[A,B]) = i

  //final class uncheckedVariance extends scala.annotation.StaticAnnotation {}


}

trait ViewsSeq extends Views { self: ScalanSeq =>

  trait SeqIso[A,B] extends GenIso[A,B] {
    def from = x => this.fromStaged(x)
    def to = x => this.toStaged(x)
  }

  protected[scalan] def defaultViewElem[A,B](implicit i: Iso[A,B]) = new SeqViewElem[A,B] { val iso = i }

  trait SeqViewElem[A,B] extends ViewElem[A,B] with SeqElement[B] {
    implicit val elemB = this
    implicit private def eA = iso.eA
    implicit private def eB = iso.eB
    private lazy val m: Manifest[B] = iso.manifest
    private lazy val z = iso.zero
    def manifest: Manifest[B] = m
    def zero = z
  }
}

trait ViewsExp extends Views with OptionsExp { self: ScalanStaged =>

  trait StagedIso[A,B] extends GenIso[A,B] {
    def from = x => ???
    def to = x => ???
  }

  protected[scalan] def defaultViewElem[A,B](implicit i: Iso[A,B]) = new StagedViewElem[A,B] { val iso = i }

  trait StagedViewElem[A,B] extends ViewElem[A,B] with StagedElement[B] {
    implicit val elemB = this
    implicit private def eA = iso.eA
    implicit private def eB = iso.eB
    implicit private lazy val m = iso.manifest
    private lazy val z = iso.zero
    def manifest: Manifest[B] = m
    def zero = z
    override def toRep(p: B) = iso.toStaged(eA.toRep(iso.from(p)))
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
    val symClazz = s.Elem.manifest.runtimeClass

    //manifest[UserType[_]].erasure.isAssignableFrom(symClazz)
    //userTypes.find(c => c.manifest.erasure.isAssignableFrom(symClazz)).isDefined
    userTypes.exists(c => symClazz.isAssignableFrom(c.manifest.runtimeClass))
  }

  trait UserType[T] extends Def[T] {
    //type ThisType = T
    def elem: Elem[T]
    override def thisSymbol = reifyObject(this)(elem)
  }
  object UserType {
    def unapply[T](d: Def[T]): Option[Iso[_,T]] = {
      val s = d.thisSymbol
      val eT = s.Elem
      eT match {
        case e: ViewElem[_,_] if isUserTypeConstr(d) => Some(e.asInstanceOf[ViewElem[_,T]].iso)
        case _ => None
      }
    }
  }
  object UserTypeSym {
    def unapply[T](s: Exp[T]): Option[Iso[_,T]] = {
      val eT = s.Elem
      eT match {
        case e: ViewElem[_,_] if isUserTypeSym(s) => Some(e.asInstanceOf[ViewElem[_,T]].iso)
        case _ => None
      }
    }
  }

  implicit class IsoOps[A,B](iso: Iso[A,B]) {
    def toFunTo: Rep[A => B] = fun(iso.toStaged)(iso.eA, iso.eB)
    def toFunFrom: Rep[B => A] = fun(iso.fromStaged)(iso.eB, iso.eA)
  }
  //implicit def isoToOps[A,B](iso: Iso[A,B]) = new IsoOps(iso)

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
