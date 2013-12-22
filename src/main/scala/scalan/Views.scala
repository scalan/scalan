package scalan

import scala.text._
import Document._
import scalan.common.{DefaultOf, Zero}

trait Views extends Base with Reification { self: Scalan =>

  trait GenIso[From,To] {
    def eFrom: Elem[From]
    def eTo: Elem[To]
    def from: To => From
    def to: From => To
    def manifest: Manifest[To]
    def defaultOf: DefaultOf[Rep[To]]
    def fromStaged: Rep[To] => Rep[From]
    def toStaged: Rep[From] => Rep[To]
  }
  type Iso[From,To] = GenIso[From,To]

  abstract class IsoBase[From,To](implicit efrom: Elem[From]) extends GenIso[From,To] { ///self: Iso[A,B] =>
    lazy val eFrom = efrom

    // eB should be lazy val as it is used recursively in ViewElem
    // override it in concrete isos to create hierarchy of Elem classes
    lazy val eTo: Elem[To] = defaultViewElem(this)
    def fromStaged: Rep[To] => Rep[From] = (x: Rep[To]) => ???("Not implemented ", x)
    def toStaged: Rep[From] => Rep[To] = (x:Rep[From]) => ???("Not implemented ", x)
  }

  protected[scalan] def defaultViewElem[From,To](implicit iso: Iso[From,To]): Elem[To]

  implicit def viewElement[From,To](implicit iso: Iso[From,To], ut: To <:< UserType[_]): Elem[To] = iso.eTo  // always ask elem from Iso

  trait ViewElem[From,To] extends Element[To] {
    def iso: Iso[From,To]
  }

  trait IdentityIso[A] extends GenIso[A,A] { //self: Iso[A,A] =>
    override def from = (x: A) => x
    override def to = (x: A) => x
    override def fromStaged = (x: Rep[A]) => x
    override def toStaged = (x: Rep[A]) => x
  }

  def iso[From,To](implicit i: Iso[From,To]) = i

  trait UserType[T] {
    def Elem: Elem[T]
  }
}

trait ViewsSeq extends Views { self: ScalanSeq =>

  trait SeqIso[From,To] extends GenIso[From,To] {
    def from = x => this.fromStaged(x)
    def to = x => this.toStaged(x)
  }

  protected[scalan] def defaultViewElem[From,To](implicit i: Iso[From,To]) = new SeqViewElem[From,To] { val iso = i }

  trait SeqViewElem[From,To] extends ViewElem[From,To] with SeqElement[To] {
    implicit val elemTo = this
    implicit private def eFrom = iso.eFrom
    implicit private def eTo = iso.eTo
    private lazy val m: Manifest[To] = iso.manifest
    private lazy val z = iso.defaultOf
    def manifest: Manifest[To] = m
    def defaultOf = z
  }
}

trait ViewsExp extends Views with OptionsExp { self: ScalanStaged =>

  trait StagedIso[From,To] extends GenIso[From,To] {
    def from = x => ???
    def to = x => ???
  }

  protected[scalan] def defaultViewElem[From,To](implicit i: Iso[From,To]) = new StagedViewElem[From,To] { val iso = i }

  trait StagedViewElem[From,To] extends ViewElem[From,To] with StagedElement[To] {
    implicit val elemTo = this
    implicit private def eFrom = iso.eFrom
    implicit private def eTo = iso.eTo
    implicit private lazy val m = iso.manifest
    private lazy val z = iso.defaultOf
    def manifest: Manifest[To] = m
    def defaultOf = z
    //override def toRep(p: To) = iso.toStaged(eFrom.toRep(iso.from(p)))
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

  trait UserTypeDef[T, TImpl] extends UserType[T] with ReifiableObject[TImpl] {
    //type ThisType = T
    override def thisSymbol = reifyObject(this)(Elem.asInstanceOf[Elem[TImpl]])
  }
  object UserTypeDef {
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

  implicit class IsoOps[From,To](iso: Iso[From,To]) {
    def toFunTo: Rep[From => To] = fun(iso.toStaged)(iso.eFrom, iso.eTo)
    def toFunFrom: Rep[To => From] = fun(iso.fromStaged)(iso.eTo, iso.eFrom)
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
