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
    lazy val eB: Elem[B] = viewElement(this)
//    def from: B => A = (x: B) => ???
//    def to: A => B = (x: A) => ???
    def fromStaged: Rep[B] => Rep[A] = (x: Rep[B]) => ???("Not implemented ", x)
    def toStaged: Rep[A] => Rep[B] = (x:Rep[A]) => ???("Not implemented ", x)
  }


  implicit def viewElement[A,B](implicit iso: Iso[A,B]): Elem[B]

  abstract class ViewElem[A,B](val iso: Iso[A,B]) extends Element[B] {
  }

  trait IdentityIso[A] extends GenIso[A,A] { //self: Iso[A,A] =>
    override def from = (x: A) => x
    override def to = (x: A) => x
    override def fromStaged = (x: Rep[A]) => x
    override def toStaged = (x: Rep[A]) => x
  }

  def iso[A,B](implicit i: Iso[A,B]) = i

}

trait ViewsSeq extends Views { self: ScalanSeq =>

  trait SeqIso[A,B] extends GenIso[A,B] {
    def from = x => this.fromStaged(x)
    def to = x => this.toStaged(x)
  }

  override implicit def viewElement[A,B](implicit iso: Iso[A,B]): Elem[B] = new SeqViewElem[A,B]

  class SeqViewElem[A,B](implicit iso: Iso[A,B]) extends ViewElem[A,B](iso) with SeqElement[B] {
    implicit val elemB = this
    implicit private def eA = iso.eA
    implicit private def eB = iso.eB
    private lazy val m: Manifest[B] = iso.manifest
    private lazy val z = iso.zero
    def manifest: Manifest[B] = m
    def zero = z

    //    def replicate(count: IntRep, v: Rep[B]) =
    //      SeqViewArray(Some(eA.replicate(count, iso.fromStaged(v))), iso)
    //
    //    def replicateSeg(count: IntRep, v: PA[B]) = {
    //      val va = v.asInstanceOf[ViewArray[A,B]]
    //      va.arr match {
    //        case Some(arr) => SeqViewArray(Some(eA.replicateSeg(count, arr)), iso)
    //        case None => SeqViewArray(None, iso)
    //      }
    //    }
    //
    //    def tabulate(len: IntRep)(f:IntRep => B) = {
    //      val arr = if (len === 0) None else Some(eA.tabulate(len)(iso.fromStaged compose f))
    //      SeqViewArray(arr, iso)
    //    }
    //    def tabulateSeg(len: IntRep)(f:IntRep => PA[B]) = {
    //      val fa = (i:IntRep) => { val segB = f(i); segB.asInstanceOf[ViewArray[A,B]].arrOrEmpty }
    //      val arr = eA.tabulateSeg(len)(fa)
    //      SeqViewArray(Some(arr), iso)
    //    }
    //    def empty = SeqViewArray(None, iso)
  }


  //  implicit def extendViewArray[A,B](xs: PA[B])(implicit iso: Iso[A,B]): ViewArrayExtensions[A,B] = {
  //    implicit val eB = xs.elem
  //    implicit val mB = eB.manifest
  //    new ViewArrayExtensions[A,B] {
  //      val elem = eB
  //      protected def resolved = xs
  //      def arr = asViewArray(_.arr, !!!)
  //      def iso = asViewArray(_.iso, !!!)
  //      def arrOrEmpty = asViewArray(_.arrOrEmpty, !!!)
  //    }
  //  }


}

trait ViewsExp extends Views with OptionsExp { self: ScalanStaged =>

  trait StagedIso[A,B] extends GenIso[A,B] {
    def from = x => ???
    def to = x => ???
  }

  override implicit def viewElement[A, B](implicit iso: Iso[A, B]): Elem[B] = new StagedViewElem[A,B]

  class StagedViewElem[A,B](implicit iso: Iso[A,B]) extends ViewElem[A,B](iso) with StagedElement[B] {
    implicit val elemB = this
    implicit private def eA = iso.eA
    implicit private def eB = iso.eB
    implicit private lazy val m = iso.manifest
    private lazy val z = iso.zero
    def manifest: Manifest[B] = m
    def zero = z

//    def replicate(count: IntRep, v: Rep[B]) =
//      ExpViewArray(Some(eA.replicate(count, iso.fromStaged(v))), iso)
//
//    //def replicateSeg(count: IntRep, v: PA[B]) = ???
//    //    v match {
//    //      case Def(ExpViewArray(arr, iso1))
//    //           if iso1 == iso => ExpViewArray(eA.replicateSeg(count, arr.asInstanceOf[PA[A]]), iso)
//    //      case _ =>
//    //        ExpViewArray(eA.replicateSeg(count, ArrayFromView(v, iso)), iso)
//    //    }
//
//    def tabulate(len: IntRep)(f: IntRep => Rep[B]) = {
//      val arr = eA.tabulate(len)(iso.fromStaged compose f)
//      ExpViewArray(Some(arr), iso)
//    }
//
//    def tabulateSeg(len: IntRep)(f: IntRep => PA[B]) = ???
//    //    {
//    //      val fa = (i: IntRep) => {
//    //        val segB = f(i); segB.asInstanceOf[ViewArray[A, B]].arr
//    //      }
//    //      val arr = eA.tabulateSeg(len)(fa)
//    //      ExpViewArray(arr, iso)
//    //    }
//
//    def empty = ExpViewArray(Some(eA.empty), iso)
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
