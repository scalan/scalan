package scalan
package impl

import scala.reflect.runtime.universe._
import scala.reflect._
import scalan.common.Default

// Abs -----------------------------------
trait ConvertersAbs extends Converters  {
  self: Scalan =>
  // single proxy for each type family
  implicit def proxyConverter[T, R](p: Rep[Converter[T, R]]): Converter[T, R] = {
    proxyOps[Converter[T, R]](p)(classTag[Converter[T, R]])
  }

  class ConverterElem[T, R, To <: Converter[T, R]](implicit val eDom: Elem[T], val eRange: Elem[R])
    extends EntityElem[To] {
    override def isEntityType = true
    override def tag = {
      implicit val tagT = eDom.tag
      implicit val tagR = eRange.tag
      weakTypeTag[Converter[T, R]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = convertConverter(x.asRep[Converter[T, R]])
    def convertConverter(x : Rep[Converter[T, R]]): Rep[To] = {
      assert(x.selfType1.isInstanceOf[ConverterElem[_,_,_]])
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def converterElement[T, R](implicit eDom: Elem[T], eRange: Elem[R]) =
    new ConverterElem[T, R, Converter[T, R]]()(eDom, eRange)

  trait ConverterCompanionElem extends CompanionElem[ConverterCompanionAbs]
  implicit lazy val ConverterCompanionElem: ConverterCompanionElem = new ConverterCompanionElem {
    lazy val tag = weakTypeTag[ConverterCompanionAbs]
    protected def getDefaultRep = Converter
  }

  abstract class ConverterCompanionAbs extends CompanionBase[ConverterCompanionAbs] with ConverterCompanion {
    override def toString = "Converter"
  }
  def Converter: Rep[ConverterCompanionAbs]
  implicit def proxyConverterCompanion(p: Rep[ConverterCompanion]): ConverterCompanion = {
    proxyOps[ConverterCompanion](p)
  }

  // elem for concrete class
  class ConverterImplElem[T, R](val iso: Iso[ConverterImplData[T, R], ConverterImpl[T, R]])(implicit eDom: Elem[T], eRange: Elem[R])
    extends ConverterElem[T, R, ConverterImpl[T, R]]
    with ViewElem[ConverterImplData[T, R], ConverterImpl[T, R]] {
    override def convertConverter(x: Rep[Converter[T, R]]) = ConverterImpl()
    override def getDefaultRep = super[ViewElem].getDefaultRep
    override lazy val tag = super[ViewElem].tag
  }

  // state representation type
  type ConverterImplData[T, R] = Unit

  // 3) Iso for concrete class
  class ConverterImplIso[T, R](implicit eDom: Elem[T], eRange: Elem[R])
    extends Iso[ConverterImplData[T, R], ConverterImpl[T, R]] {
    override def from(p: Rep[ConverterImpl[T, R]]) =
      unmkConverterImpl(p) match {
        case Some((unit)) => unit
        case None => !!!
      }
    override def to(p: Rep[Unit]) = {
      val unit = p
      ConverterImpl()
    }
    lazy val tag = {
      implicit val tagT = eDom.tag
      implicit val tagR = eRange.tag
      weakTypeTag[ConverterImpl[T, R]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[ConverterImpl[T, R]]](ConverterImpl())
    lazy val eTo = new ConverterImplElem[T, R](this)
  }
  // 4) constructor and deconstructor
  abstract class ConverterImplCompanionAbs extends CompanionBase[ConverterImplCompanionAbs] with ConverterImplCompanion {
    override def toString = "ConverterImpl"
    def apply[T, R](p: Rep[ConverterImplData[T, R]])(implicit eDom: Elem[T], eRange: Elem[R]): Rep[ConverterImpl[T, R]] =
      isoConverterImpl(eDom, eRange).to(p)
    def apply[T, R]()(implicit eDom: Elem[T], eRange: Elem[R]): Rep[ConverterImpl[T, R]] =
      mkConverterImpl()
    def unapply[T:Elem, R:Elem](p: Rep[ConverterImpl[T, R]]) = unmkConverterImpl(p)
  }
  def ConverterImpl: Rep[ConverterImplCompanionAbs]
  implicit def proxyConverterImplCompanion(p: Rep[ConverterImplCompanionAbs]): ConverterImplCompanionAbs = {
    proxyOps[ConverterImplCompanionAbs](p)
  }

  class ConverterImplCompanionElem extends CompanionElem[ConverterImplCompanionAbs] {
    lazy val tag = weakTypeTag[ConverterImplCompanionAbs]
    protected def getDefaultRep = ConverterImpl
  }
  implicit lazy val ConverterImplCompanionElem: ConverterImplCompanionElem = new ConverterImplCompanionElem

  implicit def proxyConverterImpl[T, R](p: Rep[ConverterImpl[T, R]]): ConverterImpl[T, R] =
    proxyOps[ConverterImpl[T, R]](p)

  implicit class ExtendedConverterImpl[T, R](p: Rep[ConverterImpl[T, R]])(implicit eDom: Elem[T], eRange: Elem[R]) {
    def toData: Rep[ConverterImplData[T, R]] = isoConverterImpl(eDom, eRange).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoConverterImpl[T, R](implicit eDom: Elem[T], eRange: Elem[R]): Iso[ConverterImplData[T, R], ConverterImpl[T, R]] =
    new ConverterImplIso[T, R]

  // 6) smart constructor and deconstructor
  def mkConverterImpl[T, R]()(implicit eDom: Elem[T], eRange: Elem[R]): Rep[ConverterImpl[T, R]]
  def unmkConverterImpl[T:Elem, R:Elem](p: Rep[ConverterImpl[T, R]]): Option[(Rep[Unit])]
}

// Seq -----------------------------------
trait ConvertersSeq extends ConvertersDsl  {
  self: ScalanSeq =>
  lazy val Converter: Rep[ConverterCompanionAbs] = new ConverterCompanionAbs with UserTypeSeq[ConverterCompanionAbs, ConverterCompanionAbs] {
    lazy val selfType = element[ConverterCompanionAbs]
  }

  case class SeqConverterImpl[T, R]
      ()
      (implicit eDom: Elem[T], eRange: Elem[R])
    extends ConverterImpl[T, R]()
        with UserTypeSeq[Converter[T,R], ConverterImpl[T, R]] {
    lazy val selfType = element[ConverterImpl[T, R]].asInstanceOf[Elem[Converter[T,R]]]
  }
  lazy val ConverterImpl = new ConverterImplCompanionAbs with UserTypeSeq[ConverterImplCompanionAbs, ConverterImplCompanionAbs] {
    lazy val selfType = element[ConverterImplCompanionAbs]
  }

  def mkConverterImpl[T, R]
      ()(implicit eDom: Elem[T], eRange: Elem[R]): Rep[ConverterImpl[T, R]] =
      new SeqConverterImpl[T, R]()
  def unmkConverterImpl[T:Elem, R:Elem](p: Rep[ConverterImpl[T, R]]) =
    Some(())
}

// Exp -----------------------------------
trait ConvertersExp extends ConvertersDsl  {
  self: ScalanExp =>
  lazy val Converter: Rep[ConverterCompanionAbs] = new ConverterCompanionAbs with UserTypeDef[ConverterCompanionAbs, ConverterCompanionAbs] {
    lazy val selfType = element[ConverterCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpConverterImpl[T, R]
      ()
      (implicit eDom: Elem[T], eRange: Elem[R])
    extends ConverterImpl[T, R]() with UserTypeDef[Converter[T,R], ConverterImpl[T, R]] {
    lazy val selfType = element[ConverterImpl[T, R]].asInstanceOf[Elem[Converter[T,R]]]
    override def mirror(t: Transformer) = ExpConverterImpl[T, R]()
  }

  lazy val ConverterImpl: Rep[ConverterImplCompanionAbs] = new ConverterImplCompanionAbs with UserTypeDef[ConverterImplCompanionAbs, ConverterImplCompanionAbs] {
    lazy val selfType = element[ConverterImplCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object ConverterImplMethods {
    object apply {
      def unapply(d: Def[_]): Option[(Rep[ConverterImpl[T, R]], Rep[T]) forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[ConverterImplElem[_, _]] && method.getName == "apply" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[ConverterImpl[T, R]], Rep[T]) forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConverterImpl[T, R]], Rep[T]) forSome {type T; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ConverterImplCompanionMethods {
  }

  def mkConverterImpl[T, R]
    ()(implicit eDom: Elem[T], eRange: Elem[R]): Rep[ConverterImpl[T, R]] =
    new ExpConverterImpl[T, R]()
  def unmkConverterImpl[T:Elem, R:Elem](p: Rep[ConverterImpl[T, R]]) =
    Some(())

  object ConverterMethods {
    object apply {
      def unapply(d: Def[_]): Option[(Rep[Converter[T, R]], Rep[T]) forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[ConverterElem[_, _, _]] && method.getName == "apply" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[Converter[T, R]], Rep[T]) forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Converter[T, R]], Rep[T]) forSome {type T; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ConverterCompanionMethods {
  }
}
