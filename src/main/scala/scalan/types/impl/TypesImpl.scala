
package scalan.types

import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance
import scalan.common.{DefaultOf,Common}
import Common._
import scala.language.implicitConversions
import scalan._


trait TypesAbs extends Types
{ self: TypesDsl =>

  // single proxy for each type family
  implicit def proxyType[A:Elem](p: Ty[A]): Type[A] = {
    implicit val mA = element[A].manifest;
    proxyOps[Type[A], Type[A]](p)
  }

  trait TypeElem[From,To] extends ViewElem[From, To]

  // elem for concrete class
  trait BaseTypeElem[A] extends TypeElem[BaseTypeData[A], BaseType[A]]

  // state representation type
  type BaseTypeData[A] = (String, A)

  // 3) companion object with Iso, constructor and deconstructor
  object BaseType extends BaseTypeCompanion {
    abstract class Iso[A](implicit  eA: Elem[A])
           extends IsoBase[BaseTypeData[A], BaseType[A]] {
      override def from = { case BaseType(typeCode, defaultValue) => Pair(typeCode, defaultValue) }
      override def to = (p: Rep[(String, A)]) => {
        val Pair(typeCode, defaultValue) = p
        BaseType(typeCode, defaultValue)
      }
      def manifest = { 
        implicit val mA = element[A].manifest
        Predef.manifest[BaseType[A]] 
      }
      def defaultOf = Common.defaultVal[Rep[BaseType[A]]](BaseType("", element[A].defaultOf.value))
    }

    def apply[A](p: Rep[BaseTypeData[A]])(implicit  eA: Elem[A]): Rep[BaseType[A]]
        = isoBaseType(eA).to(p)
    def apply[A](p: Type[A])(implicit  eA: Elem[A]): Rep[BaseType[A]]
        = mkBaseType(p.typeCode, p.defaultValue)
    def apply[A]
          (typeCode: Rep[String], defaultValue: Rep[A])
          (implicit  eA: Elem[A]): Rep[BaseType[A]]
        = mkBaseType(typeCode, defaultValue)
    def unapply[A:Elem](p: Rep[BaseType[A]]) = unmkBaseType(p)
  }

  implicit def proxyBaseType[A](p: Rep[BaseType[A]])(implicit  eA: Elem[A]): BaseTypeOps[A] = {
    implicit val mA = element[A].manifest;
    proxyOps[BaseTypeOps[A], BaseTypeOps[A]](p)
  }

  implicit def extendBaseType[A](p: Rep[BaseType[A]])(implicit  eA: Elem[A]) = new {
    def toData: Rep[BaseTypeData[A]] = isoBaseType(eA).from(p)
  }

  // 4) implicit resolution of Iso
  implicit def isoBaseType[A](implicit  eA: Elem[A]): Iso[BaseTypeData[A], BaseType[A]]

  // 5) smart constructor and deconstructor
  def mkBaseType[A](typeCode: Rep[String], defaultValue: Rep[A])(implicit  eA: Elem[A]): Rep[BaseType[A]]
  def unmkBaseType[A](p: Rep[BaseType[A]])(implicit  eA: Elem[A]): Option[(Rep[String], Rep[A])]

}


trait TypesSeq extends TypesAbs
{ self: ScalanSeq with TypesDsl =>

  case class SeqBaseType[A]
      (override val typeCode: Rep[String], override val defaultValue: Rep[A])
      (implicit override val eA: Elem[A])
      extends BaseType[A](typeCode, defaultValue) with BaseTypeOps[A] {
    def elem = element[BaseType[A]].asInstanceOf[Elem[Type[A]]]
  }


  implicit def isoBaseType[A](implicit  eA: Elem[A]): Iso[BaseTypeData[A], BaseType[A]]
    = new BaseType.Iso[A] with SeqIso[BaseTypeData[A], BaseType[A]] { i =>
        // should use i as iso reference
        override lazy val eTo = new SeqViewElem[BaseTypeData[A], BaseType[A]]
                                    with BaseTypeElem[A] { val iso = i }
      }


  def mkBaseType[A]
      (typeCode: Rep[String], defaultValue: Rep[A])
      (implicit  eA: Elem[A])
      = new SeqBaseType[A](typeCode, defaultValue)
  def unmkBaseType[A](p: Rep[BaseType[A]])
      (implicit  eA: Elem[A])
    = Some((p.typeCode, p.defaultValue))

}


trait TypesExp extends TypesAbs with ProxyExp with ViewsExp
{ self: ScalanStaged with TypesDsl =>

  case class ExpBaseType[A]
      (override val typeCode: Rep[String], override val defaultValue: Rep[A])
      (implicit override val eA: Elem[A])
    extends BaseType[A](typeCode, defaultValue) with BaseTypeOps[A]
       with UserTypeDef[Type[A],BaseType[A]] {
    lazy val objType = element[BaseType[A]]
    def elem = objType.asInstanceOf[Elem[Type[A]]]
    override def mirror(t: Transformer): Rep[_] = ExpBaseType[A](t(typeCode), t(defaultValue))
  }
  addUserType(manifest[ExpBaseType[Any]])


  def mkBaseType[A]
      (typeCode: Rep[String], defaultValue: Rep[A])
      (implicit  eA: Elem[A])
      = new ExpBaseType[A](typeCode, defaultValue)
  def unmkBaseType[A]
      (p: Rep[BaseType[A]])
      (implicit  eA: Elem[A])
    = Some((p.typeCode, p.defaultValue))


  implicit def isoBaseType[A](implicit  eA: Elem[A]): Iso[BaseTypeData[A], BaseType[A]]
    = new BaseType.Iso[A] with StagedIso[BaseTypeData[A], BaseType[A]] { i =>
        // should use i as iso reference
        override lazy val eTo = new StagedViewElem[BaseTypeData[A], BaseType[A]]
                                    with BaseTypeElem[A] { val iso = i }
      }

}
