
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
  trait TypeImplElem[A] extends TypeElem[TypeImplData[A], TypeImpl[A]]

  // state representation type
  type TypeImplData[A] = (String, A)

  // 3) companion object with Iso, constructor and deconstructor
  object TypeImpl extends TypeImplCompanion {
    abstract class Iso[A](implicit  eA: Elem[A])
           extends IsoBase[TypeImplData[A], TypeImpl[A]] {
      override def fromStaged = { case TypeImpl(typeCode, defaultValue) => Pair(typeCode, defaultValue) }
      override def toStaged = (p: Rep[(String, A)]) => {
        val Pair(typeCode, defaultValue) = p
        TypeImpl(typeCode, defaultValue)
      }
      def manifest = { 
        implicit val mA = element[A].manifest
        Predef.manifest[TypeImpl[A]] 
      }
      def defaultOf = Common.defaultVal[Rep[TypeImpl[A]]](TypeImpl("", element[A].defaultOf.value))
    }

    def apply[A](p: Rep[TypeImplData[A]])(implicit  eA: Elem[A]): Rep[TypeImpl[A]]
        = isoTypeImpl(eA).toStaged(p)
    def apply[A](p: Type[A])(implicit  eA: Elem[A]): Rep[TypeImpl[A]]
        = mkTypeImpl(p.typeCode, p.defaultValue)
    def apply[A]
          (typeCode: Rep[String], defaultValue: Rep[A])
          (implicit  eA: Elem[A]): Rep[TypeImpl[A]]
        = mkTypeImpl(typeCode, defaultValue)
    def unapply[A:Elem](p: Rep[TypeImpl[A]]) = unmkTypeImpl(p)
  }

  implicit def proxyTypeImpl[A](p: Rep[TypeImpl[A]])(implicit  eA: Elem[A]): TypeImplOps[A] = {
    implicit val mA = element[A].manifest;
    proxyOps[TypeImplOps[A], TypeImplOps[A]](p)
  }

  implicit def extendTypeImpl[A](p: Rep[TypeImpl[A]])(implicit  eA: Elem[A]) = new {
    def toData: Rep[TypeImplData[A]] = isoTypeImpl(eA).fromStaged(p)
  }

  // 4) implicit resolution of Iso
  implicit def isoTypeImpl[A](implicit  eA: Elem[A]): Iso[TypeImplData[A], TypeImpl[A]]

  // 5) smart constructor and deconstructor
  def mkTypeImpl[A](typeCode: Rep[String], defaultValue: Rep[A])(implicit  eA: Elem[A]): Rep[TypeImpl[A]]
  def unmkTypeImpl[A](p: Rep[TypeImpl[A]])(implicit  eA: Elem[A]): Option[(Rep[String], Rep[A])]

}


trait TypesSeq extends TypesAbs
{ self: ScalanSeq with TypesDsl =>

  case class SeqTypeImpl[A]
      (override val typeCode: Rep[String], override val defaultValue: Rep[A])
      (implicit override val eA: Elem[A])
      extends TypeImpl[A](typeCode, defaultValue) with TypeImplOps[A] {
    def Elem = element[TypeImpl[A]].asInstanceOf[Elem[Type[A]]]
  }


  implicit def isoTypeImpl[A](implicit  eA: Elem[A]): Iso[TypeImplData[A], TypeImpl[A]]
    = new TypeImpl.Iso[A] with SeqIso[TypeImplData[A], TypeImpl[A]] { i =>
        // should use i as iso reference
        override lazy val eTo = new SeqViewElem[TypeImplData[A], TypeImpl[A]]
                                    with TypeImplElem[A] { val iso = i }
      }


  def mkTypeImpl[A]
      (typeCode: Rep[String], defaultValue: Rep[A])
      (implicit  eA: Elem[A])
      = new SeqTypeImpl[A](typeCode, defaultValue)
  def unmkTypeImpl[A](p: Rep[TypeImpl[A]])
      (implicit  eA: Elem[A])
    = Some((p.typeCode, p.defaultValue))

}


trait TypesExp extends TypesAbs with ProxyExp with ViewsExp
{ self: ScalanStaged with TypesDsl =>

  case class ExpTypeImpl[A]
      (override val typeCode: Rep[String], override val defaultValue: Rep[A])
      (implicit override val eA: Elem[A])
    extends TypeImpl[A](typeCode, defaultValue) with TypeImplOps[A]
       with UserTypeDef[Type[A],TypeImpl[A]] {
    lazy val objType = element[TypeImpl[A]]
    def Elem = objType.asInstanceOf[Elem[Type[A]]]
    def elem = element[TypeImpl[A]]
    override def mirror(t: Transformer): Rep[_] = ExpTypeImpl[A](t(typeCode), t(defaultValue))
  }
  addUserType(manifest[ExpTypeImpl[Any]])


  def mkTypeImpl[A]
      (typeCode: Rep[String], defaultValue: Rep[A])
      (implicit  eA: Elem[A])
      = new ExpTypeImpl[A](typeCode, defaultValue)
  def unmkTypeImpl[A]
      (p: Rep[TypeImpl[A]])
      (implicit  eA: Elem[A])
    = Some((p.typeCode, p.defaultValue))


  implicit def isoTypeImpl[A](implicit  eA: Elem[A]): Iso[TypeImplData[A], TypeImpl[A]]
    = new TypeImpl.Iso[A] with StagedIso[TypeImplData[A], TypeImpl[A]] { i =>
        // should use i as iso reference
        override lazy val eTo = new StagedViewElem[TypeImplData[A], TypeImpl[A]]
                                    with TypeImplElem[A] { val iso = i }
      }

}
