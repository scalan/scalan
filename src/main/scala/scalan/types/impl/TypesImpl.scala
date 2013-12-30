
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
  implicit def defaultTypeElement[A:Elem]: Elem[Type[A]] = ???


  //------------------------------- BaseType ------------------------------------
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
    //def apply[A](p: Type[A])(implicit  eA: Elem[A]): Rep[BaseType[A]]
    //    = mkBaseType(p.typeCode, p.defaultValue)
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


  //------------------------------- Tuple2Type ------------------------------------
  // elem for concrete class
  trait Tuple2TypeElem[A, B] extends TypeElem[Tuple2TypeData[A, B], Tuple2Type[A, B]]

  // state representation type
  type Tuple2TypeData[A, B] = (Type[A], Type[B])

  // 3) companion object with Iso, constructor and deconstructor
  object Tuple2Type extends Tuple2TypeCompanion {
    abstract class Iso[A, B](implicit  eA: Elem[A],  eB: Elem[B])
           extends IsoBase[Tuple2TypeData[A, B], Tuple2Type[A, B]] {
      override def from = { case Tuple2Type(tyA, tyB) => Pair(tyA, tyB) }
      override def to = (p: Rep[(Type[A], Type[B])]) => {
        val Pair(tyA, tyB) = p
        Tuple2Type(tyA, tyB)
      }
      def manifest = { 
        implicit val mA = element[A].manifest
        implicit val mB = element[B].manifest
        Predef.manifest[Tuple2Type[A, B]] 
      }
      def defaultOf = Common.defaultVal[Rep[Tuple2Type[A, B]]](Tuple2Type(element[Type[A]].defaultOf.value, element[Type[B]].defaultOf.value))
    }

    def apply[A, B](p: Rep[Tuple2TypeData[A, B]])(implicit  eA: Elem[A],  eB: Elem[B]): Rep[Tuple2Type[A, B]]
        = isoTuple2Type(eA, eB).to(p)
    //def apply[A, B](p: Type[A])(implicit  eA: Elem[A],  eB: Elem[B]): Rep[Tuple2Type[A, B]]
    //    = mkTuple2Type(p.tyA, p.tyB)
    def apply[A, B]
          (tyA: Ty[A], tyB: Ty[B])
          (implicit  eA: Elem[A],  eB: Elem[B]): Rep[Tuple2Type[A, B]]
        = mkTuple2Type(tyA, tyB)
    def unapply[A:Elem, B:Elem](p: Rep[Tuple2Type[A, B]]) = unmkTuple2Type(p)
  }

  implicit def proxyTuple2Type[A, B](p: Rep[Tuple2Type[A, B]])(implicit  eA: Elem[A],  eB: Elem[B]): Tuple2TypeOps[A, B] = {
    implicit val mA = element[A].manifest;
    implicit val mB = element[B].manifest;
    proxyOps[Tuple2TypeOps[A, B], Tuple2TypeOps[A, B]](p)
  }

  implicit def extendTuple2Type[A, B](p: Rep[Tuple2Type[A, B]])(implicit  eA: Elem[A],  eB: Elem[B]) = new {
    def toData: Rep[Tuple2TypeData[A, B]] = isoTuple2Type(eA, eB).from(p)
  }

  // 4) implicit resolution of Iso
  implicit def isoTuple2Type[A, B](implicit  eA: Elem[A],  eB: Elem[B]): Iso[Tuple2TypeData[A, B], Tuple2Type[A, B]]

  // 5) smart constructor and deconstructor
  def mkTuple2Type[A, B](tyA: Ty[A], tyB: Ty[B])(implicit  eA: Elem[A],  eB: Elem[B]): Rep[Tuple2Type[A, B]]
  def unmkTuple2Type[A, B](p: Rep[Tuple2Type[A, B]])(implicit  eA: Elem[A],  eB: Elem[B]): Option[(Rep[Type[A]], Rep[Type[B]])]

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


  case class SeqTuple2Type[A, B]
      (override val tyA: Ty[A], override val tyB: Ty[B])
      (implicit override val e1: Elem[A], override val e2: Elem[B])
      extends Tuple2Type[A, B](tyA, tyB) with Tuple2TypeOps[A,B] {
    def elem = element[Tuple2Type[A, B]].asInstanceOf[Elem[Type[(A,B)]]]
  }


  implicit def isoTuple2Type[A, B](implicit  eA: Elem[A],  eB: Elem[B]): Iso[Tuple2TypeData[A, B], Tuple2Type[A, B]]
    = new Tuple2Type.Iso[A, B] with SeqIso[Tuple2TypeData[A, B], Tuple2Type[A, B]] { i =>
        // should use i as iso reference
        override lazy val eTo = new SeqViewElem[Tuple2TypeData[A, B], Tuple2Type[A, B]]
                                    with Tuple2TypeElem[A, B] { val iso = i }
      }


  def mkTuple2Type[A, B]
      (tyA: Ty[A], tyB: Ty[B])
      (implicit  eA: Elem[A],  eB: Elem[B])
      = new SeqTuple2Type[A, B](tyA, tyB)
  def unmkTuple2Type[A, B](p: Rep[Tuple2Type[A, B]])
      (implicit  eA: Elem[A],  eB: Elem[B])
    = Some((p.tyA, p.tyB))

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


  case class ExpTuple2Type[A, B]
      (override val tyA: Ty[A], override val tyB: Ty[B])
      (implicit override val e1: Elem[A], override val e2: Elem[B])
    extends Tuple2Type[A, B](tyA, tyB) with Tuple2TypeOps[A,B]
       with UserTypeDef[Type[(A,B)],Tuple2Type[A, B]] {
    lazy val objType = element[Tuple2Type[A, B]]
    def elem = objType.asInstanceOf[Elem[Type[(A,B)]]]
    override def mirror(t: Transformer): Rep[_] = ExpTuple2Type[A, B](t(tyA), t(tyB))
  }
  addUserType(manifest[ExpTuple2Type[Any,Any]])


  def mkTuple2Type[A, B]
      (tyA: Ty[A], tyB: Ty[B])
      (implicit  eA: Elem[A],  eB: Elem[B])
      = new ExpTuple2Type[A, B](tyA, tyB)
  def unmkTuple2Type[A, B]
      (p: Rep[Tuple2Type[A, B]])
      (implicit  eA: Elem[A],  eB: Elem[B])
    = Some((p.tyA, p.tyB))


  implicit def isoTuple2Type[A, B](implicit  eA: Elem[A],  eB: Elem[B]): Iso[Tuple2TypeData[A, B], Tuple2Type[A, B]]
    = new Tuple2Type.Iso[A, B] with StagedIso[Tuple2TypeData[A, B], Tuple2Type[A, B]] { i =>
        // should use i as iso reference
        override lazy val eTo = new StagedViewElem[Tuple2TypeData[A, B], Tuple2Type[A, B]]
                                    with Tuple2TypeElem[A, B] { val iso = i }
      }

}
