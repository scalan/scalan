
package scalan.types
package impl

import scalan.common.Default
import scalan._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe._
import scalan.common.Default


trait TypesAbs extends Types
{ self: TypesDsl =>

  // single proxy for each type family
  implicit def proxyType[A:Elem](p: Rep[Type[A]]): Type[A] =
    proxyOps[Type[A]](p)

  trait TypeElem[From,To] extends ViewElem[From, To]

  trait TypeCompanionElem extends CompanionElem[TypeCompanionAbs]
  implicit lazy val TypeCompanionElem: TypeCompanionElem = new TypeCompanionElem {
    lazy val tag = typeTag[TypeCompanionAbs]
    lazy val defaultRep = Default.defaultVal(Type)
  }

  trait TypeCompanionAbs extends TypeCompanion
  def Type: Rep[TypeCompanionAbs]
  implicit def proxyTypeCompanion(p: Rep[TypeCompanion]): TypeCompanion = {
    proxyOps[TypeCompanion](p, true)
  }


  // elem for concrete class
  class BaseTypeElem[A](implicit iso: Iso[BaseTypeData[A], BaseType[A]]) extends TypeElem[BaseTypeData[A], BaseType[A]]

  // state representation type
  type BaseTypeData[A] = (String, A)

  // 3) Iso for concrete class
  class BaseTypeIso[A](implicit eA: Elem[A])
    extends Iso[BaseTypeData[A], BaseType[A]] {
    override def from(p: Rep[BaseType[A]]) =
      unmkBaseType(p) match {
        case Some((typeCode, defaultValue)) => Pair(typeCode, defaultValue)
        case None => !!!
      }
    override def to(p: Rep[(String, A)]) = {
      val Pair(typeCode, defaultValue) = p
      BaseType(typeCode, defaultValue)
    }
    lazy val tag = {
      implicit val tagA = element[A].tag
      typeTag[BaseType[A]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[BaseType[A]]](BaseType("", element[A].defaultRepValue))
    lazy val eTo = new BaseTypeElem[A]()(this)
  }
  // 4) constructor and deconstructor
  trait BaseTypeCompanionAbs extends BaseTypeCompanion {

    def apply[A](p: Rep[BaseTypeData[A]])(implicit eA: Elem[A]): Rep[BaseType[A]] =
      isoBaseType(eA).to(p)
    def apply[A](typeCode: Rep[String], defaultValue: Rep[A])(implicit eA: Elem[A]): Rep[BaseType[A]] =
      mkBaseType(typeCode, defaultValue)
    def unapply[A:Elem](p: Rep[BaseType[A]]) = unmkBaseType(p)
  }
  def BaseType: Rep[BaseTypeCompanionAbs]
  implicit def proxyBaseTypeCompanion(p: Rep[BaseTypeCompanionAbs]): BaseTypeCompanionAbs = {
    proxyOps[BaseTypeCompanionAbs](p, true)
  }

  trait BaseTypeCompanionElem extends CompanionElem[BaseTypeCompanionAbs]
  implicit lazy val BaseTypeCompanionElem: BaseTypeCompanionElem = new BaseTypeCompanionElem {
    lazy val tag = typeTag[BaseTypeCompanionAbs]
    lazy val defaultRep = Default.defaultVal(BaseType)
  }

  implicit def proxyBaseType[A:Elem](p: Rep[BaseType[A]]): BaseType[A] = {
    proxyOps[BaseType[A]](p)
  }

  implicit class ExtendedBaseType[A](p: Rep[BaseType[A]])(implicit eA: Elem[A]) {
    def toData: Rep[BaseTypeData[A]] = isoBaseType(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoBaseType[A](implicit eA: Elem[A]): Iso[BaseTypeData[A], BaseType[A]] =
    new BaseTypeIso[A]

  // 6) smart constructor and deconstructor
  def mkBaseType[A](typeCode: Rep[String], defaultValue: Rep[A])(implicit eA: Elem[A]): Rep[BaseType[A]]
  def unmkBaseType[A:Elem](p: Rep[BaseType[A]]): Option[(Rep[String], Rep[A])]


  // elem for concrete class
  class Tuple2TypeElem[A, B](implicit iso: Iso[Tuple2TypeData[A, B], Tuple2Type[A, B]]) extends TypeElem[Tuple2TypeData[A, B], Tuple2Type[A, B]]

  // state representation type
  type Tuple2TypeData[A, B] = (Type[A], Type[B])

  // 3) Iso for concrete class
  class Tuple2TypeIso[A, B](implicit e1: Elem[A], e2: Elem[B])
    extends Iso[Tuple2TypeData[A, B], Tuple2Type[A, B]] {
    override def from(p: Rep[Tuple2Type[A, B]]) =
      unmkTuple2Type(p) match {
        case Some((tyA, tyB)) => Pair(tyA, tyB)
        case None => !!!
      }
    override def to(p: Rep[(Type[A], Type[B])]) = {
      val Pair(tyA, tyB) = p
      Tuple2Type(tyA, tyB)
    }
    lazy val tag = {
      implicit val tagA = element[A].tag
      implicit val tagB = element[B].tag
      typeTag[Tuple2Type[A, B]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[Tuple2Type[A, B]]](Tuple2Type(element[Type[A]].defaultRepValue, element[Type[B]].defaultRepValue))
    lazy val eTo = new Tuple2TypeElem[A, B]()(this)
  }
  // 4) constructor and deconstructor
  trait Tuple2TypeCompanionAbs extends Tuple2TypeCompanion {

    def apply[A, B](p: Rep[Tuple2TypeData[A, B]])(implicit e1: Elem[A], e2: Elem[B]): Rep[Tuple2Type[A, B]] =
      isoTuple2Type(e1, e2).to(p)
    def apply[A, B](tyA: Rep[Type[A]], tyB: Rep[Type[B]])(implicit e1: Elem[A], e2: Elem[B]): Rep[Tuple2Type[A, B]] =
      mkTuple2Type(tyA, tyB)
    def unapply[A:Elem, B:Elem](p: Rep[Tuple2Type[A, B]]) = unmkTuple2Type(p)
  }
  def Tuple2Type: Rep[Tuple2TypeCompanionAbs]
  implicit def proxyTuple2TypeCompanion(p: Rep[Tuple2TypeCompanionAbs]): Tuple2TypeCompanionAbs = {
    proxyOps[Tuple2TypeCompanionAbs](p, true)
  }

  trait Tuple2TypeCompanionElem extends CompanionElem[Tuple2TypeCompanionAbs]
  implicit lazy val Tuple2TypeCompanionElem: Tuple2TypeCompanionElem = new Tuple2TypeCompanionElem {
    lazy val tag = typeTag[Tuple2TypeCompanionAbs]
    lazy val defaultRep = Default.defaultVal(Tuple2Type)
  }

  implicit def proxyTuple2Type[A:Elem, B:Elem](p: Rep[Tuple2Type[A, B]]): Tuple2Type[A, B] = {
    proxyOps[Tuple2Type[A, B]](p)
  }

  implicit class ExtendedTuple2Type[A, B](p: Rep[Tuple2Type[A, B]])(implicit e1: Elem[A], e2: Elem[B]) {
    def toData: Rep[Tuple2TypeData[A, B]] = isoTuple2Type(e1, e2).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoTuple2Type[A, B](implicit e1: Elem[A], e2: Elem[B]): Iso[Tuple2TypeData[A, B], Tuple2Type[A, B]] =
    new Tuple2TypeIso[A, B]

  // 6) smart constructor and deconstructor
  def mkTuple2Type[A, B](tyA: Rep[Type[A]], tyB: Rep[Type[B]])(implicit e1: Elem[A], e2: Elem[B]): Rep[Tuple2Type[A, B]]
  def unmkTuple2Type[A:Elem, B:Elem](p: Rep[Tuple2Type[A, B]]): Option[(Rep[Type[A]], Rep[Type[B]])]

}


trait TypesSeq extends TypesAbs { self: ScalanSeq with TypesDsl =>
  lazy val Type: Rep[TypeCompanionAbs] = new TypeCompanionAbs with UserTypeSeq[TypeCompanionAbs, TypeCompanionAbs] {
    lazy val selfType = element[TypeCompanionAbs]
  }

  case class SeqBaseType[A]
      (override val typeCode: Rep[String], override val defaultValue: Rep[A])
      (implicit override val eA: Elem[A])
    extends BaseType[A](typeCode, defaultValue) with UserTypeSeq[Type[A], BaseType[A]] {
    lazy val selfType = element[BaseType[A]].asInstanceOf[Elem[Type[A]]]
  }
  lazy val BaseType = new BaseTypeCompanionAbs with UserTypeSeq[BaseTypeCompanionAbs, BaseTypeCompanionAbs] {
    lazy val selfType = element[BaseTypeCompanionAbs]
  }

  def mkBaseType[A]
      (typeCode: Rep[String], defaultValue: Rep[A])(implicit eA: Elem[A]) =
      new SeqBaseType[A](typeCode, defaultValue)
  def unmkBaseType[A:Elem](p: Rep[BaseType[A]]) =
    Some((p.typeCode, p.defaultValue))


  case class SeqTuple2Type[A, B]
      (override val tyA: Rep[Type[A]], override val tyB: Rep[Type[B]])
      (implicit override val e1: Elem[A], override val e2: Elem[B])
    extends Tuple2Type[A, B](tyA, tyB) with UserTypeSeq[Type[(A,B)], Tuple2Type[A, B]] {
    lazy val selfType = element[Tuple2Type[A, B]].asInstanceOf[Elem[Type[(A,B)]]]
  }
  lazy val Tuple2Type = new Tuple2TypeCompanionAbs with UserTypeSeq[Tuple2TypeCompanionAbs, Tuple2TypeCompanionAbs] {
    lazy val selfType = element[Tuple2TypeCompanionAbs]
  }

  def mkTuple2Type[A, B]
      (tyA: Rep[Type[A]], tyB: Rep[Type[B]])(implicit e1: Elem[A], e2: Elem[B]) =
      new SeqTuple2Type[A, B](tyA, tyB)
  def unmkTuple2Type[A:Elem, B:Elem](p: Rep[Tuple2Type[A, B]]) =
    Some((p.tyA, p.tyB))

}


trait TypesExp extends TypesAbs { self: ScalanStaged with TypesDsl =>
  lazy val Type: Rep[TypeCompanionAbs] = new TypeCompanionAbs with UserTypeDef[TypeCompanionAbs, TypeCompanionAbs] {
    lazy val selfType = element[TypeCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpBaseType[A]
      (override val typeCode: Rep[String], override val defaultValue: Rep[A])
      (implicit override val eA: Elem[A])
    extends BaseType[A](typeCode, defaultValue) with UserTypeDef[Type[A], BaseType[A]] {
    lazy val selfType = element[BaseType[A]].asInstanceOf[Elem[Type[A]]]
    override def mirror(t: Transformer) = ExpBaseType[A](t(typeCode), t(defaultValue))
  }

  lazy val BaseType: Rep[BaseTypeCompanionAbs] = new BaseTypeCompanionAbs with UserTypeDef[BaseTypeCompanionAbs, BaseTypeCompanionAbs] {
    lazy val selfType = element[BaseTypeCompanionAbs]
    override def mirror(t: Transformer) = this
  }


  def mkBaseType[A]
    (typeCode: Rep[String], defaultValue: Rep[A])(implicit eA: Elem[A]) =
    new ExpBaseType[A](typeCode, defaultValue)
  def unmkBaseType[A:Elem](p: Rep[BaseType[A]]) =
    Some((p.typeCode, p.defaultValue))


  case class ExpTuple2Type[A, B]
      (override val tyA: Rep[Type[A]], override val tyB: Rep[Type[B]])
      (implicit override val e1: Elem[A], override val e2: Elem[B])
    extends Tuple2Type[A, B](tyA, tyB) with UserTypeDef[Type[(A,B)], Tuple2Type[A, B]] {
    lazy val selfType = element[Tuple2Type[A, B]].asInstanceOf[Elem[Type[(A,B)]]]
    override def mirror(t: Transformer) = ExpTuple2Type[A, B](t(tyA), t(tyB))
  }

  lazy val Tuple2Type: Rep[Tuple2TypeCompanionAbs] = new Tuple2TypeCompanionAbs with UserTypeDef[Tuple2TypeCompanionAbs, Tuple2TypeCompanionAbs] {
    lazy val selfType = element[Tuple2TypeCompanionAbs]
    override def mirror(t: Transformer) = this
  }


  def mkTuple2Type[A, B]
    (tyA: Rep[Type[A]], tyB: Rep[Type[B]])(implicit e1: Elem[A], e2: Elem[B]) =
    new ExpTuple2Type[A, B](tyA, tyB)
  def unmkTuple2Type[A:Elem, B:Elem](p: Rep[Tuple2Type[A, B]]) =
    Some((p.tyA, p.tyB))

}
