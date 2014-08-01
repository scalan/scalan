
package scalan.arrays
package impl

import scalan._
import scalan.common.Default
import scalan.common.OverloadHack.Overloaded1
import scala.annotation.unchecked.uncheckedVariance
import scalan.community.{ScalanCommunity, ScalanCommunityStaged, ScalanCommunitySeq}
import scala.reflect.runtime.universe._
import scalan.common.Default.defaultVal


trait PArraysAbs extends PArrays
{ self: PArraysDsl =>

  // single proxy for each type family
  implicit def proxyPArray[A:Elem](p: PA[A]): PArray[A] = {
    proxyOps[PArray[A]](p)
  }

  trait PArrayElem[From,To] extends ViewElem[From, To]

  trait PArrayCompanionElem extends CompanionElem[PArrayCompanionAbs]
  implicit lazy val PArrayCompanionElem: PArrayCompanionElem = new PArrayCompanionElem {
    lazy val tag = typeTag[PArrayCompanionAbs]
    lazy val defaultRep = defaultVal(PArray)
  }

  trait PArrayCompanionAbs extends PArrayCompanion
  def PArray: Rep[PArrayCompanionAbs]
  implicit def defaultOfPArray[A:Elem]: Default[Rep[PArray[A]]] = PArray.defaultOf[A]
  implicit def proxyPArrayCompanion(p: Rep[PArrayCompanion]): PArrayCompanion = {
    proxyOps[PArrayCompanion](p, true)
  }


  // elem for concrete class
  class BaseArrayElem[A](implicit iso: Iso[BaseArrayData[A], BaseArray[A]]) extends PArrayElem[BaseArrayData[A], BaseArray[A]]

  // state representation type
  type BaseArrayData[A] = Array[A]

  // 3) Iso for concrete class
  class BaseArrayIso[A](implicit eA: Elem[A])
    extends Iso[BaseArrayData[A], BaseArray[A]] {
    override def from(p: Rep[BaseArray[A]]) =
      unmkBaseArray(p) match {
        case Some((arr)) => arr
        case None => !!!
      }
    override def to(p: Rep[Array[A]]) = {
      val arr = p
      BaseArray(arr)
    }
    lazy val tag = {
      implicit val tagA = element[A].tag
      typeTag[BaseArray[A]]
    }
    lazy val defaultRepTo = defaultVal[Rep[BaseArray[A]]](BaseArray(element[Array[A]].defaultRepValue))
    lazy val eTo = new BaseArrayElem[A]()(this)
  }
  // 4) constructor and deconstructor
  trait BaseArrayCompanionAbs extends BaseArrayCompanion {

    def apply[A]
          (arr: Rep[Array[A]])(implicit eA: Elem[A]): Rep[BaseArray[A]] =
      mkBaseArray(arr)
    def unapply[A:Elem](p: Rep[BaseArray[A]]) = unmkBaseArray(p)
  }

  def BaseArray: Rep[BaseArrayCompanionAbs]
  implicit def proxyBaseArrayCompanion(p: Rep[BaseArrayCompanionAbs]): BaseArrayCompanionAbs = {
    proxyOps[BaseArrayCompanionAbs](p, true)
  }

  trait BaseArrayCompanionElem extends CompanionElem[BaseArrayCompanionAbs]
  implicit lazy val BaseArrayCompanionElem: BaseArrayCompanionElem = new BaseArrayCompanionElem {
    lazy val tag = typeTag[BaseArrayCompanionAbs]
    lazy val defaultRep = defaultVal(BaseArray)
  }

  implicit def proxyBaseArray[A:Elem](p: Rep[BaseArray[A]]): BaseArray[A] = {
    proxyOps[BaseArray[A]](p)
  }

  implicit class ExtendedBaseArray[A](p: Rep[BaseArray[A]])(implicit eA: Elem[A]) {
    def toData: Rep[BaseArrayData[A]] = isoBaseArray(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoBaseArray[A](implicit eA: Elem[A]): Iso[BaseArrayData[A], BaseArray[A]] =
    new BaseArrayIso[A]

  // 6) smart constructor and deconstructor
  def mkBaseArray[A](arr: Rep[Array[A]])(implicit eA: Elem[A]): Rep[BaseArray[A]]
  def unmkBaseArray[A:Elem](p: Rep[BaseArray[A]]): Option[(Rep[Array[A]])]


  // elem for concrete class
  class PairArrayElem[A, B](implicit iso: Iso[PairArrayData[A, B], PairArray[A, B]]) extends PArrayElem[PairArrayData[A, B], PairArray[A, B]]

  // state representation type
  type PairArrayData[A, B] = (PArray[A], PArray[B])

  // 3) Iso for concrete class
  class PairArrayIso[A, B](implicit eA: Elem[A], eB: Elem[B])
    extends Iso[PairArrayData[A, B], PairArray[A, B]] {
    override def from(p: Rep[PairArray[A, B]]) =
      unmkPairArray(p) match {
        case Some((as, bs)) => Pair(as, bs)
        case None => !!!
      }
    override def to(p: Rep[(PArray[A], PArray[B])]) = {
      val Pair(as, bs) = p
      PairArray(as, bs)
    }
    lazy val tag = {
      implicit val tagA = element[A].tag
      implicit val tagB = element[B].tag
      typeTag[PairArray[A, B]]
    }
    lazy val defaultRepTo = defaultVal[Rep[PairArray[A, B]]](PairArray(element[PArray[A]].defaultRepValue, element[PArray[B]].defaultRepValue))
    lazy val eTo = new PairArrayElem[A, B]()(this)
  }
  // 4) constructor and deconstructor
  trait PairArrayCompanionAbs extends PairArrayCompanion {

    def apply[A, B](p: Rep[PairArrayData[A, B]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairArray[A, B]] =
      isoPairArray(eA, eB).to(p)
    def apply[A, B]
          (as: Rep[PArray[A]], bs: Rep[PArray[B]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairArray[A, B]] =
      mkPairArray(as, bs)
    def unapply[A:Elem, B:Elem](p: Rep[PairArray[A, B]]) = unmkPairArray(p)
  }

  def PairArray: Rep[PairArrayCompanionAbs]
  implicit def proxyPairArrayCompanion(p: Rep[PairArrayCompanionAbs]): PairArrayCompanionAbs = {
    proxyOps[PairArrayCompanionAbs](p, true)
  }

  trait PairArrayCompanionElem extends CompanionElem[PairArrayCompanionAbs]
  implicit lazy val PairArrayCompanionElem: PairArrayCompanionElem = new PairArrayCompanionElem {
    lazy val tag = typeTag[PairArrayCompanionAbs]
    lazy val defaultRep = defaultVal(PairArray)
  }

  implicit def proxyPairArray[A:Elem, B:Elem](p: Rep[PairArray[A, B]]): PairArray[A, B] = {
    proxyOps[PairArray[A, B]](p)
  }

  implicit class ExtendedPairArray[A, B](p: Rep[PairArray[A, B]])(implicit eA: Elem[A], eB: Elem[B]) {
    def toData: Rep[PairArrayData[A, B]] = isoPairArray(eA, eB).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoPairArray[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[PairArrayData[A, B], PairArray[A, B]] =
    new PairArrayIso[A, B]

  // 6) smart constructor and deconstructor
  def mkPairArray[A, B](as: Rep[PArray[A]], bs: Rep[PArray[B]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairArray[A, B]]
  def unmkPairArray[A:Elem, B:Elem](p: Rep[PairArray[A, B]]): Option[(Rep[PArray[A]], Rep[PArray[B]])]


  // elem for concrete class
  class FlatNestedArrayElem[A](implicit iso: Iso[FlatNestedArrayData[A], FlatNestedArray[A]]) extends PArrayElem[FlatNestedArrayData[A], FlatNestedArray[A]]

  // state representation type
  type FlatNestedArrayData[A] = (PArray[A], PArray[(Int,Int)])

  // 3) Iso for concrete class
  class FlatNestedArrayIso[A](implicit eA: Elem[A])
    extends Iso[FlatNestedArrayData[A], FlatNestedArray[A]] {
    override def from(p: Rep[FlatNestedArray[A]]) =
      unmkFlatNestedArray(p) match {
        case Some((values, segments)) => Pair(values, segments)
        case None => !!!
      }
    override def to(p: Rep[(PArray[A], PArray[(Int,Int)])]) = {
      val Pair(values, segments) = p
      FlatNestedArray(values, segments)
    }
    lazy val tag = {
      implicit val tagA = element[A].tag
      typeTag[FlatNestedArray[A]]
    }
    lazy val defaultRepTo = defaultVal[Rep[FlatNestedArray[A]]](FlatNestedArray(element[PArray[A]].defaultRepValue, element[PArray[(Int,Int)]].defaultRepValue))
    lazy val eTo = new FlatNestedArrayElem[A]()(this)
  }
  // 4) constructor and deconstructor
  trait FlatNestedArrayCompanionAbs extends FlatNestedArrayCompanion {

    def apply[A](p: Rep[FlatNestedArrayData[A]])(implicit eA: Elem[A]): Rep[FlatNestedArray[A]] =
      isoFlatNestedArray(eA).to(p)
    def apply[A]
          (values: Rep[PArray[A]], segments: Rep[PArray[(Int,Int)]])(implicit eA: Elem[A]): Rep[FlatNestedArray[A]] =
      mkFlatNestedArray(values, segments)
    def unapply[A:Elem](p: Rep[FlatNestedArray[A]]) = unmkFlatNestedArray(p)
  }

  def FlatNestedArray: Rep[FlatNestedArrayCompanionAbs]
  implicit def proxyFlatNestedArrayCompanion(p: Rep[FlatNestedArrayCompanionAbs]): FlatNestedArrayCompanionAbs = {
    proxyOps[FlatNestedArrayCompanionAbs](p, true)
  }

  trait FlatNestedArrayCompanionElem extends CompanionElem[FlatNestedArrayCompanionAbs]
  implicit lazy val FlatNestedArrayCompanionElem: FlatNestedArrayCompanionElem = new FlatNestedArrayCompanionElem {
    lazy val tag = typeTag[FlatNestedArrayCompanionAbs]
    lazy val defaultRep = defaultVal(FlatNestedArray)
  }

  implicit def proxyFlatNestedArray[A:Elem](p: Rep[FlatNestedArray[A]]): FlatNestedArray[A] = {
    proxyOps[FlatNestedArray[A]](p)
  }

  implicit class ExtendedFlatNestedArray[A](p: Rep[FlatNestedArray[A]])(implicit eA: Elem[A]) {
    def toData: Rep[FlatNestedArrayData[A]] = isoFlatNestedArray(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoFlatNestedArray[A](implicit eA: Elem[A]): Iso[FlatNestedArrayData[A], FlatNestedArray[A]] =
    new FlatNestedArrayIso[A]

  // 6) smart constructor and deconstructor
  def mkFlatNestedArray[A](values: Rep[PArray[A]], segments: Rep[PArray[(Int,Int)]])(implicit eA: Elem[A]): Rep[FlatNestedArray[A]]
  def unmkFlatNestedArray[A:Elem](p: Rep[FlatNestedArray[A]]): Option[(Rep[PArray[A]], Rep[PArray[(Int,Int)]])]

}


trait PArraysSeq extends PArraysAbs { self: ScalanSeq with PArraysDsl =>

  lazy val PArray: Rep[PArrayCompanionAbs] = new PArrayCompanionAbs with UserTypeSeq[PArrayCompanionAbs, PArrayCompanionAbs] {
    lazy val selfType = element[PArrayCompanionAbs]
  }

  case class SeqBaseArray[A]
      (override val arr: Rep[Array[A]])
      (implicit override val eA: Elem[A])
    extends BaseArray[A](arr) with UserTypeSeq[PArray[A], BaseArray[A]] {
    lazy val selfType = element[BaseArray[A]].asInstanceOf[Elem[PArray[A]]]
  }

  lazy val BaseArray = new BaseArrayCompanionAbs with UserTypeSeq[BaseArrayCompanionAbs, BaseArrayCompanionAbs] {
    lazy val selfType = element[BaseArrayCompanionAbs]
  }



  def mkBaseArray[A]
      (arr: Rep[Array[A]])(implicit eA: Elem[A]) =
      new SeqBaseArray[A](arr)
  def unmkBaseArray[A:Elem](p: Rep[BaseArray[A]]) =
    Some((p.arr))


  case class SeqPairArray[A, B]
      (override val as: Rep[PArray[A]], override val bs: Rep[PArray[B]])
      (implicit override val eA: Elem[A], override val eB: Elem[B])
    extends PairArray[A, B](as, bs) with UserTypeSeq[PArray[(A,B)], PairArray[A, B]] {
    lazy val selfType = element[PairArray[A, B]].asInstanceOf[Elem[PArray[(A,B)]]]
  }

  lazy val PairArray = new PairArrayCompanionAbs with UserTypeSeq[PairArrayCompanionAbs, PairArrayCompanionAbs] {
    lazy val selfType = element[PairArrayCompanionAbs]
  }



  def mkPairArray[A, B]
      (as: Rep[PArray[A]], bs: Rep[PArray[B]])(implicit eA: Elem[A], eB: Elem[B]) =
      new SeqPairArray[A, B](as, bs)
  def unmkPairArray[A:Elem, B:Elem](p: Rep[PairArray[A, B]]) =
    Some((p.as, p.bs))


  case class SeqFlatNestedArray[A]
      (override val values: Rep[PArray[A]], override val segments: Rep[PArray[(Int,Int)]])
      (implicit override val eA: Elem[A])
    extends FlatNestedArray[A](values, segments) with UserTypeSeq[PArray[PArray[A]], FlatNestedArray[A]] {
    lazy val selfType = element[FlatNestedArray[A]].asInstanceOf[Elem[PArray[PArray[A]]]]
  }

  lazy val FlatNestedArray = new FlatNestedArrayCompanionAbs with UserTypeSeq[FlatNestedArrayCompanionAbs, FlatNestedArrayCompanionAbs] {
    lazy val selfType = element[FlatNestedArrayCompanionAbs]
  }



  def mkFlatNestedArray[A]
      (values: Rep[PArray[A]], segments: Rep[PArray[(Int,Int)]])(implicit eA: Elem[A]) =
      new SeqFlatNestedArray[A](values, segments)
  def unmkFlatNestedArray[A:Elem](p: Rep[FlatNestedArray[A]]) =
    Some((p.values, p.segments))

}


trait PArraysExp extends PArraysAbs with scalan.ProxyExp with scalan.ViewsExp { self: ScalanStaged with PArraysDsl =>

  lazy val PArray: Rep[PArrayCompanionAbs] = new PArrayCompanionAbs with UserTypeDef[PArrayCompanionAbs, PArrayCompanionAbs] {
    lazy val selfType = element[PArrayCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpBaseArray[A]
      (override val arr: Rep[Array[A]])
      (implicit override val eA: Elem[A])
    extends BaseArray[A](arr) with UserTypeDef[PArray[A], BaseArray[A]] {
    lazy val selfType = element[BaseArray[A]].asInstanceOf[Elem[PArray[A]]]
    override def mirror(t: Transformer) = ExpBaseArray[A](t(arr))
  }

  lazy val BaseArray: Rep[BaseArrayCompanionAbs] = new BaseArrayCompanionAbs with UserTypeDef[BaseArrayCompanionAbs, BaseArrayCompanionAbs] {
    lazy val selfType = element[BaseArrayCompanionAbs]
    override def mirror(t: Transformer) = this
  }



  def mkBaseArray[A]
    (arr: Rep[Array[A]])(implicit eA: Elem[A]) =
    new ExpBaseArray[A](arr)
  def unmkBaseArray[A:Elem](p: Rep[BaseArray[A]]) =
    Some((p.arr))


  case class ExpPairArray[A, B]
      (override val as: Rep[PArray[A]], override val bs: Rep[PArray[B]])
      (implicit override val eA: Elem[A], override val eB: Elem[B])
    extends PairArray[A, B](as, bs) with UserTypeDef[PArray[(A,B)], PairArray[A, B]] {
    lazy val selfType = element[PairArray[A, B]].asInstanceOf[Elem[PArray[(A,B)]]]
    override def mirror(t: Transformer) = ExpPairArray[A, B](t(as), t(bs))
  }

  lazy val PairArray: Rep[PairArrayCompanionAbs] = new PairArrayCompanionAbs with UserTypeDef[PairArrayCompanionAbs, PairArrayCompanionAbs] {
    lazy val selfType = element[PairArrayCompanionAbs]
    override def mirror(t: Transformer) = this
  }



  def mkPairArray[A, B]
    (as: Rep[PArray[A]], bs: Rep[PArray[B]])(implicit eA: Elem[A], eB: Elem[B]) =
    new ExpPairArray[A, B](as, bs)
  def unmkPairArray[A:Elem, B:Elem](p: Rep[PairArray[A, B]]) =
    Some((p.as, p.bs))


  case class ExpFlatNestedArray[A]
      (override val values: Rep[PArray[A]], override val segments: Rep[PArray[(Int,Int)]])
      (implicit override val eA: Elem[A])
    extends FlatNestedArray[A](values, segments) with UserTypeDef[PArray[PArray[A]], FlatNestedArray[A]] {
    lazy val selfType = element[FlatNestedArray[A]].asInstanceOf[Elem[PArray[PArray[A]]]]
    override def mirror(t: Transformer) = ExpFlatNestedArray[A](t(values), t(segments))
  }

  lazy val FlatNestedArray: Rep[FlatNestedArrayCompanionAbs] = new FlatNestedArrayCompanionAbs with UserTypeDef[FlatNestedArrayCompanionAbs, FlatNestedArrayCompanionAbs] {
    lazy val selfType = element[FlatNestedArrayCompanionAbs]
    override def mirror(t: Transformer) = this
  }



  def mkFlatNestedArray[A]
    (values: Rep[PArray[A]], segments: Rep[PArray[(Int,Int)]])(implicit eA: Elem[A]) =
    new ExpFlatNestedArray[A](values, segments)
  def unmkFlatNestedArray[A:Elem](p: Rep[FlatNestedArray[A]]) =
    Some((p.values, p.segments))

}
