package scalan.parrays
package impl

import scalan._
import scalan.arrays.ArrayOps
import scalan.common.Default
import scalan.common.OverloadHack.Overloaded1
import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.runtime.universe._
import scalan.common.Default

trait PArraysAbs extends PArrays
{ self: PArraysDsl =>
  // single proxy for each type family
  implicit def proxyPArray[A:Elem](p: Rep[PArray[A]]): PArray[A] =
    proxyOps[PArray[A]](p)

  abstract class PArrayElem[From,To](iso: Iso[From, To]) extends ViewElem[From, To]()(iso)

  trait PArrayCompanionElem extends CompanionElem[PArrayCompanionAbs]
  implicit lazy val PArrayCompanionElem: PArrayCompanionElem = new PArrayCompanionElem {
    lazy val tag = typeTag[PArrayCompanionAbs]
    lazy val defaultRep = Default.defaultVal(PArray)
  }

  trait PArrayCompanionAbs extends PArrayCompanion
  def PArray: Rep[PArrayCompanionAbs]
  implicit def proxyPArrayCompanion(p: Rep[PArrayCompanion]): PArrayCompanion = {
    proxyOps[PArrayCompanion](p)
  }

  // elem for concrete class
  class UnitArrayElem(iso: Iso[UnitArrayData, UnitArray]) extends PArrayElem[UnitArrayData, UnitArray](iso)

  // state representation type
  type UnitArrayData = Int

  // 3) Iso for concrete class
  class UnitArrayIso
    extends Iso[UnitArrayData, UnitArray] {
    override def from(p: Rep[UnitArray]) =
      unmkUnitArray(p) match {
        case Some((len)) => len
        case None => !!!
      }
    override def to(p: Rep[Int]) = {
      val len = p
      UnitArray(len)
    }
    lazy val tag = {

      typeTag[UnitArray]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[UnitArray]](UnitArray(0))
    lazy val eTo = new UnitArrayElem(this)
  }
  // 4) constructor and deconstructor
  trait UnitArrayCompanionAbs extends UnitArrayCompanion {

    def apply(len: Rep[Int]): Rep[UnitArray] =
      mkUnitArray(len)
    def unapply(p: Rep[UnitArray]) = unmkUnitArray(p)
  }
  def UnitArray: Rep[UnitArrayCompanionAbs]
  implicit def proxyUnitArrayCompanion(p: Rep[UnitArrayCompanionAbs]): UnitArrayCompanionAbs = {
    proxyOps[UnitArrayCompanionAbs](p)
  }

  trait UnitArrayCompanionElem extends CompanionElem[UnitArrayCompanionAbs]
  implicit lazy val UnitArrayCompanionElem: UnitArrayCompanionElem = new UnitArrayCompanionElem {
    lazy val tag = typeTag[UnitArrayCompanionAbs]
    lazy val defaultRep = Default.defaultVal(UnitArray)
  }

  implicit def proxyUnitArray(p: Rep[UnitArray]): UnitArray = {
    proxyOps[UnitArray](p)
  }

  implicit class ExtendedUnitArray(p: Rep[UnitArray]) {
    def toData: Rep[UnitArrayData] = isoUnitArray.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoUnitArray: Iso[UnitArrayData, UnitArray] =
    new UnitArrayIso

  // 6) smart constructor and deconstructor
  def mkUnitArray(len: Rep[Int]): Rep[UnitArray]
  def unmkUnitArray(p: Rep[UnitArray]): Option[(Rep[Int])]

  // elem for concrete class
  class BaseArrayElem[A](iso: Iso[BaseArrayData[A], BaseArray[A]]) extends PArrayElem[BaseArrayData[A], BaseArray[A]](iso)

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
    lazy val defaultRepTo = Default.defaultVal[Rep[BaseArray[A]]](BaseArray(element[Array[A]].defaultRepValue))
    lazy val eTo = new BaseArrayElem[A](this)
  }
  // 4) constructor and deconstructor
  trait BaseArrayCompanionAbs extends BaseArrayCompanion {

    def apply[A](arr: Rep[Array[A]])(implicit eA: Elem[A]): Rep[BaseArray[A]] =
      mkBaseArray(arr)
    def unapply[A:Elem](p: Rep[BaseArray[A]]) = unmkBaseArray(p)
  }
  def BaseArray: Rep[BaseArrayCompanionAbs]
  implicit def proxyBaseArrayCompanion(p: Rep[BaseArrayCompanionAbs]): BaseArrayCompanionAbs = {
    proxyOps[BaseArrayCompanionAbs](p)
  }

  trait BaseArrayCompanionElem extends CompanionElem[BaseArrayCompanionAbs]
  implicit lazy val BaseArrayCompanionElem: BaseArrayCompanionElem = new BaseArrayCompanionElem {
    lazy val tag = typeTag[BaseArrayCompanionAbs]
    lazy val defaultRep = Default.defaultVal(BaseArray)
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
  class PairArrayElem[A, B](iso: Iso[PairArrayData[A, B], PairArray[A, B]]) extends PArrayElem[PairArrayData[A, B], PairArray[A, B]](iso)

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
    lazy val defaultRepTo = Default.defaultVal[Rep[PairArray[A, B]]](PairArray(element[PArray[A]].defaultRepValue, element[PArray[B]].defaultRepValue))
    lazy val eTo = new PairArrayElem[A, B](this)
  }
  // 4) constructor and deconstructor
  trait PairArrayCompanionAbs extends PairArrayCompanion {
    def apply[A, B](p: Rep[PairArrayData[A, B]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairArray[A, B]] =
      isoPairArray(eA, eB).to(p)
    def apply[A, B](as: Rep[PArray[A]], bs: Rep[PArray[B]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairArray[A, B]] =
      mkPairArray(as, bs)
    def unapply[A:Elem, B:Elem](p: Rep[PairArray[A, B]]) = unmkPairArray(p)
  }
  def PairArray: Rep[PairArrayCompanionAbs]
  implicit def proxyPairArrayCompanion(p: Rep[PairArrayCompanionAbs]): PairArrayCompanionAbs = {
    proxyOps[PairArrayCompanionAbs](p)
  }

  trait PairArrayCompanionElem extends CompanionElem[PairArrayCompanionAbs]
  implicit lazy val PairArrayCompanionElem: PairArrayCompanionElem = new PairArrayCompanionElem {
    lazy val tag = typeTag[PairArrayCompanionAbs]
    lazy val defaultRep = Default.defaultVal(PairArray)
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
  class NestedArrayElem[A](iso: Iso[NestedArrayData[A], NestedArray[A]]) extends PArrayElem[NestedArrayData[A], NestedArray[A]](iso)

  // state representation type
  type NestedArrayData[A] = (PArray[A], PArray[(Int,Int)])

  // 3) Iso for concrete class
  class NestedArrayIso[A](implicit eA: Elem[A])
    extends Iso[NestedArrayData[A], NestedArray[A]] {
    override def from(p: Rep[NestedArray[A]]) =
      unmkNestedArray(p) match {
        case Some((values, segments)) => Pair(values, segments)
        case None => !!!
      }
    override def to(p: Rep[(PArray[A], PArray[(Int,Int)])]) = {
      val Pair(values, segments) = p
      NestedArray(values, segments)
    }
    lazy val tag = {
      implicit val tagA = element[A].tag
      typeTag[NestedArray[A]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[NestedArray[A]]](NestedArray(element[PArray[A]].defaultRepValue, element[PArray[(Int,Int)]].defaultRepValue))
    lazy val eTo = new NestedArrayElem[A](this)
  }
  // 4) constructor and deconstructor
  trait NestedArrayCompanionAbs extends NestedArrayCompanion {
    def apply[A](p: Rep[NestedArrayData[A]])(implicit eA: Elem[A]): Rep[NestedArray[A]] =
      isoNestedArray(eA).to(p)
    def apply[A](values: Rep[PArray[A]], segments: Rep[PArray[(Int,Int)]])(implicit eA: Elem[A]): Rep[NestedArray[A]] =
      mkNestedArray(values, segments)
    def unapply[A:Elem](p: Rep[NestedArray[A]]) = unmkNestedArray(p)
  }
  def NestedArray: Rep[NestedArrayCompanionAbs]
  implicit def proxyNestedArrayCompanion(p: Rep[NestedArrayCompanionAbs]): NestedArrayCompanionAbs = {
    proxyOps[NestedArrayCompanionAbs](p)
  }

  trait NestedArrayCompanionElem extends CompanionElem[NestedArrayCompanionAbs]
  implicit lazy val NestedArrayCompanionElem: NestedArrayCompanionElem = new NestedArrayCompanionElem {
    lazy val tag = typeTag[NestedArrayCompanionAbs]
    lazy val defaultRep = Default.defaultVal(NestedArray)
  }

  implicit def proxyNestedArray[A:Elem](p: Rep[NestedArray[A]]): NestedArray[A] = {
    proxyOps[NestedArray[A]](p)
  }

  implicit class ExtendedNestedArray[A](p: Rep[NestedArray[A]])(implicit eA: Elem[A]) {
    def toData: Rep[NestedArrayData[A]] = isoNestedArray(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoNestedArray[A](implicit eA: Elem[A]): Iso[NestedArrayData[A], NestedArray[A]] =
    new NestedArrayIso[A]

  // 6) smart constructor and deconstructor
  def mkNestedArray[A](values: Rep[PArray[A]], segments: Rep[PArray[(Int,Int)]])(implicit eA: Elem[A]): Rep[NestedArray[A]]
  def unmkNestedArray[A:Elem](p: Rep[NestedArray[A]]): Option[(Rep[PArray[A]], Rep[PArray[(Int,Int)]])]
}

trait PArraysSeq extends PArraysAbs { self: ScalanSeq with PArraysDsl =>
  lazy val PArray: Rep[PArrayCompanionAbs] = new PArrayCompanionAbs with UserTypeSeq[PArrayCompanionAbs, PArrayCompanionAbs] {
    lazy val selfType = element[PArrayCompanionAbs]
  }

  case class SeqUnitArray
      (override val len: Rep[Int])
      
    extends UnitArray(len) with UserTypeSeq[PArray[Unit], UnitArray] {
    lazy val selfType = element[UnitArray].asInstanceOf[Elem[PArray[Unit]]]
  }
  lazy val UnitArray = new UnitArrayCompanionAbs with UserTypeSeq[UnitArrayCompanionAbs, UnitArrayCompanionAbs] {
    lazy val selfType = element[UnitArrayCompanionAbs]
  }

  def mkUnitArray
      (len: Rep[Int]) =
      new SeqUnitArray(len)
  def unmkUnitArray(p: Rep[UnitArray]) =
    Some((p.len))

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

  case class SeqNestedArray[A]
      (override val values: Rep[PArray[A]], override val segments: Rep[PArray[(Int,Int)]])
      (implicit override val eA: Elem[A])
    extends NestedArray[A](values, segments) with UserTypeSeq[PArray[PArray[A]], NestedArray[A]] {
    lazy val selfType = element[NestedArray[A]].asInstanceOf[Elem[PArray[PArray[A]]]]
  }
  lazy val NestedArray = new NestedArrayCompanionAbs with UserTypeSeq[NestedArrayCompanionAbs, NestedArrayCompanionAbs] {
    lazy val selfType = element[NestedArrayCompanionAbs]
  }

  def mkNestedArray[A]
      (values: Rep[PArray[A]], segments: Rep[PArray[(Int,Int)]])(implicit eA: Elem[A]) =
      new SeqNestedArray[A](values, segments)
  def unmkNestedArray[A:Elem](p: Rep[NestedArray[A]]) =
    Some((p.values, p.segments))
}

trait PArraysExp extends PArraysAbs { self: ScalanExp with PArraysDsl =>
  lazy val PArray: Rep[PArrayCompanionAbs] = new PArrayCompanionAbs with UserTypeDef[PArrayCompanionAbs, PArrayCompanionAbs] {
    lazy val selfType = element[PArrayCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpUnitArray
      (override val len: Rep[Int])
      
    extends UnitArray(len) with UserTypeDef[PArray[Unit], UnitArray] {
    lazy val selfType = element[UnitArray].asInstanceOf[Elem[PArray[Unit]]]
    override def mirror(t: Transformer) = ExpUnitArray(t(len))
  }

  lazy val UnitArray: Rep[UnitArrayCompanionAbs] = new UnitArrayCompanionAbs with UserTypeDef[UnitArrayCompanionAbs, UnitArrayCompanionAbs] {
    lazy val selfType = element[UnitArrayCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  def mkUnitArray
    (len: Rep[Int]) =
    new ExpUnitArray(len)
  def unmkUnitArray(p: Rep[UnitArray]) =
    Some((p.len))

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

  case class ExpNestedArray[A]
      (override val values: Rep[PArray[A]], override val segments: Rep[PArray[(Int,Int)]])
      (implicit override val eA: Elem[A])
    extends NestedArray[A](values, segments) with UserTypeDef[PArray[PArray[A]], NestedArray[A]] {
    lazy val selfType = element[NestedArray[A]].asInstanceOf[Elem[PArray[PArray[A]]]]
    override def mirror(t: Transformer) = ExpNestedArray[A](t(values), t(segments))
  }

  lazy val NestedArray: Rep[NestedArrayCompanionAbs] = new NestedArrayCompanionAbs with UserTypeDef[NestedArrayCompanionAbs, NestedArrayCompanionAbs] {
    lazy val selfType = element[NestedArrayCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  def mkNestedArray[A]
    (values: Rep[PArray[A]], segments: Rep[PArray[(Int,Int)]])(implicit eA: Elem[A]) =
    new ExpNestedArray[A](values, segments)
  def unmkNestedArray[A:Elem](p: Rep[NestedArray[A]]) =
    Some((p.values, p.segments))

  object PArray_$init$ {
    def unapply(d: Def[_]): Option[PArray[A] forSome {type A}] = d match {
      case MethodCall(receiver, method, _) if method.getName == "$init$" && receiver.elem.isInstanceOf[PArrayElem[_, _]] =>
        Some(receiver).asInstanceOf[Option[PArray[A] forSome {type A}]]
      case _ => None
    }
    def unapply(exp: Exp[_]): Option[PArray[A] forSome {type A}] = exp match {
      case Def(d) => unapply(d)
      case _ => None
    }
  }

  object PArray_elem {
    def unapply(d: Def[_]): Option[PArray[A] forSome {type A}] = d match {
      case MethodCall(receiver, method, _) if method.getName == "elem" && receiver.elem.isInstanceOf[PArrayElem[_, _]] =>
        Some(receiver).asInstanceOf[Option[PArray[A] forSome {type A}]]
      case _ => None
    }
    def unapply(exp: Exp[_]): Option[PArray[A] forSome {type A}] = exp match {
      case Def(d) => unapply(d)
      case _ => None
    }
  }

  object PArray_length {
    def unapply(d: Def[_]): Option[PArray[A] forSome {type A}] = d match {
      case MethodCall(receiver, method, _) if method.getName == "length" && receiver.elem.isInstanceOf[PArrayElem[_, _]] =>
        Some(receiver).asInstanceOf[Option[PArray[A] forSome {type A}]]
      case _ => None
    }
    def unapply(exp: Exp[_]): Option[PArray[A] forSome {type A}] = exp match {
      case Def(d) => unapply(d)
      case _ => None
    }
  }

  object PArray_arr {
    def unapply(d: Def[_]): Option[PArray[A] forSome {type A}] = d match {
      case MethodCall(receiver, method, _) if method.getName == "arr" && receiver.elem.isInstanceOf[PArrayElem[_, _]] =>
        Some(receiver).asInstanceOf[Option[PArray[A] forSome {type A}]]
      case _ => None
    }
    def unapply(exp: Exp[_]): Option[PArray[A] forSome {type A}] = exp match {
      case Def(d) => unapply(d)
      case _ => None
    }
  }

  object PArray_apply {
    def unapply(d: Def[_]): Option[(PArray[A], Rep[Int]) forSome {type A}] = d match {
      case MethodCall(receiver, method, Seq(i, _*)) if method.getName == "apply" && receiver.elem.isInstanceOf[PArrayElem[_, _]] =>
        Some((receiver, i)).asInstanceOf[Option[(PArray[A], Rep[Int]) forSome {type A}]]
      case _ => None
    }
    def unapply(exp: Exp[_]): Option[(PArray[A], Rep[Int]) forSome {type A}] = exp match {
      case Def(d) => unapply(d)
      case _ => None
    }
  }

  object PArray_apply1 {
    def unapply(d: Def[_]): Option[(PArray[A], Arr[Int]) forSome {type A}] = d match {
      case MethodCall(receiver, method, Seq(indices, _*)) if method.getName == "apply" && receiver.elem.isInstanceOf[PArrayElem[_, _]] =>
        Some((receiver, indices)).asInstanceOf[Option[(PArray[A], Arr[Int]) forSome {type A}]]
      case _ => None
    }
    def unapply(exp: Exp[_]): Option[(PArray[A], Arr[Int]) forSome {type A}] = exp match {
      case Def(d) => unapply(d)
      case _ => None
    }
  }

  object PArray_map {
    def unapply(d: Def[_]): Option[(PArray[A], Rep[A] => Rep[B]) forSome {type A; type B}] = d match {
      case MethodCall(receiver, method, Seq(f, _*)) if method.getName == "map" && receiver.elem.isInstanceOf[PArrayElem[_, _]] =>
        Some((receiver, f)).asInstanceOf[Option[(PArray[A], Rep[A] => Rep[B]) forSome {type A; type B}]]
      case _ => None
    }
    def unapply(exp: Exp[_]): Option[(PArray[A], Rep[A] => Rep[B]) forSome {type A; type B}] = exp match {
      case Def(d) => unapply(d)
      case _ => None
    }
  }

  object PArray_mapBy {
    def unapply(d: Def[_]): Option[(PArray[A], Rep[A => B]) forSome {type A; type B}] = d match {
      case MethodCall(receiver, method, Seq(f, _*)) if method.getName == "mapBy" && receiver.elem.isInstanceOf[PArrayElem[_, _]] =>
        Some((receiver, f)).asInstanceOf[Option[(PArray[A], Rep[A => B]) forSome {type A; type B}]]
      case _ => None
    }
    def unapply(exp: Exp[_]): Option[(PArray[A], Rep[A => B]) forSome {type A; type B}] = exp match {
      case Def(d) => unapply(d)
      case _ => None
    }
  }

  object PArray_zip {
    def unapply(d: Def[_]): Option[(PArray[A], PA[B]) forSome {type A; type B}] = d match {
      case MethodCall(receiver, method, Seq(ys, _*)) if method.getName == "zip" && receiver.elem.isInstanceOf[PArrayElem[_, _]] =>
        Some((receiver, ys)).asInstanceOf[Option[(PArray[A], PA[B]) forSome {type A; type B}]]
      case _ => None
    }
    def unapply(exp: Exp[_]): Option[(PArray[A], PA[B]) forSome {type A; type B}] = exp match {
      case Def(d) => unapply(d)
      case _ => None
    }
  }

  object PArray_slice {
    def unapply(d: Def[_]): Option[(PArray[A], Rep[Int], Rep[Int]) forSome {type A}] = d match {
      case MethodCall(receiver, method, Seq(offset, length, _*)) if method.getName == "slice" && receiver.elem.isInstanceOf[PArrayElem[_, _]] =>
        Some((receiver, offset, length)).asInstanceOf[Option[(PArray[A], Rep[Int], Rep[Int]) forSome {type A}]]
      case _ => None
    }
    def unapply(exp: Exp[_]): Option[(PArray[A], Rep[Int], Rep[Int]) forSome {type A}] = exp match {
      case Def(d) => unapply(d)
      case _ => None
    }
  }

  object PArray_reduce {
    def unapply(d: Def[_]): Option[PArray[A] forSome {type A}] = d match {
      case MethodCall(receiver, method, _) if method.getName == "reduce" && receiver.elem.isInstanceOf[PArrayElem[_, _]] =>
        Some(receiver).asInstanceOf[Option[PArray[A] forSome {type A}]]
      case _ => None
    }
    def unapply(exp: Exp[_]): Option[PArray[A] forSome {type A}] = exp match {
      case Def(d) => unapply(d)
      case _ => None
    }
  }

  object PArray_scan {
    def unapply(d: Def[_]): Option[PArray[A] forSome {type A}] = d match {
      case MethodCall(receiver, method, _) if method.getName == "scan" && receiver.elem.isInstanceOf[PArrayElem[_, _]] =>
        Some(receiver).asInstanceOf[Option[PArray[A] forSome {type A}]]
      case _ => None
    }
    def unapply(exp: Exp[_]): Option[PArray[A] forSome {type A}] = exp match {
      case Def(d) => unapply(d)
      case _ => None
    }
  }
}
