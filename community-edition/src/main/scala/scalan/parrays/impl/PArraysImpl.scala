package scalan.parrays
package impl

import scalan._
import scalan.arrays.ArrayOps
import scalan.common.Default
import scalan.common.OverloadHack.Overloaded1
import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.runtime.universe._
import scalan.common.Default

// Abs -----------------------------------
trait PArraysAbs extends Scalan with PArrays
{ self: PArraysDsl =>
  // single proxy for each type family
  implicit def proxyPArray[A](p: Rep[PArray[A]]): PArray[A] =
    proxyOps[PArray[A]](p)



  abstract class PArrayElem[A, From, To <: PArray[A]](iso: Iso[From, To]) extends ViewElem[From, To]()(iso)

  trait PArrayCompanionElem extends CompanionElem[PArrayCompanionAbs]
  implicit lazy val PArrayCompanionElem: PArrayCompanionElem = new PArrayCompanionElem {
    lazy val tag = typeTag[PArrayCompanionAbs]
    protected def getDefaultRep = PArray
  }

  abstract class PArrayCompanionAbs extends CompanionBase[PArrayCompanionAbs] with PArrayCompanion {
    override def toString = "PArray"
    
  }
  def PArray: Rep[PArrayCompanionAbs]
  implicit def proxyPArrayCompanion(p: Rep[PArrayCompanion]): PArrayCompanion = {
    proxyOps[PArrayCompanion](p)
  }

  //default wrapper implementation
  
  // elem for concrete class
  class UnitArrayElem(iso: Iso[UnitArrayData, UnitArray]) extends PArrayElem[Unit, UnitArrayData, UnitArray](iso)

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
      weakTypeTag[UnitArray]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[UnitArray]](UnitArray(0))
    lazy val eTo = new UnitArrayElem(this)
  }
  // 4) constructor and deconstructor
  abstract class UnitArrayCompanionAbs extends CompanionBase[UnitArrayCompanionAbs] with UnitArrayCompanion {
    override def toString = "UnitArray"

    def apply(len: Rep[Int]): Rep[UnitArray] =
      mkUnitArray(len)
    def unapply(p: Rep[UnitArray]) = unmkUnitArray(p)
  }
  def UnitArray: Rep[UnitArrayCompanionAbs]
  implicit def proxyUnitArrayCompanion(p: Rep[UnitArrayCompanionAbs]): UnitArrayCompanionAbs = {
    proxyOps[UnitArrayCompanionAbs](p)
  }

  class UnitArrayCompanionElem extends CompanionElem[UnitArrayCompanionAbs] {
    lazy val tag = typeTag[UnitArrayCompanionAbs]
    protected def getDefaultRep = UnitArray
  }
  implicit lazy val UnitArrayCompanionElem: UnitArrayCompanionElem = new UnitArrayCompanionElem

  implicit def proxyUnitArray(p: Rep[UnitArray]): UnitArray =
    proxyOps[UnitArray](p)

  implicit class ExtendedUnitArray(p: Rep[UnitArray]) {
    def toData: Rep[UnitArrayData] = isoUnitArray.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoUnitArray: Iso[UnitArrayData, UnitArray] =
    new UnitArrayIso

  // 6) smart constructor and deconstructor
  def mkUnitArray(len: Rep[Int]): Rep[UnitArray]
  def unmkUnitArray(p: Rep[UnitArray]): Option[(Rep[Int])]

  //default wrapper implementation
  
  // elem for concrete class
  class BaseArrayElem[A](iso: Iso[BaseArrayData[A], BaseArray[A]]) extends PArrayElem[A, BaseArrayData[A], BaseArray[A]](iso)

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
      weakTypeTag[BaseArray[A]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[BaseArray[A]]](BaseArray(element[Array[A]].defaultRepValue))
    lazy val eTo = new BaseArrayElem[A](this)
  }
  // 4) constructor and deconstructor
  abstract class BaseArrayCompanionAbs extends CompanionBase[BaseArrayCompanionAbs] with BaseArrayCompanion {
    override def toString = "BaseArray"

    def apply[A](arr: Rep[Array[A]])(implicit eA: Elem[A]): Rep[BaseArray[A]] =
      mkBaseArray(arr)
    def unapply[A:Elem](p: Rep[BaseArray[A]]) = unmkBaseArray(p)
  }
  def BaseArray: Rep[BaseArrayCompanionAbs]
  implicit def proxyBaseArrayCompanion(p: Rep[BaseArrayCompanionAbs]): BaseArrayCompanionAbs = {
    proxyOps[BaseArrayCompanionAbs](p)
  }

  class BaseArrayCompanionElem extends CompanionElem[BaseArrayCompanionAbs] {
    lazy val tag = typeTag[BaseArrayCompanionAbs]
    protected def getDefaultRep = BaseArray
  }
  implicit lazy val BaseArrayCompanionElem: BaseArrayCompanionElem = new BaseArrayCompanionElem

  implicit def proxyBaseArray[A](p: Rep[BaseArray[A]]): BaseArray[A] =
    proxyOps[BaseArray[A]](p)

  implicit class ExtendedBaseArray[A](p: Rep[BaseArray[A]])(implicit eA: Elem[A]) {
    def toData: Rep[BaseArrayData[A]] = isoBaseArray(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoBaseArray[A](implicit eA: Elem[A]): Iso[BaseArrayData[A], BaseArray[A]] =
    new BaseArrayIso[A]

  // 6) smart constructor and deconstructor
  def mkBaseArray[A](arr: Rep[Array[A]])(implicit eA: Elem[A]): Rep[BaseArray[A]]
  def unmkBaseArray[A:Elem](p: Rep[BaseArray[A]]): Option[(Rep[Array[A]])]

  //default wrapper implementation
  
  // elem for concrete class
  class PairArrayElem[A, B](iso: Iso[PairArrayData[A, B], PairArray[A, B]]) extends PArrayElem[(A,B), PairArrayData[A, B], PairArray[A, B]](iso)

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
      weakTypeTag[PairArray[A, B]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[PairArray[A, B]]](PairArray(element[PArray[A]].defaultRepValue, element[PArray[B]].defaultRepValue))
    lazy val eTo = new PairArrayElem[A, B](this)
  }
  // 4) constructor and deconstructor
  abstract class PairArrayCompanionAbs extends CompanionBase[PairArrayCompanionAbs] with PairArrayCompanion {
    override def toString = "PairArray"
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

  class PairArrayCompanionElem extends CompanionElem[PairArrayCompanionAbs] {
    lazy val tag = typeTag[PairArrayCompanionAbs]
    protected def getDefaultRep = PairArray
  }
  implicit lazy val PairArrayCompanionElem: PairArrayCompanionElem = new PairArrayCompanionElem

  implicit def proxyPairArray[A, B](p: Rep[PairArray[A, B]]): PairArray[A, B] =
    proxyOps[PairArray[A, B]](p)

  implicit class ExtendedPairArray[A, B](p: Rep[PairArray[A, B]])(implicit eA: Elem[A], eB: Elem[B]) {
    def toData: Rep[PairArrayData[A, B]] = isoPairArray(eA, eB).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoPairArray[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[PairArrayData[A, B], PairArray[A, B]] =
    new PairArrayIso[A, B]

  // 6) smart constructor and deconstructor
  def mkPairArray[A, B](as: Rep[PArray[A]], bs: Rep[PArray[B]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairArray[A, B]]
  def unmkPairArray[A:Elem, B:Elem](p: Rep[PairArray[A, B]]): Option[(Rep[PArray[A]], Rep[PArray[B]])]

  //default wrapper implementation
  
  // elem for concrete class
  class NestedArrayElem[A](iso: Iso[NestedArrayData[A], NestedArray[A]]) extends PArrayElem[PArray[A], NestedArrayData[A], NestedArray[A]](iso)

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
      weakTypeTag[NestedArray[A]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[NestedArray[A]]](NestedArray(element[PArray[A]].defaultRepValue, element[PArray[(Int,Int)]].defaultRepValue))
    lazy val eTo = new NestedArrayElem[A](this)
  }
  // 4) constructor and deconstructor
  abstract class NestedArrayCompanionAbs extends CompanionBase[NestedArrayCompanionAbs] with NestedArrayCompanion {
    override def toString = "NestedArray"
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

  class NestedArrayCompanionElem extends CompanionElem[NestedArrayCompanionAbs] {
    lazy val tag = typeTag[NestedArrayCompanionAbs]
    protected def getDefaultRep = NestedArray
  }
  implicit lazy val NestedArrayCompanionElem: NestedArrayCompanionElem = new NestedArrayCompanionElem

  implicit def proxyNestedArray[A](p: Rep[NestedArray[A]]): NestedArray[A] =
    proxyOps[NestedArray[A]](p)

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

// Seq -----------------------------------
trait PArraysSeq extends PArraysAbs with PArraysDsl with ScalanSeq
{ self: PArraysDslSeq =>
  lazy val PArray: Rep[PArrayCompanionAbs] = new PArrayCompanionAbs with UserTypeSeq[PArrayCompanionAbs, PArrayCompanionAbs] {
    lazy val selfType = element[PArrayCompanionAbs]
    
  }

  

  

  case class SeqUnitArray
      (override val len: Rep[Int])
      
    extends UnitArray(len)
        with UserTypeSeq[PArray[Unit], UnitArray] {
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
      (implicit eA: Elem[A])
    extends BaseArray[A](arr)
        with UserTypeSeq[PArray[A], BaseArray[A]] {
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
      (implicit eA: Elem[A], eB: Elem[B])
    extends PairArray[A, B](as, bs)
        with UserTypeSeq[PArray[(A,B)], PairArray[A, B]] {
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
      (implicit eA: Elem[A])
    extends NestedArray[A](values, segments)
        with UserTypeSeq[PArray[PArray[A]], NestedArray[A]] {
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

// Exp -----------------------------------
trait PArraysExp extends PArraysAbs with PArraysDsl with ScalanExp
{ self: PArraysDslExp =>
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

  object UnitArrayMethods {
    object elem {
      def unapply(d: Def[_]): Option[Rep[UnitArray]] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[UnitArrayElem] && method.getName == "elem" =>
          Some(receiver).asInstanceOf[Option[Rep[UnitArray]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[UnitArray]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object arr {
      def unapply(d: Def[_]): Option[Rep[UnitArray]] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[UnitArrayElem] && method.getName == "arr" =>
          Some(receiver).asInstanceOf[Option[Rep[UnitArray]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[UnitArray]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[UnitArray]] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[UnitArrayElem] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[UnitArray]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[UnitArray]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[UnitArray], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(i, _*)) if receiver.elem.isInstanceOf[UnitArrayElem] && method.getName == "apply"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[UnitArray], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[UnitArray], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_many {
      def unapply(d: Def[_]): Option[(Rep[UnitArray], Arr[Int])] = d match {
        case MethodCall(receiver, method, Seq(indices, _*)) if receiver.elem.isInstanceOf[UnitArrayElem] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[UnitArray], Arr[Int])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[UnitArray], Arr[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object slice {
      def unapply(d: Def[_]): Option[(Rep[UnitArray], Rep[Int], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(offset, length, _*)) if receiver.elem.isInstanceOf[UnitArrayElem] && method.getName == "slice" =>
          Some((receiver, offset, length)).asInstanceOf[Option[(Rep[UnitArray], Rep[Int], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[UnitArray], Rep[Int], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object UnitArrayCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Unit] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[UnitArrayCompanionElem] && method.getName == "defaultOf" =>
          Some(()).asInstanceOf[Option[Unit]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkUnitArray
    (len: Rep[Int]) =
    new ExpUnitArray(len)
  def unmkUnitArray(p: Rep[UnitArray]) =
    Some((p.len))

  case class ExpBaseArray[A]
      (override val arr: Rep[Array[A]])
      (implicit eA: Elem[A])
    extends BaseArray[A](arr) with UserTypeDef[PArray[A], BaseArray[A]] {
    lazy val selfType = element[BaseArray[A]].asInstanceOf[Elem[PArray[A]]]
    override def mirror(t: Transformer) = ExpBaseArray[A](t(arr))
  }

  lazy val BaseArray: Rep[BaseArrayCompanionAbs] = new BaseArrayCompanionAbs with UserTypeDef[BaseArrayCompanionAbs, BaseArrayCompanionAbs] {
    lazy val selfType = element[BaseArrayCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object BaseArrayMethods {
    object elem {
      def unapply(d: Def[_]): Option[Rep[BaseArray[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[BaseArrayElem[_]] && method.getName == "elem" =>
          Some(receiver).asInstanceOf[Option[Rep[BaseArray[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[BaseArray[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[BaseArray[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[BaseArrayElem[_]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[BaseArray[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[BaseArray[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[BaseArray[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(i, _*)) if receiver.elem.isInstanceOf[BaseArrayElem[_]] && method.getName == "apply"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[BaseArray[A]], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BaseArray[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object slice {
      def unapply(d: Def[_]): Option[(Rep[BaseArray[A]], Rep[Int], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(offset, length, _*)) if receiver.elem.isInstanceOf[BaseArrayElem[_]] && method.getName == "slice" =>
          Some((receiver, offset, length)).asInstanceOf[Option[(Rep[BaseArray[A]], Rep[Int], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BaseArray[A]], Rep[Int], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_many {
      def unapply(d: Def[_]): Option[(Rep[BaseArray[A]], Arr[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(indices, _*)) if receiver.elem.isInstanceOf[BaseArrayElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[BaseArray[A]], Arr[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BaseArray[A]], Arr[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object BaseArrayCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Elem[A] forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(ea, _*)) if receiver.elem.isInstanceOf[BaseArrayCompanionElem] && method.getName == "defaultOf" =>
          Some(ea).asInstanceOf[Option[Elem[A] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Elem[A] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkBaseArray[A]
    (arr: Rep[Array[A]])(implicit eA: Elem[A]) =
    new ExpBaseArray[A](arr)
  def unmkBaseArray[A:Elem](p: Rep[BaseArray[A]]) =
    Some((p.arr))

  case class ExpPairArray[A, B]
      (override val as: Rep[PArray[A]], override val bs: Rep[PArray[B]])
      (implicit eA: Elem[A], eB: Elem[B])
    extends PairArray[A, B](as, bs) with UserTypeDef[PArray[(A,B)], PairArray[A, B]] {
    lazy val selfType = element[PairArray[A, B]].asInstanceOf[Elem[PArray[(A,B)]]]
    override def mirror(t: Transformer) = ExpPairArray[A, B](t(as), t(bs))
  }

  lazy val PairArray: Rep[PairArrayCompanionAbs] = new PairArrayCompanionAbs with UserTypeDef[PairArrayCompanionAbs, PairArrayCompanionAbs] {
    lazy val selfType = element[PairArrayCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object PairArrayMethods {
    // WARNING: Cannot generate matcher for method `mapPairs`: Method has function arguments f

    object arr {
      def unapply(d: Def[_]): Option[Rep[PairArray[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[PairArrayElem[_, _]] && method.getName == "arr" =>
          Some(receiver).asInstanceOf[Option[Rep[PairArray[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairArray[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[PairArray[A, B]], Rep[Int]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(i, _*)) if receiver.elem.isInstanceOf[PairArrayElem[_, _]] && method.getName == "apply"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[PairArray[A, B]], Rep[Int]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairArray[A, B]], Rep[Int]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[PairArray[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[PairArrayElem[_, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[PairArray[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairArray[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object slice {
      def unapply(d: Def[_]): Option[(Rep[PairArray[A, B]], Rep[Int], Rep[Int]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(offset, length, _*)) if receiver.elem.isInstanceOf[PairArrayElem[_, _]] && method.getName == "slice" =>
          Some((receiver, offset, length)).asInstanceOf[Option[(Rep[PairArray[A, B]], Rep[Int], Rep[Int]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairArray[A, B]], Rep[Int], Rep[Int]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_many {
      def unapply(d: Def[_]): Option[(Rep[PairArray[A, B]], Arr[Int]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(indices, _*)) if receiver.elem.isInstanceOf[PairArrayElem[_, _]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[PairArray[A, B]], Arr[Int]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairArray[A, B]], Arr[Int]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object PairArrayCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[(Elem[A], Elem[B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(ea, eb, _*)) if receiver.elem.isInstanceOf[PairArrayCompanionElem] && method.getName == "defaultOf" =>
          Some((ea, eb)).asInstanceOf[Option[(Elem[A], Elem[B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Elem[A], Elem[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkPairArray[A, B]
    (as: Rep[PArray[A]], bs: Rep[PArray[B]])(implicit eA: Elem[A], eB: Elem[B]) =
    new ExpPairArray[A, B](as, bs)
  def unmkPairArray[A:Elem, B:Elem](p: Rep[PairArray[A, B]]) =
    Some((p.as, p.bs))

  case class ExpNestedArray[A]
      (override val values: Rep[PArray[A]], override val segments: Rep[PArray[(Int,Int)]])
      (implicit eA: Elem[A])
    extends NestedArray[A](values, segments) with UserTypeDef[PArray[PArray[A]], NestedArray[A]] {
    lazy val selfType = element[NestedArray[A]].asInstanceOf[Elem[PArray[PArray[A]]]]
    override def mirror(t: Transformer) = ExpNestedArray[A](t(values), t(segments))
  }

  lazy val NestedArray: Rep[NestedArrayCompanionAbs] = new NestedArrayCompanionAbs with UserTypeDef[NestedArrayCompanionAbs, NestedArrayCompanionAbs] {
    lazy val selfType = element[NestedArrayCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object NestedArrayMethods {
    object length {
      def unapply(d: Def[_]): Option[Rep[NestedArray[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[NestedArrayElem[_]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[NestedArray[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[NestedArray[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[NestedArray[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(i, _*)) if receiver.elem.isInstanceOf[NestedArrayElem[_]] && method.getName == "apply"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[NestedArray[A]], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedArray[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object arr {
      def unapply(d: Def[_]): Option[Rep[NestedArray[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[NestedArrayElem[_]] && method.getName == "arr" =>
          Some(receiver).asInstanceOf[Option[Rep[NestedArray[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[NestedArray[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object slice {
      def unapply(d: Def[_]): Option[(Rep[NestedArray[A]], Rep[Int], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(offset, length, _*)) if receiver.elem.isInstanceOf[NestedArrayElem[_]] && method.getName == "slice" =>
          Some((receiver, offset, length)).asInstanceOf[Option[(Rep[NestedArray[A]], Rep[Int], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedArray[A]], Rep[Int], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_many {
      def unapply(d: Def[_]): Option[(Rep[NestedArray[A]], Arr[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(indices, _*)) if receiver.elem.isInstanceOf[NestedArrayElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[NestedArray[A]], Arr[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedArray[A]], Arr[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object NestedArrayCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Elem[A] forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(ea, _*)) if receiver.elem.isInstanceOf[NestedArrayCompanionElem] && method.getName == "defaultOf" =>
          Some(ea).asInstanceOf[Option[Elem[A] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Elem[A] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkNestedArray[A]
    (values: Rep[PArray[A]], segments: Rep[PArray[(Int,Int)]])(implicit eA: Elem[A]) =
    new ExpNestedArray[A](values, segments)
  def unmkNestedArray[A:Elem](p: Rep[NestedArray[A]]) =
    Some((p.values, p.segments))

  object PArrayMethods {
    object length {
      def unapply(d: Def[_]): Option[Rep[PArray[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[PArrayElem[_, _, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[PArray[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PArray[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object arr {
      def unapply(d: Def[_]): Option[Rep[PArray[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _) if receiver.elem.isInstanceOf[PArrayElem[_, _, _]] && method.getName == "arr" =>
          Some(receiver).asInstanceOf[Option[Rep[PArray[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PArray[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[PArray[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(i, _*)) if receiver.elem.isInstanceOf[PArrayElem[_, _, _]] && method.getName == "apply"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[PArray[A]], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PArray[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_many {
      def unapply(d: Def[_]): Option[(Rep[PArray[A]], Arr[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(indices, _*)) if receiver.elem.isInstanceOf[PArrayElem[_, _, _]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[PArray[A]], Arr[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PArray[A]], Arr[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `map`: Method has function arguments f

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[PArray[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*)) if receiver.elem.isInstanceOf[PArrayElem[_, _, _]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[PArray[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PArray[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zip {
      def unapply(d: Def[_]): Option[(Rep[PArray[A]], PA[B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(ys, _*)) if receiver.elem.isInstanceOf[PArrayElem[_, _, _]] && method.getName == "zip" =>
          Some((receiver, ys)).asInstanceOf[Option[(Rep[PArray[A]], PA[B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PArray[A]], PA[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object slice {
      def unapply(d: Def[_]): Option[(Rep[PArray[A]], Rep[Int], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(offset, length, _*)) if receiver.elem.isInstanceOf[PArrayElem[_, _, _]] && method.getName == "slice" =>
          Some((receiver, offset, length)).asInstanceOf[Option[(Rep[PArray[A]], Rep[Int], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PArray[A]], Rep[Int], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[PArray[A]], RepMonoid[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(m, _*)) if receiver.elem.isInstanceOf[PArrayElem[_, _, _]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[PArray[A]], RepMonoid[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PArray[A]], RepMonoid[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object scan {
      def unapply(d: Def[_]): Option[(Rep[PArray[A]], RepMonoid[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(m, _*)) if receiver.elem.isInstanceOf[PArrayElem[_, _, _]] && method.getName == "scan" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[PArray[A]], RepMonoid[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PArray[A]], RepMonoid[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object PArrayCompanionMethods {
    // WARNING: Cannot generate matcher for method `defaultOf`: Method's return type Default[Rep[PArray[A]]] is not a Rep

    object apply {
      def unapply(d: Def[_]): Option[Rep[Array[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(arr, _*)) if receiver.elem.isInstanceOf[PArrayCompanionElem] && method.getName == "apply" =>
          Some(arr).asInstanceOf[Option[Rep[Array[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Array[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromArray {
      def unapply(d: Def[_]): Option[Rep[Array[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(arr, _*)) if receiver.elem.isInstanceOf[PArrayCompanionElem] && method.getName == "fromArray" =>
          Some(arr).asInstanceOf[Option[Rep[Array[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Array[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object replicate {
      def unapply(d: Def[_]): Option[(Rep[Int], Rep[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(len, v, _*)) if receiver.elem.isInstanceOf[PArrayCompanionElem] && method.getName == "replicate" =>
          Some((len, v)).asInstanceOf[Option[(Rep[Int], Rep[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Int], Rep[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object singleton {
      def unapply(d: Def[_]): Option[Rep[T] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(v, _*)) if receiver.elem.isInstanceOf[PArrayCompanionElem] && method.getName == "singleton" =>
          Some(v).asInstanceOf[Option[Rep[T] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[T] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}
