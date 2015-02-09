package scalan.collection
package impl

import scala.annotation.unchecked.uncheckedVariance
import scalan._
import scalan.arrays.ArrayOps
import scalan.collections._
import scalan.common.Default
import scalan.common.OverloadHack.Overloaded1
import scala.reflect.runtime.universe._
import scalan.common.Default

// Abs -----------------------------------
trait CollectionsAbs extends Scalan with Collections {
  self: CollectionsDsl =>
  // single proxy for each type family
  implicit def proxyCollection[A](p: Rep[Collection[A]]): Collection[A] =
    proxyOps[Collection[A]](p)

  abstract class CollectionElem[A, From, To <: Collection[A]](iso: Iso[From, To]) extends ViewElem[From, To]()(iso)

  trait CollectionCompanionElem extends CompanionElem[CollectionCompanionAbs]
  implicit lazy val CollectionCompanionElem: CollectionCompanionElem = new CollectionCompanionElem {
    lazy val tag = weakTypeTag[CollectionCompanionAbs]
    protected def getDefaultRep = Collection
  }

  abstract class CollectionCompanionAbs extends CompanionBase[CollectionCompanionAbs] with CollectionCompanion {
    override def toString = "Collection"
  }
  def Collection: Rep[CollectionCompanionAbs]
  implicit def proxyCollectionCompanion(p: Rep[CollectionCompanion]): CollectionCompanion = {
    proxyOps[CollectionCompanion](p)
  }

  // elem for concrete class
  class UnitCollectionElem(iso: Iso[UnitCollectionData, UnitCollection]) extends CollectionElem[Unit, UnitCollectionData, UnitCollection](iso)

  // state representation type
  type UnitCollectionData = Int

  // 3) Iso for concrete class
  class UnitCollectionIso
    extends Iso[UnitCollectionData, UnitCollection] {
    override def from(p: Rep[UnitCollection]) =
      unmkUnitCollection(p) match {
        case Some((len)) => len
        case None => !!!
      }
    override def to(p: Rep[Int]) = {
      val len = p
      UnitCollection(len)
    }
    lazy val tag = {
      weakTypeTag[UnitCollection]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[UnitCollection]](UnitCollection(0))
    lazy val eTo = new UnitCollectionElem(this)
  }
  // 4) constructor and deconstructor
  abstract class UnitCollectionCompanionAbs extends CompanionBase[UnitCollectionCompanionAbs] with UnitCollectionCompanion {
    override def toString = "UnitCollection"

    def apply(len: Rep[Int]): Rep[UnitCollection] =
      mkUnitCollection(len)
    def unapply(p: Rep[UnitCollection]) = unmkUnitCollection(p)
  }
  def UnitCollection: Rep[UnitCollectionCompanionAbs]
  implicit def proxyUnitCollectionCompanion(p: Rep[UnitCollectionCompanionAbs]): UnitCollectionCompanionAbs = {
    proxyOps[UnitCollectionCompanionAbs](p)
  }

  class UnitCollectionCompanionElem extends CompanionElem[UnitCollectionCompanionAbs] {
    lazy val tag = weakTypeTag[UnitCollectionCompanionAbs]
    protected def getDefaultRep = UnitCollection
  }
  implicit lazy val UnitCollectionCompanionElem: UnitCollectionCompanionElem = new UnitCollectionCompanionElem

  implicit def proxyUnitCollection(p: Rep[UnitCollection]): UnitCollection =
    proxyOps[UnitCollection](p)

  implicit class ExtendedUnitCollection(p: Rep[UnitCollection]) {
    def toData: Rep[UnitCollectionData] = isoUnitCollection.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoUnitCollection: Iso[UnitCollectionData, UnitCollection] =
    new UnitCollectionIso

  // 6) smart constructor and deconstructor
  def mkUnitCollection(len: Rep[Int]): Rep[UnitCollection]
  def unmkUnitCollection(p: Rep[UnitCollection]): Option[(Rep[Int])]

  // elem for concrete class
  class BaseCollectionElem[A](iso: Iso[BaseCollectionData[A], BaseCollection[A]]) extends CollectionElem[A, BaseCollectionData[A], BaseCollection[A]](iso)

  // state representation type
  type BaseCollectionData[A] = Array[A]

  // 3) Iso for concrete class
  class BaseCollectionIso[A](implicit eA: Elem[A])
    extends Iso[BaseCollectionData[A], BaseCollection[A]] {
    override def from(p: Rep[BaseCollection[A]]) =
      unmkBaseCollection(p) match {
        case Some((arr)) => arr
        case None => !!!
      }
    override def to(p: Rep[Array[A]]) = {
      val arr = p
      BaseCollection(arr)
    }
    lazy val tag = {
      weakTypeTag[BaseCollection[A]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[BaseCollection[A]]](BaseCollection(element[Array[A]].defaultRepValue))
    lazy val eTo = new BaseCollectionElem[A](this)
  }
  // 4) constructor and deconstructor
  abstract class BaseCollectionCompanionAbs extends CompanionBase[BaseCollectionCompanionAbs] with BaseCollectionCompanion {
    override def toString = "BaseCollection"

    def apply[A](arr: Rep[Array[A]])(implicit eA: Elem[A]): Rep[BaseCollection[A]] =
      mkBaseCollection(arr)
    def unapply[A:Elem](p: Rep[BaseCollection[A]]) = unmkBaseCollection(p)
  }
  def BaseCollection: Rep[BaseCollectionCompanionAbs]
  implicit def proxyBaseCollectionCompanion(p: Rep[BaseCollectionCompanionAbs]): BaseCollectionCompanionAbs = {
    proxyOps[BaseCollectionCompanionAbs](p)
  }

  class BaseCollectionCompanionElem extends CompanionElem[BaseCollectionCompanionAbs] {
    lazy val tag = weakTypeTag[BaseCollectionCompanionAbs]
    protected def getDefaultRep = BaseCollection
  }
  implicit lazy val BaseCollectionCompanionElem: BaseCollectionCompanionElem = new BaseCollectionCompanionElem

  implicit def proxyBaseCollection[A](p: Rep[BaseCollection[A]]): BaseCollection[A] =
    proxyOps[BaseCollection[A]](p)

  implicit class ExtendedBaseCollection[A](p: Rep[BaseCollection[A]])(implicit eA: Elem[A]) {
    def toData: Rep[BaseCollectionData[A]] = isoBaseCollection(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoBaseCollection[A](implicit eA: Elem[A]): Iso[BaseCollectionData[A], BaseCollection[A]] =
    new BaseCollectionIso[A]

  // 6) smart constructor and deconstructor
  def mkBaseCollection[A](arr: Rep[Array[A]])(implicit eA: Elem[A]): Rep[BaseCollection[A]]
  def unmkBaseCollection[A:Elem](p: Rep[BaseCollection[A]]): Option[(Rep[Array[A]])]

  // elem for concrete class
  class ListCollectionElem[A](iso: Iso[ListCollectionData[A], ListCollection[A]]) extends CollectionElem[A, ListCollectionData[A], ListCollection[A]](iso)

  // state representation type
  type ListCollectionData[A] = List[A]

  // 3) Iso for concrete class
  class ListCollectionIso[A](implicit eA: Elem[A])
    extends Iso[ListCollectionData[A], ListCollection[A]] {
    override def from(p: Rep[ListCollection[A]]) =
      unmkListCollection(p) match {
        case Some((lst)) => lst
        case None => !!!
      }
    override def to(p: Rep[List[A]]) = {
      val lst = p
      ListCollection(lst)
    }
    lazy val tag = {
      weakTypeTag[ListCollection[A]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[ListCollection[A]]](ListCollection(element[List[A]].defaultRepValue))
    lazy val eTo = new ListCollectionElem[A](this)
  }
  // 4) constructor and deconstructor
  abstract class ListCollectionCompanionAbs extends CompanionBase[ListCollectionCompanionAbs] with ListCollectionCompanion {
    override def toString = "ListCollection"

    def apply[A](lst: Rep[List[A]])(implicit eA: Elem[A]): Rep[ListCollection[A]] =
      mkListCollection(lst)
    def unapply[A:Elem](p: Rep[ListCollection[A]]) = unmkListCollection(p)
  }
  def ListCollection: Rep[ListCollectionCompanionAbs]
  implicit def proxyListCollectionCompanion(p: Rep[ListCollectionCompanionAbs]): ListCollectionCompanionAbs = {
    proxyOps[ListCollectionCompanionAbs](p)
  }

  class ListCollectionCompanionElem extends CompanionElem[ListCollectionCompanionAbs] {
    lazy val tag = weakTypeTag[ListCollectionCompanionAbs]
    protected def getDefaultRep = ListCollection
  }
  implicit lazy val ListCollectionCompanionElem: ListCollectionCompanionElem = new ListCollectionCompanionElem

  implicit def proxyListCollection[A](p: Rep[ListCollection[A]]): ListCollection[A] =
    proxyOps[ListCollection[A]](p)

  implicit class ExtendedListCollection[A](p: Rep[ListCollection[A]])(implicit eA: Elem[A]) {
    def toData: Rep[ListCollectionData[A]] = isoListCollection(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoListCollection[A](implicit eA: Elem[A]): Iso[ListCollectionData[A], ListCollection[A]] =
    new ListCollectionIso[A]

  // 6) smart constructor and deconstructor
  def mkListCollection[A](lst: Rep[List[A]])(implicit eA: Elem[A]): Rep[ListCollection[A]]
  def unmkListCollection[A:Elem](p: Rep[ListCollection[A]]): Option[(Rep[List[A]])]

  // elem for concrete class
  class PairCollectionElem[A, B](iso: Iso[PairCollectionData[A, B], PairCollection[A, B]]) extends CollectionElem[(A,B), PairCollectionData[A, B], PairCollection[A, B]](iso)

  // state representation type
  type PairCollectionData[A, B] = (Collection[A], Collection[B])

  // 3) Iso for concrete class
  class PairCollectionIso[A, B](implicit eA: Elem[A], eB: Elem[B])
    extends Iso[PairCollectionData[A, B], PairCollection[A, B]] {
    override def from(p: Rep[PairCollection[A, B]]) =
      unmkPairCollection(p) match {
        case Some((as, bs)) => Pair(as, bs)
        case None => !!!
      }
    override def to(p: Rep[(Collection[A], Collection[B])]) = {
      val Pair(as, bs) = p
      PairCollection(as, bs)
    }
    lazy val tag = {
      weakTypeTag[PairCollection[A, B]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[PairCollection[A, B]]](PairCollection(element[Collection[A]].defaultRepValue, element[Collection[B]].defaultRepValue))
    lazy val eTo = new PairCollectionElem[A, B](this)
  }
  // 4) constructor and deconstructor
  abstract class PairCollectionCompanionAbs extends CompanionBase[PairCollectionCompanionAbs] with PairCollectionCompanion {
    override def toString = "PairCollection"
    def apply[A, B](p: Rep[PairCollectionData[A, B]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairCollection[A, B]] =
      isoPairCollection(eA, eB).to(p)
    def apply[A, B](as: Rep[Collection[A]], bs: Rep[Collection[B]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairCollection[A, B]] =
      mkPairCollection(as, bs)
    def unapply[A:Elem, B:Elem](p: Rep[PairCollection[A, B]]) = unmkPairCollection(p)
  }
  def PairCollection: Rep[PairCollectionCompanionAbs]
  implicit def proxyPairCollectionCompanion(p: Rep[PairCollectionCompanionAbs]): PairCollectionCompanionAbs = {
    proxyOps[PairCollectionCompanionAbs](p)
  }

  class PairCollectionCompanionElem extends CompanionElem[PairCollectionCompanionAbs] {
    lazy val tag = weakTypeTag[PairCollectionCompanionAbs]
    protected def getDefaultRep = PairCollection
  }
  implicit lazy val PairCollectionCompanionElem: PairCollectionCompanionElem = new PairCollectionCompanionElem

  implicit def proxyPairCollection[A, B](p: Rep[PairCollection[A, B]]): PairCollection[A, B] =
    proxyOps[PairCollection[A, B]](p)

  implicit class ExtendedPairCollection[A, B](p: Rep[PairCollection[A, B]])(implicit eA: Elem[A], eB: Elem[B]) {
    def toData: Rep[PairCollectionData[A, B]] = isoPairCollection(eA, eB).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoPairCollection[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[PairCollectionData[A, B], PairCollection[A, B]] =
    new PairCollectionIso[A, B]

  // 6) smart constructor and deconstructor
  def mkPairCollection[A, B](as: Rep[Collection[A]], bs: Rep[Collection[B]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairCollection[A, B]]
  def unmkPairCollection[A:Elem, B:Elem](p: Rep[PairCollection[A, B]]): Option[(Rep[Collection[A]], Rep[Collection[B]])]

  // elem for concrete class
  class NestedCollectionElem[A](iso: Iso[NestedCollectionData[A], NestedCollection[A]]) extends CollectionElem[Collection[A], NestedCollectionData[A], NestedCollection[A]](iso)

  // state representation type
  type NestedCollectionData[A] = (Collection[A], Collection[(Int,Int)])

  // 3) Iso for concrete class
  class NestedCollectionIso[A](implicit eA: Elem[A])
    extends Iso[NestedCollectionData[A], NestedCollection[A]] {
    override def from(p: Rep[NestedCollection[A]]) =
      unmkNestedCollection(p) match {
        case Some((values, segments)) => Pair(values, segments)
        case None => !!!
      }
    override def to(p: Rep[(Collection[A], Collection[(Int,Int)])]) = {
      val Pair(values, segments) = p
      NestedCollection(values, segments)
    }
    lazy val tag = {
      weakTypeTag[NestedCollection[A]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[NestedCollection[A]]](NestedCollection(element[Collection[A]].defaultRepValue, element[Collection[(Int,Int)]].defaultRepValue))
    lazy val eTo = new NestedCollectionElem[A](this)
  }
  // 4) constructor and deconstructor
  abstract class NestedCollectionCompanionAbs extends CompanionBase[NestedCollectionCompanionAbs] with NestedCollectionCompanion {
    override def toString = "NestedCollection"
    def apply[A](p: Rep[NestedCollectionData[A]])(implicit eA: Elem[A]): Rep[NestedCollection[A]] =
      isoNestedCollection(eA).to(p)
    def apply[A](values: Rep[Collection[A]], segments: Rep[Collection[(Int,Int)]])(implicit eA: Elem[A]): Rep[NestedCollection[A]] =
      mkNestedCollection(values, segments)
    def unapply[A:Elem](p: Rep[NestedCollection[A]]) = unmkNestedCollection(p)
  }
  def NestedCollection: Rep[NestedCollectionCompanionAbs]
  implicit def proxyNestedCollectionCompanion(p: Rep[NestedCollectionCompanionAbs]): NestedCollectionCompanionAbs = {
    proxyOps[NestedCollectionCompanionAbs](p)
  }

  class NestedCollectionCompanionElem extends CompanionElem[NestedCollectionCompanionAbs] {
    lazy val tag = weakTypeTag[NestedCollectionCompanionAbs]
    protected def getDefaultRep = NestedCollection
  }
  implicit lazy val NestedCollectionCompanionElem: NestedCollectionCompanionElem = new NestedCollectionCompanionElem

  implicit def proxyNestedCollection[A](p: Rep[NestedCollection[A]]): NestedCollection[A] =
    proxyOps[NestedCollection[A]](p)

  implicit class ExtendedNestedCollection[A](p: Rep[NestedCollection[A]])(implicit eA: Elem[A]) {
    def toData: Rep[NestedCollectionData[A]] = isoNestedCollection(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoNestedCollection[A](implicit eA: Elem[A]): Iso[NestedCollectionData[A], NestedCollection[A]] =
    new NestedCollectionIso[A]

  // 6) smart constructor and deconstructor
  def mkNestedCollection[A](values: Rep[Collection[A]], segments: Rep[Collection[(Int,Int)]])(implicit eA: Elem[A]): Rep[NestedCollection[A]]
  def unmkNestedCollection[A:Elem](p: Rep[NestedCollection[A]]): Option[(Rep[Collection[A]], Rep[Collection[(Int,Int)]])]
}

// Seq -----------------------------------
trait CollectionsSeq extends CollectionsDsl with ScalanSeq {
  self: CollectionsDslSeq =>
  lazy val Collection: Rep[CollectionCompanionAbs] = new CollectionCompanionAbs with UserTypeSeq[CollectionCompanionAbs, CollectionCompanionAbs] {
    lazy val selfType = element[CollectionCompanionAbs]
  }

  case class SeqUnitCollection
      (override val len: Rep[Int])

    extends UnitCollection(len)
        with UserTypeSeq[Collection[Unit], UnitCollection] {
    lazy val selfType = element[UnitCollection].asInstanceOf[Elem[Collection[Unit]]]
  }
  lazy val UnitCollection = new UnitCollectionCompanionAbs with UserTypeSeq[UnitCollectionCompanionAbs, UnitCollectionCompanionAbs] {
    lazy val selfType = element[UnitCollectionCompanionAbs]
  }

  def mkUnitCollection
      (len: Rep[Int]) =
      new SeqUnitCollection(len)
  def unmkUnitCollection(p: Rep[UnitCollection]) =
    Some((p.len))

  case class SeqBaseCollection[A]
      (override val arr: Rep[Array[A]])
      (implicit eA: Elem[A])
    extends BaseCollection[A](arr)
        with UserTypeSeq[Collection[A], BaseCollection[A]] {
    lazy val selfType = element[BaseCollection[A]].asInstanceOf[Elem[Collection[A]]]
  }
  lazy val BaseCollection = new BaseCollectionCompanionAbs with UserTypeSeq[BaseCollectionCompanionAbs, BaseCollectionCompanionAbs] {
    lazy val selfType = element[BaseCollectionCompanionAbs]
  }

  def mkBaseCollection[A]
      (arr: Rep[Array[A]])(implicit eA: Elem[A]) =
      new SeqBaseCollection[A](arr)
  def unmkBaseCollection[A:Elem](p: Rep[BaseCollection[A]]) =
    Some((p.arr))

  case class SeqListCollection[A]
      (override val lst: Rep[List[A]])
      (implicit eA: Elem[A])
    extends ListCollection[A](lst)
        with UserTypeSeq[Collection[A], ListCollection[A]] {
    lazy val selfType = element[ListCollection[A]].asInstanceOf[Elem[Collection[A]]]
  }
  lazy val ListCollection = new ListCollectionCompanionAbs with UserTypeSeq[ListCollectionCompanionAbs, ListCollectionCompanionAbs] {
    lazy val selfType = element[ListCollectionCompanionAbs]
  }

  def mkListCollection[A]
      (lst: Rep[List[A]])(implicit eA: Elem[A]) =
      new SeqListCollection[A](lst)
  def unmkListCollection[A:Elem](p: Rep[ListCollection[A]]) =
    Some((p.lst))

  case class SeqPairCollection[A, B]
      (override val as: Rep[Collection[A]], override val bs: Rep[Collection[B]])
      (implicit eA: Elem[A], eB: Elem[B])
    extends PairCollection[A, B](as, bs)
        with UserTypeSeq[Collection[(A,B)], PairCollection[A, B]] {
    lazy val selfType = element[PairCollection[A, B]].asInstanceOf[Elem[Collection[(A,B)]]]
  }
  lazy val PairCollection = new PairCollectionCompanionAbs with UserTypeSeq[PairCollectionCompanionAbs, PairCollectionCompanionAbs] {
    lazy val selfType = element[PairCollectionCompanionAbs]
  }

  def mkPairCollection[A, B]
      (as: Rep[Collection[A]], bs: Rep[Collection[B]])(implicit eA: Elem[A], eB: Elem[B]) =
      new SeqPairCollection[A, B](as, bs)
  def unmkPairCollection[A:Elem, B:Elem](p: Rep[PairCollection[A, B]]) =
    Some((p.as, p.bs))

  case class SeqNestedCollection[A]
      (override val values: Rep[Collection[A]], override val segments: Rep[Collection[(Int,Int)]])
      (implicit eA: Elem[A])
    extends NestedCollection[A](values, segments)
        with UserTypeSeq[Collection[Collection[A]], NestedCollection[A]] {
    lazy val selfType = element[NestedCollection[A]].asInstanceOf[Elem[Collection[Collection[A]]]]
  }
  lazy val NestedCollection = new NestedCollectionCompanionAbs with UserTypeSeq[NestedCollectionCompanionAbs, NestedCollectionCompanionAbs] {
    lazy val selfType = element[NestedCollectionCompanionAbs]
  }

  def mkNestedCollection[A]
      (values: Rep[Collection[A]], segments: Rep[Collection[(Int,Int)]])(implicit eA: Elem[A]) =
      new SeqNestedCollection[A](values, segments)
  def unmkNestedCollection[A:Elem](p: Rep[NestedCollection[A]]) =
    Some((p.values, p.segments))
}

// Exp -----------------------------------
trait CollectionsExp extends CollectionsDsl with ScalanExp {
  self: CollectionsDslExp =>
  lazy val Collection: Rep[CollectionCompanionAbs] = new CollectionCompanionAbs with UserTypeDef[CollectionCompanionAbs, CollectionCompanionAbs] {
    lazy val selfType = element[CollectionCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpUnitCollection
      (override val len: Rep[Int])

    extends UnitCollection(len) with UserTypeDef[Collection[Unit], UnitCollection] {
    lazy val selfType = element[UnitCollection].asInstanceOf[Elem[Collection[Unit]]]
    override def mirror(t: Transformer) = ExpUnitCollection(t(len))
  }

  lazy val UnitCollection: Rep[UnitCollectionCompanionAbs] = new UnitCollectionCompanionAbs with UserTypeDef[UnitCollectionCompanionAbs, UnitCollectionCompanionAbs] {
    lazy val selfType = element[UnitCollectionCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object UnitCollectionMethods {
    object elem {
      def unapply(d: Def[_]): Option[Rep[UnitCollection]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[UnitCollectionElem] && method.getName == "elem" =>
          Some(receiver).asInstanceOf[Option[Rep[UnitCollection]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[UnitCollection]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object arr {
      def unapply(d: Def[_]): Option[Rep[UnitCollection]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[UnitCollectionElem] && method.getName == "arr" =>
          Some(receiver).asInstanceOf[Option[Rep[UnitCollection]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[UnitCollection]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[UnitCollection]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[UnitCollectionElem] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[UnitCollection]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[UnitCollection]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[UnitCollection], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[UnitCollectionElem] && method.getName == "apply"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[UnitCollection], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[UnitCollection], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `map`: Method has function arguments f

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[UnitCollection], Rep[Unit => B]) forSome {type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[UnitCollectionElem] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[UnitCollection], Rep[Unit => B]) forSome {type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[UnitCollection], Rep[Unit => B]) forSome {type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_many {
      def unapply(d: Def[_]): Option[(Rep[UnitCollection], Arr[Int])] = d match {
        case MethodCall(receiver, method, Seq(indices, _*), _) if receiver.elem.isInstanceOf[UnitCollectionElem] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[UnitCollection], Arr[Int])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[UnitCollection], Arr[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object slice {
      def unapply(d: Def[_]): Option[(Rep[UnitCollection], Rep[Int], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(offset, length, _*), _) if receiver.elem.isInstanceOf[UnitCollectionElem] && method.getName == "slice" =>
          Some((receiver, offset, length)).asInstanceOf[Option[(Rep[UnitCollection], Rep[Int], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[UnitCollection], Rep[Int], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[UnitCollection], RepMonoid[Unit])] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[UnitCollectionElem] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[UnitCollection], RepMonoid[Unit])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[UnitCollection], RepMonoid[Unit])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zip {
      def unapply(d: Def[_]): Option[(Rep[UnitCollection], Coll[B]) forSome {type B}] = d match {
        case MethodCall(receiver, method, Seq(ys, _*), _) if receiver.elem.isInstanceOf[UnitCollectionElem] && method.getName == "zip" =>
          Some((receiver, ys)).asInstanceOf[Option[(Rep[UnitCollection], Coll[B]) forSome {type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[UnitCollection], Coll[B]) forSome {type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object UnitCollectionCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Unit] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[UnitCollectionCompanionElem] && method.getName == "defaultOf" =>
          Some(()).asInstanceOf[Option[Unit]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkUnitCollection
    (len: Rep[Int]) =
    new ExpUnitCollection(len)
  def unmkUnitCollection(p: Rep[UnitCollection]) =
    Some((p.len))

  case class ExpBaseCollection[A]
      (override val arr: Rep[Array[A]])
      (implicit eA: Elem[A])
    extends BaseCollection[A](arr) with UserTypeDef[Collection[A], BaseCollection[A]] {
    lazy val selfType = element[BaseCollection[A]].asInstanceOf[Elem[Collection[A]]]
    override def mirror(t: Transformer) = ExpBaseCollection[A](t(arr))
  }

  lazy val BaseCollection: Rep[BaseCollectionCompanionAbs] = new BaseCollectionCompanionAbs with UserTypeDef[BaseCollectionCompanionAbs, BaseCollectionCompanionAbs] {
    lazy val selfType = element[BaseCollectionCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object BaseCollectionMethods {
    object elem {
      def unapply(d: Def[_]): Option[Rep[BaseCollection[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BaseCollectionElem[_]] && method.getName == "elem" =>
          Some(receiver).asInstanceOf[Option[Rep[BaseCollection[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[BaseCollection[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[BaseCollection[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BaseCollectionElem[_]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[BaseCollection[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[BaseCollection[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[BaseCollection[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[BaseCollectionElem[_]] && method.getName == "apply"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[BaseCollection[A]], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BaseCollection[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `map`: Method has function arguments f

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[BaseCollection[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[BaseCollectionElem[_]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[BaseCollection[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BaseCollection[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object slice {
      def unapply(d: Def[_]): Option[(Rep[BaseCollection[A]], Rep[Int], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(offset, length, _*), _) if receiver.elem.isInstanceOf[BaseCollectionElem[_]] && method.getName == "slice" =>
          Some((receiver, offset, length)).asInstanceOf[Option[(Rep[BaseCollection[A]], Rep[Int], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BaseCollection[A]], Rep[Int], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_many {
      def unapply(d: Def[_]): Option[(Rep[BaseCollection[A]], Arr[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(indices, _*), _) if receiver.elem.isInstanceOf[BaseCollectionElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[BaseCollection[A]], Arr[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BaseCollection[A]], Arr[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[BaseCollection[A]], RepMonoid[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[BaseCollectionElem[_]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[BaseCollection[A]], RepMonoid[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BaseCollection[A]], RepMonoid[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zip {
      def unapply(d: Def[_]): Option[(Rep[BaseCollection[A]], Coll[B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(ys, _*), _) if receiver.elem.isInstanceOf[BaseCollectionElem[_]] && method.getName == "zip" =>
          Some((receiver, ys)).asInstanceOf[Option[(Rep[BaseCollection[A]], Coll[B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BaseCollection[A]], Coll[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object BaseCollectionCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Elem[A] forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(ea, _*), _) if receiver.elem.isInstanceOf[BaseCollectionCompanionElem] && method.getName == "defaultOf" =>
          Some(ea).asInstanceOf[Option[Elem[A] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Elem[A] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkBaseCollection[A]
    (arr: Rep[Array[A]])(implicit eA: Elem[A]) =
    new ExpBaseCollection[A](arr)
  def unmkBaseCollection[A:Elem](p: Rep[BaseCollection[A]]) =
    Some((p.arr))

  case class ExpListCollection[A]
      (override val lst: Rep[List[A]])
      (implicit eA: Elem[A])
    extends ListCollection[A](lst) with UserTypeDef[Collection[A], ListCollection[A]] {
    lazy val selfType = element[ListCollection[A]].asInstanceOf[Elem[Collection[A]]]
    override def mirror(t: Transformer) = ExpListCollection[A](t(lst))
  }

  lazy val ListCollection: Rep[ListCollectionCompanionAbs] = new ListCollectionCompanionAbs with UserTypeDef[ListCollectionCompanionAbs, ListCollectionCompanionAbs] {
    lazy val selfType = element[ListCollectionCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object ListCollectionMethods {
    object elem {
      def unapply(d: Def[_]): Option[Rep[ListCollection[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ListCollectionElem[_]] && method.getName == "elem" =>
          Some(receiver).asInstanceOf[Option[Rep[ListCollection[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ListCollection[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[ListCollection[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ListCollectionElem[_]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[ListCollection[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ListCollection[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[ListCollection[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[ListCollectionElem[_]] && method.getName == "apply"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[ListCollection[A]], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ListCollection[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object arr {
      def unapply(d: Def[_]): Option[Rep[ListCollection[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ListCollectionElem[_]] && method.getName == "arr" =>
          Some(receiver).asInstanceOf[Option[Rep[ListCollection[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ListCollection[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `map`: Method has function arguments f

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[ListCollection[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[ListCollectionElem[_]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[ListCollection[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ListCollection[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object slice {
      def unapply(d: Def[_]): Option[(Rep[ListCollection[A]], Rep[Int], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(offset, length, _*), _) if receiver.elem.isInstanceOf[ListCollectionElem[_]] && method.getName == "slice" =>
          Some((receiver, offset, length)).asInstanceOf[Option[(Rep[ListCollection[A]], Rep[Int], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ListCollection[A]], Rep[Int], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_many {
      def unapply(d: Def[_]): Option[(Rep[ListCollection[A]], Arr[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(indices, _*), _) if receiver.elem.isInstanceOf[ListCollectionElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[ListCollection[A]], Arr[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ListCollection[A]], Arr[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[ListCollection[A]], RepMonoid[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[ListCollectionElem[_]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[ListCollection[A]], RepMonoid[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ListCollection[A]], RepMonoid[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zip {
      def unapply(d: Def[_]): Option[(Rep[ListCollection[A]], Coll[B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(ys, _*), _) if receiver.elem.isInstanceOf[ListCollectionElem[_]] && method.getName == "zip" =>
          Some((receiver, ys)).asInstanceOf[Option[(Rep[ListCollection[A]], Coll[B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ListCollection[A]], Coll[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ListCollectionCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Elem[A] forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(ea, _*), _) if receiver.elem.isInstanceOf[ListCollectionCompanionElem] && method.getName == "defaultOf" =>
          Some(ea).asInstanceOf[Option[Elem[A] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Elem[A] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkListCollection[A]
    (lst: Rep[List[A]])(implicit eA: Elem[A]) =
    new ExpListCollection[A](lst)
  def unmkListCollection[A:Elem](p: Rep[ListCollection[A]]) =
    Some((p.lst))

  case class ExpPairCollection[A, B]
      (override val as: Rep[Collection[A]], override val bs: Rep[Collection[B]])
      (implicit eA: Elem[A], eB: Elem[B])
    extends PairCollection[A, B](as, bs) with UserTypeDef[Collection[(A,B)], PairCollection[A, B]] {
    lazy val selfType = element[PairCollection[A, B]].asInstanceOf[Elem[Collection[(A,B)]]]
    override def mirror(t: Transformer) = ExpPairCollection[A, B](t(as), t(bs))
  }

  lazy val PairCollection: Rep[PairCollectionCompanionAbs] = new PairCollectionCompanionAbs with UserTypeDef[PairCollectionCompanionAbs, PairCollectionCompanionAbs] {
    lazy val selfType = element[PairCollectionCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object PairCollectionMethods {
    // WARNING: Cannot generate matcher for method `mapPairs`: Method has function arguments f

    object arr {
      def unapply(d: Def[_]): Option[Rep[PairCollection[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairCollectionElem[_, _]] && method.getName == "arr" =>
          Some(receiver).asInstanceOf[Option[Rep[PairCollection[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairCollection[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[PairCollection[A, B]], Rep[Int]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[PairCollectionElem[_, _]] && method.getName == "apply"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[PairCollection[A, B]], Rep[Int]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollection[A, B]], Rep[Int]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[PairCollection[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairCollectionElem[_, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[PairCollection[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairCollection[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object slice {
      def unapply(d: Def[_]): Option[(Rep[PairCollection[A, B]], Rep[Int], Rep[Int]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(offset, length, _*), _) if receiver.elem.isInstanceOf[PairCollectionElem[_, _]] && method.getName == "slice" =>
          Some((receiver, offset, length)).asInstanceOf[Option[(Rep[PairCollection[A, B]], Rep[Int], Rep[Int]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollection[A, B]], Rep[Int], Rep[Int]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_many {
      def unapply(d: Def[_]): Option[(Rep[PairCollection[A, B]], Arr[Int]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(indices, _*), _) if receiver.elem.isInstanceOf[PairCollectionElem[_, _]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[PairCollection[A, B]], Arr[Int]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollection[A, B]], Arr[Int]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `map`: Method has function arguments f

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[PairCollection[A, B]], Rep[((A,B)) => C]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[PairCollectionElem[_, _]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[PairCollection[A, B]], Rep[((A,B)) => C]) forSome {type A; type B; type C}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollection[A, B]], Rep[((A,B)) => C]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[PairCollection[A, B]], RepMonoid[(A,B)]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[PairCollectionElem[_, _]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[PairCollection[A, B]], RepMonoid[(A,B)]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollection[A, B]], RepMonoid[(A,B)]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zip {
      def unapply(d: Def[_]): Option[(Rep[PairCollection[A, B]], Coll[C]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, Seq(ys, _*), _) if receiver.elem.isInstanceOf[PairCollectionElem[_, _]] && method.getName == "zip" =>
          Some((receiver, ys)).asInstanceOf[Option[(Rep[PairCollection[A, B]], Coll[C]) forSome {type A; type B; type C}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollection[A, B]], Coll[C]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object PairCollectionCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[(Elem[A], Elem[B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(ea, eb, _*), _) if receiver.elem.isInstanceOf[PairCollectionCompanionElem] && method.getName == "defaultOf" =>
          Some((ea, eb)).asInstanceOf[Option[(Elem[A], Elem[B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Elem[A], Elem[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkPairCollection[A, B]
    (as: Rep[Collection[A]], bs: Rep[Collection[B]])(implicit eA: Elem[A], eB: Elem[B]) =
    new ExpPairCollection[A, B](as, bs)
  def unmkPairCollection[A:Elem, B:Elem](p: Rep[PairCollection[A, B]]) =
    Some((p.as, p.bs))

  case class ExpNestedCollection[A]
      (override val values: Rep[Collection[A]], override val segments: Rep[Collection[(Int,Int)]])
      (implicit eA: Elem[A])
    extends NestedCollection[A](values, segments) with UserTypeDef[Collection[Collection[A]], NestedCollection[A]] {
    lazy val selfType = element[NestedCollection[A]].asInstanceOf[Elem[Collection[Collection[A]]]]
    override def mirror(t: Transformer) = ExpNestedCollection[A](t(values), t(segments))
  }

  lazy val NestedCollection: Rep[NestedCollectionCompanionAbs] = new NestedCollectionCompanionAbs with UserTypeDef[NestedCollectionCompanionAbs, NestedCollectionCompanionAbs] {
    lazy val selfType = element[NestedCollectionCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object NestedCollectionMethods {
    object length {
      def unapply(d: Def[_]): Option[Rep[NestedCollection[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[NestedCollectionElem[_]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[NestedCollection[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[NestedCollection[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[NestedCollection[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[NestedCollectionElem[_]] && method.getName == "apply"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[NestedCollection[A]], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollection[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object arr {
      def unapply(d: Def[_]): Option[Rep[NestedCollection[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[NestedCollectionElem[_]] && method.getName == "arr" =>
          Some(receiver).asInstanceOf[Option[Rep[NestedCollection[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[NestedCollection[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object slice {
      def unapply(d: Def[_]): Option[(Rep[NestedCollection[A]], Rep[Int], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(offset, length, _*), _) if receiver.elem.isInstanceOf[NestedCollectionElem[_]] && method.getName == "slice" =>
          Some((receiver, offset, length)).asInstanceOf[Option[(Rep[NestedCollection[A]], Rep[Int], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollection[A]], Rep[Int], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_many {
      def unapply(d: Def[_]): Option[(Rep[NestedCollection[A]], Arr[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(indices, _*), _) if receiver.elem.isInstanceOf[NestedCollectionElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[NestedCollection[A]], Arr[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollection[A]], Arr[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `map`: Method has function arguments f

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[NestedCollection[A]], Rep[Collection[A] => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[NestedCollectionElem[_]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[NestedCollection[A]], Rep[Collection[A] => B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollection[A]], Rep[Collection[A] => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[NestedCollection[A]], RepMonoid[Collection[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[NestedCollectionElem[_]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[NestedCollection[A]], RepMonoid[Collection[A]]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollection[A]], RepMonoid[Collection[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zip {
      def unapply(d: Def[_]): Option[(Rep[NestedCollection[A]], Coll[B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(ys, _*), _) if receiver.elem.isInstanceOf[NestedCollectionElem[_]] && method.getName == "zip" =>
          Some((receiver, ys)).asInstanceOf[Option[(Rep[NestedCollection[A]], Coll[B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollection[A]], Coll[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object NestedCollectionCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Elem[A] forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(ea, _*), _) if receiver.elem.isInstanceOf[NestedCollectionCompanionElem] && method.getName == "defaultOf" =>
          Some(ea).asInstanceOf[Option[Elem[A] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Elem[A] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkNestedCollection[A]
    (values: Rep[Collection[A]], segments: Rep[Collection[(Int,Int)]])(implicit eA: Elem[A]) =
    new ExpNestedCollection[A](values, segments)
  def unmkNestedCollection[A:Elem](p: Rep[NestedCollection[A]]) =
    Some((p.values, p.segments))

  object CollectionMethods {
    object length {
      def unapply(d: Def[_]): Option[Rep[Collection[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionElem[_, _, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[Collection[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Collection[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object arr {
      def unapply(d: Def[_]): Option[Rep[Collection[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionElem[_, _, _]] && method.getName == "arr" =>
          Some(receiver).asInstanceOf[Option[Rep[Collection[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Collection[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[Collection[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _, _]] && method.getName == "apply"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[Collection[A]], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_many {
      def unapply(d: Def[_]): Option[(Rep[Collection[A]], Arr[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(indices, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _, _]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[Collection[A]], Arr[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[A]], Arr[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `map`: Method has function arguments f

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[Collection[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _, _]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Collection[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zip {
      def unapply(d: Def[_]): Option[(Rep[Collection[A]], Coll[B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(ys, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _, _]] && method.getName == "zip" =>
          Some((receiver, ys)).asInstanceOf[Option[(Rep[Collection[A]], Coll[B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[A]], Coll[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object slice {
      def unapply(d: Def[_]): Option[(Rep[Collection[A]], Rep[Int], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(offset, length, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _, _]] && method.getName == "slice" =>
          Some((receiver, offset, length)).asInstanceOf[Option[(Rep[Collection[A]], Rep[Int], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[A]], Rep[Int], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[Collection[A]], RepMonoid[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _, _]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[Collection[A]], RepMonoid[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[A]], RepMonoid[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CollectionCompanionMethods {
    // WARNING: Cannot generate matcher for method `defaultOf`: Method's return type Default[Rep[Collection[A]]] is not a Rep

    object apply {
      def unapply(d: Def[_]): Option[Rep[Array[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(arr, _*), _) if receiver.elem.isInstanceOf[CollectionCompanionElem] && method.getName == "apply" =>
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
        case MethodCall(receiver, method, Seq(arr, _*), _) if receiver.elem.isInstanceOf[CollectionCompanionElem] && method.getName == "fromArray" =>
          Some(arr).asInstanceOf[Option[Rep[Array[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Array[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromList {
      def unapply(d: Def[_]): Option[Rep[List[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(arr, _*), _) if receiver.elem.isInstanceOf[CollectionCompanionElem] && method.getName == "fromList" =>
          Some(arr).asInstanceOf[Option[Rep[List[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[List[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object replicate {
      def unapply(d: Def[_]): Option[(Rep[Int], Rep[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(len, v, _*), _) if receiver.elem.isInstanceOf[CollectionCompanionElem] && method.getName == "replicate" =>
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
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem.isInstanceOf[CollectionCompanionElem] && method.getName == "singleton" =>
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
