package scalan.collections
package impl

import scala.annotation.unchecked.uncheckedVariance
import scalan._
import scalan.arrays.ArrayOps
import scalan.common.Default
import scalan.common.OverloadHack.Overloaded1
import scala.reflect.runtime.universe._
import scala.reflect._
import scalan.common.Default

// Abs -----------------------------------
trait CollectionsAbs extends Scalan with Collections {
  self: ScalanCommunityDsl =>
  // single proxy for each type family
  implicit def proxyCollection[A](p: Rep[Collection[A]]): Collection[A] = {
    proxyOps[Collection[A]](p)(classTag[Collection[A]])
  }

  class CollectionElem[A, To <: Collection[A]](implicit val elem: Elem[A])
    extends EntityElem[To] {
    override def isEntityType = true
    override def tag = {
      implicit val tagA = elem.tag
      weakTypeTag[Collection[A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = convertCollection(x.asRep[Collection[A]])
    def convertCollection(x : Rep[Collection[A]]): Rep[To] = {
      assert(x.selfType1.isInstanceOf[CollectionElem[_,_]])
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def collectionElement[A](implicit elem: Elem[A]) =
    new CollectionElem[A, Collection[A]]()(elem)

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

  // single proxy for each type family
  implicit def proxyIPairCollection[A, B](p: Rep[IPairCollection[A, B]]): IPairCollection[A, B] = {
    proxyOps[IPairCollection[A, B]](p)(classTag[IPairCollection[A, B]])
  }
  class IPairCollectionElem[A, B, To <: IPairCollection[A, B]](implicit val eA: Elem[A], val eB: Elem[B])
    extends CollectionElem[(A, B), To] {
    override def isEntityType = true
    override def tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[IPairCollection[A, B]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = convertIPairCollection(x.asRep[IPairCollection[A, B]])
    def convertIPairCollection(x : Rep[IPairCollection[A, B]]): Rep[To] = {
      assert(x.selfType1.isInstanceOf[IPairCollectionElem[_,_,_]])
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def iPairCollectionElement[A, B](implicit eA: Elem[A], eB: Elem[B]) =
    new IPairCollectionElem[A, B, IPairCollection[A, B]]()(eA, eB)

  // single proxy for each type family
  implicit def proxyINestedCollection[A](p: Rep[INestedCollection[A]]): INestedCollection[A] = {
    proxyOps[INestedCollection[A]](p)(classTag[INestedCollection[A]])
  }
  class INestedCollectionElem[A, To <: INestedCollection[A]](implicit val eA: Elem[A])
    extends CollectionElem[Collection[A], To] {
    override def isEntityType = true
    override def tag = {
      implicit val tagA = eA.tag
      weakTypeTag[INestedCollection[A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = convertINestedCollection(x.asRep[INestedCollection[A]])
    def convertINestedCollection(x : Rep[INestedCollection[A]]): Rep[To] = {
      assert(x.selfType1.isInstanceOf[INestedCollectionElem[_,_]])
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def iNestedCollectionElement[A](implicit eA: Elem[A]) =
    new INestedCollectionElem[A, INestedCollection[A]]()(eA)

  // elem for concrete class
  class UnitCollectionElem(val iso: Iso[UnitCollectionData, UnitCollection])
    extends CollectionElem[Unit, UnitCollection]
    with ViewElem[UnitCollectionData, UnitCollection] {
    override def convertCollection(x: Rep[Collection[Unit]]) = UnitCollection(x.length)
    override def getDefaultRep = super[ViewElem].getDefaultRep
    override lazy val tag = super[ViewElem].tag
  }

  // state representation type
  type UnitCollectionData = Int

  // 3) Iso for concrete class
  class UnitCollectionIso
    extends Iso[UnitCollectionData, UnitCollection] {
    override def from(p: Rep[UnitCollection]) =
      unmkUnitCollection(p) match {
        case Some((length)) => length
        case None => !!!
      }
    override def to(p: Rep[Int]) = {
      val length = p
      UnitCollection(length)
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

    def apply(length: Rep[Int]): Rep[UnitCollection] =
      mkUnitCollection(length)
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
  def mkUnitCollection(length: Rep[Int]): Rep[UnitCollection]
  def unmkUnitCollection(p: Rep[UnitCollection]): Option[(Rep[Int])]

  // elem for concrete class
  class BaseCollectionElem[A](val iso: Iso[BaseCollectionData[A], BaseCollection[A]])(implicit eA: Elem[A])
    extends CollectionElem[A, BaseCollection[A]]
    with ViewElem[BaseCollectionData[A], BaseCollection[A]] {
    override def convertCollection(x: Rep[Collection[A]]) = BaseCollection(x.arr)
    override def getDefaultRep = super[ViewElem].getDefaultRep
    override lazy val tag = super[ViewElem].tag
  }

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
      implicit val tagA = eA.tag
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
  class ListCollectionElem[A](val iso: Iso[ListCollectionData[A], ListCollection[A]])(implicit eA: Elem[A])
    extends CollectionElem[A, ListCollection[A]]
    with ViewElem[ListCollectionData[A], ListCollection[A]] {
    override def convertCollection(x: Rep[Collection[A]]) = ListCollection(x.lst)
    override def getDefaultRep = super[ViewElem].getDefaultRep
    override lazy val tag = super[ViewElem].tag
  }

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
      implicit val tagA = eA.tag
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
  class CollectionOnSeqElem[A](val iso: Iso[CollectionOnSeqData[A], CollectionOnSeq[A]])(implicit eA: Elem[A])
    extends CollectionElem[A, CollectionOnSeq[A]]
    with ViewElem[CollectionOnSeqData[A], CollectionOnSeq[A]] {
    override def convertCollection(x: Rep[Collection[A]]) = CollectionOnSeq(x.seq)
    override def getDefaultRep = super[ViewElem].getDefaultRep
    override lazy val tag = super[ViewElem].tag
  }

  // state representation type
  type CollectionOnSeqData[A] = SSeq[A]

  // 3) Iso for concrete class
  class CollectionOnSeqIso[A](implicit eA: Elem[A])
    extends Iso[CollectionOnSeqData[A], CollectionOnSeq[A]] {
    override def from(p: Rep[CollectionOnSeq[A]]) =
      unmkCollectionOnSeq(p) match {
        case Some((seq)) => seq
        case None => !!!
      }
    override def to(p: Rep[SSeq[A]]) = {
      val seq = p
      CollectionOnSeq(seq)
    }
    lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[CollectionOnSeq[A]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[CollectionOnSeq[A]]](CollectionOnSeq(element[SSeq[A]].defaultRepValue))
    lazy val eTo = new CollectionOnSeqElem[A](this)
  }
  // 4) constructor and deconstructor
  abstract class CollectionOnSeqCompanionAbs extends CompanionBase[CollectionOnSeqCompanionAbs] with CollectionOnSeqCompanion {
    override def toString = "CollectionOnSeq"

    def apply[A](seq: Rep[SSeq[A]])(implicit eA: Elem[A]): Rep[CollectionOnSeq[A]] =
      mkCollectionOnSeq(seq)
    def unapply[A:Elem](p: Rep[CollectionOnSeq[A]]) = unmkCollectionOnSeq(p)
  }
  def CollectionOnSeq: Rep[CollectionOnSeqCompanionAbs]
  implicit def proxyCollectionOnSeqCompanion(p: Rep[CollectionOnSeqCompanionAbs]): CollectionOnSeqCompanionAbs = {
    proxyOps[CollectionOnSeqCompanionAbs](p)
  }

  class CollectionOnSeqCompanionElem extends CompanionElem[CollectionOnSeqCompanionAbs] {
    lazy val tag = weakTypeTag[CollectionOnSeqCompanionAbs]
    protected def getDefaultRep = CollectionOnSeq
  }
  implicit lazy val CollectionOnSeqCompanionElem: CollectionOnSeqCompanionElem = new CollectionOnSeqCompanionElem

  implicit def proxyCollectionOnSeq[A](p: Rep[CollectionOnSeq[A]]): CollectionOnSeq[A] =
    proxyOps[CollectionOnSeq[A]](p)

  implicit class ExtendedCollectionOnSeq[A](p: Rep[CollectionOnSeq[A]])(implicit eA: Elem[A]) {
    def toData: Rep[CollectionOnSeqData[A]] = isoCollectionOnSeq(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoCollectionOnSeq[A](implicit eA: Elem[A]): Iso[CollectionOnSeqData[A], CollectionOnSeq[A]] =
    new CollectionOnSeqIso[A]

  // 6) smart constructor and deconstructor
  def mkCollectionOnSeq[A](seq: Rep[SSeq[A]])(implicit eA: Elem[A]): Rep[CollectionOnSeq[A]]
  def unmkCollectionOnSeq[A:Elem](p: Rep[CollectionOnSeq[A]]): Option[(Rep[SSeq[A]])]

  // elem for concrete class
  class PairCollectionElem[A, B](val iso: Iso[PairCollectionData[A, B], PairCollection[A, B]])(implicit eA: Elem[A], eB: Elem[B])
    extends IPairCollectionElem[A, B, PairCollection[A, B]]
    with ViewElem[PairCollectionData[A, B], PairCollection[A, B]] {
    override def convertIPairCollection(x: Rep[IPairCollection[A, B]]) = PairCollection(x.as, x.bs)
    override def getDefaultRep = super[ViewElem].getDefaultRep
    override lazy val tag = super[ViewElem].tag
  }

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
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
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
  class CollectionOfPairsElem[A, B](val iso: Iso[CollectionOfPairsData[A, B], CollectionOfPairs[A, B]])(implicit eA: Elem[A], eB: Elem[B])
    extends IPairCollectionElem[A, B, CollectionOfPairs[A, B]]
    with ViewElem[CollectionOfPairsData[A, B], CollectionOfPairs[A, B]] {
    override def convertIPairCollection(x: Rep[IPairCollection[A, B]]) = CollectionOfPairs(x.arr)
    override def getDefaultRep = super[ViewElem].getDefaultRep
    override lazy val tag = super[ViewElem].tag
  }

  // state representation type
  type CollectionOfPairsData[A, B] = Array[(A, B)]

  // 3) Iso for concrete class
  class CollectionOfPairsIso[A, B](implicit eA: Elem[A], eB: Elem[B])
    extends Iso[CollectionOfPairsData[A, B], CollectionOfPairs[A, B]] {
    override def from(p: Rep[CollectionOfPairs[A, B]]) =
      unmkCollectionOfPairs(p) match {
        case Some((arr)) => arr
        case None => !!!
      }
    override def to(p: Rep[Array[(A, B)]]) = {
      val arr = p
      CollectionOfPairs(arr)
    }
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[CollectionOfPairs[A, B]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[CollectionOfPairs[A, B]]](CollectionOfPairs(element[Array[(A, B)]].defaultRepValue))
    lazy val eTo = new CollectionOfPairsElem[A, B](this)
  }
  // 4) constructor and deconstructor
  abstract class CollectionOfPairsCompanionAbs extends CompanionBase[CollectionOfPairsCompanionAbs] with CollectionOfPairsCompanion {
    override def toString = "CollectionOfPairs"

    def apply[A, B](arr: Rep[Array[(A, B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[CollectionOfPairs[A, B]] =
      mkCollectionOfPairs(arr)
    def unapply[A:Elem, B:Elem](p: Rep[CollectionOfPairs[A, B]]) = unmkCollectionOfPairs(p)
  }
  def CollectionOfPairs: Rep[CollectionOfPairsCompanionAbs]
  implicit def proxyCollectionOfPairsCompanion(p: Rep[CollectionOfPairsCompanionAbs]): CollectionOfPairsCompanionAbs = {
    proxyOps[CollectionOfPairsCompanionAbs](p)
  }

  class CollectionOfPairsCompanionElem extends CompanionElem[CollectionOfPairsCompanionAbs] {
    lazy val tag = weakTypeTag[CollectionOfPairsCompanionAbs]
    protected def getDefaultRep = CollectionOfPairs
  }
  implicit lazy val CollectionOfPairsCompanionElem: CollectionOfPairsCompanionElem = new CollectionOfPairsCompanionElem

  implicit def proxyCollectionOfPairs[A, B](p: Rep[CollectionOfPairs[A, B]]): CollectionOfPairs[A, B] =
    proxyOps[CollectionOfPairs[A, B]](p)

  implicit class ExtendedCollectionOfPairs[A, B](p: Rep[CollectionOfPairs[A, B]])(implicit eA: Elem[A], eB: Elem[B]) {
    def toData: Rep[CollectionOfPairsData[A, B]] = isoCollectionOfPairs(eA, eB).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoCollectionOfPairs[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[CollectionOfPairsData[A, B], CollectionOfPairs[A, B]] =
    new CollectionOfPairsIso[A, B]

  // 6) smart constructor and deconstructor
  def mkCollectionOfPairs[A, B](arr: Rep[Array[(A, B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[CollectionOfPairs[A, B]]
  def unmkCollectionOfPairs[A:Elem, B:Elem](p: Rep[CollectionOfPairs[A, B]]): Option[(Rep[Array[(A, B)]])]

  // elem for concrete class
  class NestedCollectionElem[A](val iso: Iso[NestedCollectionData[A], NestedCollection[A]])(implicit eA: Elem[A])
    extends INestedCollectionElem[A, NestedCollection[A]]
    with ViewElem[NestedCollectionData[A], NestedCollection[A]] {
    override def convertINestedCollection(x: Rep[INestedCollection[A]]) = NestedCollection(x.values, x.segments)
    override def getDefaultRep = super[ViewElem].getDefaultRep
    override lazy val tag = super[ViewElem].tag
  }

  // state representation type
  type NestedCollectionData[A] = (Collection[A], IPairCollection[Int,Int])

  // 3) Iso for concrete class
  class NestedCollectionIso[A](implicit eA: Elem[A])
    extends Iso[NestedCollectionData[A], NestedCollection[A]] {
    override def from(p: Rep[NestedCollection[A]]) =
      unmkNestedCollection(p) match {
        case Some((values, segments)) => Pair(values, segments)
        case None => !!!
      }
    override def to(p: Rep[(Collection[A], IPairCollection[Int,Int])]) = {
      val Pair(values, segments) = p
      NestedCollection(values, segments)
    }
    lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[NestedCollection[A]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[NestedCollection[A]]](NestedCollection(element[Collection[A]].defaultRepValue, element[IPairCollection[Int,Int]].defaultRepValue))
    lazy val eTo = new NestedCollectionElem[A](this)
  }
  // 4) constructor and deconstructor
  abstract class NestedCollectionCompanionAbs extends CompanionBase[NestedCollectionCompanionAbs] with NestedCollectionCompanion {
    override def toString = "NestedCollection"
    def apply[A](p: Rep[NestedCollectionData[A]])(implicit eA: Elem[A]): Rep[NestedCollection[A]] =
      isoNestedCollection(eA).to(p)
    def apply[A](values: Coll[A], segments: PairColl[Int,Int])(implicit eA: Elem[A]): Rep[NestedCollection[A]] =
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
  def mkNestedCollection[A](values: Coll[A], segments: PairColl[Int,Int])(implicit eA: Elem[A]): Rep[NestedCollection[A]]
  def unmkNestedCollection[A:Elem](p: Rep[NestedCollection[A]]): Option[(Rep[Collection[A]], Rep[IPairCollection[Int,Int]])]
}

// Seq -----------------------------------
trait CollectionsSeq extends CollectionsDsl with ScalanSeq {
  self: ScalanCommunityDslSeq =>
  lazy val Collection: Rep[CollectionCompanionAbs] = new CollectionCompanionAbs with UserTypeSeq[CollectionCompanionAbs, CollectionCompanionAbs] {
    lazy val selfType = element[CollectionCompanionAbs]
  }

  case class SeqUnitCollection
      (override val length: Rep[Int])

    extends UnitCollection(length)
        with UserTypeSeq[Collection[Unit], UnitCollection] {
    lazy val selfType = element[UnitCollection].asInstanceOf[Elem[Collection[Unit]]]
  }
  lazy val UnitCollection = new UnitCollectionCompanionAbs with UserTypeSeq[UnitCollectionCompanionAbs, UnitCollectionCompanionAbs] {
    lazy val selfType = element[UnitCollectionCompanionAbs]
  }

  def mkUnitCollection
      (length: Rep[Int]): Rep[UnitCollection] =
      new SeqUnitCollection(length)
  def unmkUnitCollection(p: Rep[UnitCollection]) =
    Some((p.length))

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
      (arr: Rep[Array[A]])(implicit eA: Elem[A]): Rep[BaseCollection[A]] =
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
      (lst: Rep[List[A]])(implicit eA: Elem[A]): Rep[ListCollection[A]] =
      new SeqListCollection[A](lst)
  def unmkListCollection[A:Elem](p: Rep[ListCollection[A]]) =
    Some((p.lst))

  case class SeqCollectionOnSeq[A]
      (override val seq: Rep[SSeq[A]])
      (implicit eA: Elem[A])
    extends CollectionOnSeq[A](seq)
        with UserTypeSeq[Collection[A], CollectionOnSeq[A]] {
    lazy val selfType = element[CollectionOnSeq[A]].asInstanceOf[Elem[Collection[A]]]
  }
  lazy val CollectionOnSeq = new CollectionOnSeqCompanionAbs with UserTypeSeq[CollectionOnSeqCompanionAbs, CollectionOnSeqCompanionAbs] {
    lazy val selfType = element[CollectionOnSeqCompanionAbs]
  }

  def mkCollectionOnSeq[A]
      (seq: Rep[SSeq[A]])(implicit eA: Elem[A]): Rep[CollectionOnSeq[A]] =
      new SeqCollectionOnSeq[A](seq)
  def unmkCollectionOnSeq[A:Elem](p: Rep[CollectionOnSeq[A]]) =
    Some((p.seq))

  case class SeqPairCollection[A, B]
      (override val as: Rep[Collection[A]], override val bs: Rep[Collection[B]])
      (implicit eA: Elem[A], eB: Elem[B])
    extends PairCollection[A, B](as, bs)
        with UserTypeSeq[IPairCollection[A,B], PairCollection[A, B]] {
    lazy val selfType = element[PairCollection[A, B]].asInstanceOf[Elem[IPairCollection[A,B]]]
  }
  lazy val PairCollection = new PairCollectionCompanionAbs with UserTypeSeq[PairCollectionCompanionAbs, PairCollectionCompanionAbs] {
    lazy val selfType = element[PairCollectionCompanionAbs]
  }

  def mkPairCollection[A, B]
      (as: Rep[Collection[A]], bs: Rep[Collection[B]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairCollection[A, B]] =
      new SeqPairCollection[A, B](as, bs)
  def unmkPairCollection[A:Elem, B:Elem](p: Rep[PairCollection[A, B]]) =
    Some((p.as, p.bs))

  case class SeqCollectionOfPairs[A, B]
      (override val arr: Rep[Array[(A, B)]])
      (implicit eA: Elem[A], eB: Elem[B])
    extends CollectionOfPairs[A, B](arr)
        with UserTypeSeq[IPairCollection[A,B], CollectionOfPairs[A, B]] {
    lazy val selfType = element[CollectionOfPairs[A, B]].asInstanceOf[Elem[IPairCollection[A,B]]]
  }
  lazy val CollectionOfPairs = new CollectionOfPairsCompanionAbs with UserTypeSeq[CollectionOfPairsCompanionAbs, CollectionOfPairsCompanionAbs] {
    lazy val selfType = element[CollectionOfPairsCompanionAbs]
  }

  def mkCollectionOfPairs[A, B]
      (arr: Rep[Array[(A, B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[CollectionOfPairs[A, B]] =
      new SeqCollectionOfPairs[A, B](arr)
  def unmkCollectionOfPairs[A:Elem, B:Elem](p: Rep[CollectionOfPairs[A, B]]) =
    Some((p.arr))

  case class SeqNestedCollection[A]
      (override val values: Coll[A], override val segments: PairColl[Int,Int])
      (implicit eA: Elem[A])
    extends NestedCollection[A](values, segments)
        with UserTypeSeq[INestedCollection[A], NestedCollection[A]] {
    lazy val selfType = element[NestedCollection[A]].asInstanceOf[Elem[INestedCollection[A]]]
  }
  lazy val NestedCollection = new NestedCollectionCompanionAbs with UserTypeSeq[NestedCollectionCompanionAbs, NestedCollectionCompanionAbs] {
    lazy val selfType = element[NestedCollectionCompanionAbs]
  }

  def mkNestedCollection[A]
      (values: Coll[A], segments: PairColl[Int,Int])(implicit eA: Elem[A]): Rep[NestedCollection[A]] =
      new SeqNestedCollection[A](values, segments)
  def unmkNestedCollection[A:Elem](p: Rep[NestedCollection[A]]) =
    Some((p.values, p.segments))
}

// Exp -----------------------------------
trait CollectionsExp extends CollectionsDsl with ScalanExp {
  self: ScalanCommunityDslExp =>
  lazy val Collection: Rep[CollectionCompanionAbs] = new CollectionCompanionAbs with UserTypeDef[CollectionCompanionAbs, CollectionCompanionAbs] {
    lazy val selfType = element[CollectionCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpUnitCollection
      (override val length: Rep[Int])

    extends UnitCollection(length) with UserTypeDef[Collection[Unit], UnitCollection] {
    lazy val selfType = element[UnitCollection].asInstanceOf[Elem[Collection[Unit]]]
    override def mirror(t: Transformer) = ExpUnitCollection(t(length))
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

    object lst {
      def unapply(d: Def[_]): Option[Rep[UnitCollection]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[UnitCollectionElem] && method.getName == "lst" =>
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
      def unapply(d: Def[_]): Option[(Rep[UnitCollection], Coll[Int])] = d match {
        case MethodCall(receiver, method, Seq(indices, _*), _) if receiver.elem.isInstanceOf[UnitCollectionElem] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[UnitCollection], Coll[Int])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[UnitCollection], Coll[Int])] = exp match {
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

    object update {
      def unapply(d: Def[_]): Option[(Rep[UnitCollection], Rep[Int], Rep[Unit])] = d match {
        case MethodCall(receiver, method, Seq(idx, value, _*), _) if receiver.elem.isInstanceOf[UnitCollectionElem] && method.getName == "update" =>
          Some((receiver, idx, value)).asInstanceOf[Option[(Rep[UnitCollection], Rep[Int], Rep[Unit])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[UnitCollection], Rep[Int], Rep[Unit])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object updateMany {
      def unapply(d: Def[_]): Option[(Rep[UnitCollection], Coll[Int], Coll[Unit])] = d match {
        case MethodCall(receiver, method, Seq(idxs, vals, _*), _) if receiver.elem.isInstanceOf[UnitCollectionElem] && method.getName == "updateMany" =>
          Some((receiver, idxs, vals)).asInstanceOf[Option[(Rep[UnitCollection], Coll[Int], Coll[Unit])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[UnitCollection], Coll[Int], Coll[Unit])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[UnitCollection], Rep[Unit => Boolean])] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[UnitCollectionElem] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[UnitCollection], Rep[Unit => Boolean])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[UnitCollection], Rep[Unit => Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMapBy {
      def unapply(d: Def[_]): Option[(Rep[UnitCollection], Rep[Unit => Collection[B]]) forSome {type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[UnitCollectionElem] && method.getName == "flatMapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[UnitCollection], Rep[Unit => Collection[B]]) forSome {type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[UnitCollection], Rep[Unit => Collection[B]]) forSome {type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object append {
      def unapply(d: Def[_]): Option[(Rep[UnitCollection], Rep[Unit])] = d match {
        case MethodCall(receiver, method, Seq(value, _*), _) if receiver.elem.isInstanceOf[UnitCollectionElem] && method.getName == "append" =>
          Some((receiver, value)).asInstanceOf[Option[(Rep[UnitCollection], Rep[Unit])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[UnitCollection], Rep[Unit])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object UnitCollectionCompanionMethods {
  }

  def mkUnitCollection
    (length: Rep[Int]): Rep[UnitCollection] =
    new ExpUnitCollection(length)
  def unmkUnitCollection(p: Rep[UnitCollection]) =
    Some((p.length))

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

    object lst {
      def unapply(d: Def[_]): Option[Rep[BaseCollection[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BaseCollectionElem[_]] && method.getName == "lst" =>
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
      def unapply(d: Def[_]): Option[(Rep[BaseCollection[A]], Coll[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(indices, _*), _) if receiver.elem.isInstanceOf[BaseCollectionElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[BaseCollection[A]], Coll[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BaseCollection[A]], Coll[Int]) forSome {type A}] = exp match {
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

    object update {
      def unapply(d: Def[_]): Option[(Rep[BaseCollection[A]], Rep[Int], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(idx, value, _*), _) if receiver.elem.isInstanceOf[BaseCollectionElem[_]] && method.getName == "update" =>
          Some((receiver, idx, value)).asInstanceOf[Option[(Rep[BaseCollection[A]], Rep[Int], Rep[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BaseCollection[A]], Rep[Int], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object updateMany {
      def unapply(d: Def[_]): Option[(Rep[BaseCollection[A]], Coll[Int], Coll[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(idxs, vals, _*), _) if receiver.elem.isInstanceOf[BaseCollectionElem[_]] && method.getName == "updateMany" =>
          Some((receiver, idxs, vals)).asInstanceOf[Option[(Rep[BaseCollection[A]], Coll[Int], Coll[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BaseCollection[A]], Coll[Int], Coll[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[BaseCollection[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[BaseCollectionElem[_]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[BaseCollection[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BaseCollection[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMapBy {
      def unapply(d: Def[_]): Option[(Rep[BaseCollection[A]], Rep[A => Collection[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[BaseCollectionElem[_]] && method.getName == "flatMapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[BaseCollection[A]], Rep[A => Collection[B]]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BaseCollection[A]], Rep[A => Collection[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object append {
      def unapply(d: Def[_]): Option[(Rep[BaseCollection[A]], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(value, _*), _) if receiver.elem.isInstanceOf[BaseCollectionElem[_]] && method.getName == "append" =>
          Some((receiver, value)).asInstanceOf[Option[(Rep[BaseCollection[A]], Rep[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BaseCollection[A]], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object BaseCollectionCompanionMethods {
  }

  def mkBaseCollection[A]
    (arr: Rep[Array[A]])(implicit eA: Elem[A]): Rep[BaseCollection[A]] =
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
      def unapply(d: Def[_]): Option[(Rep[ListCollection[A]], Coll[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(indices, _*), _) if receiver.elem.isInstanceOf[ListCollectionElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[ListCollection[A]], Coll[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ListCollection[A]], Coll[Int]) forSome {type A}] = exp match {
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

    object update {
      def unapply(d: Def[_]): Option[(Rep[ListCollection[A]], Rep[Int], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(idx, value, _*), _) if receiver.elem.isInstanceOf[ListCollectionElem[_]] && method.getName == "update" =>
          Some((receiver, idx, value)).asInstanceOf[Option[(Rep[ListCollection[A]], Rep[Int], Rep[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ListCollection[A]], Rep[Int], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object updateMany {
      def unapply(d: Def[_]): Option[(Rep[ListCollection[A]], Coll[Int], Coll[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(idxs, vals, _*), _) if receiver.elem.isInstanceOf[ListCollectionElem[_]] && method.getName == "updateMany" =>
          Some((receiver, idxs, vals)).asInstanceOf[Option[(Rep[ListCollection[A]], Coll[Int], Coll[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ListCollection[A]], Coll[Int], Coll[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[ListCollection[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[ListCollectionElem[_]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[ListCollection[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ListCollection[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMapBy {
      def unapply(d: Def[_]): Option[(Rep[ListCollection[A]], Rep[A => Collection[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[ListCollectionElem[_]] && method.getName == "flatMapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[ListCollection[A]], Rep[A => Collection[B]]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ListCollection[A]], Rep[A => Collection[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object append {
      def unapply(d: Def[_]): Option[(Rep[ListCollection[A]], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(value, _*), _) if receiver.elem.isInstanceOf[ListCollectionElem[_]] && method.getName == "append" =>
          Some((receiver, value)).asInstanceOf[Option[(Rep[ListCollection[A]], Rep[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ListCollection[A]], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ListCollectionCompanionMethods {
  }

  def mkListCollection[A]
    (lst: Rep[List[A]])(implicit eA: Elem[A]): Rep[ListCollection[A]] =
    new ExpListCollection[A](lst)
  def unmkListCollection[A:Elem](p: Rep[ListCollection[A]]) =
    Some((p.lst))

  case class ExpCollectionOnSeq[A]
      (override val seq: Rep[SSeq[A]])
      (implicit eA: Elem[A])
    extends CollectionOnSeq[A](seq) with UserTypeDef[Collection[A], CollectionOnSeq[A]] {
    lazy val selfType = element[CollectionOnSeq[A]].asInstanceOf[Elem[Collection[A]]]
    override def mirror(t: Transformer) = ExpCollectionOnSeq[A](t(seq))
  }

  lazy val CollectionOnSeq: Rep[CollectionOnSeqCompanionAbs] = new CollectionOnSeqCompanionAbs with UserTypeDef[CollectionOnSeqCompanionAbs, CollectionOnSeqCompanionAbs] {
    lazy val selfType = element[CollectionOnSeqCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object CollectionOnSeqMethods {
    object elem {
      def unapply(d: Def[_]): Option[Rep[CollectionOnSeq[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionOnSeqElem[_]] && method.getName == "elem" =>
          Some(receiver).asInstanceOf[Option[Rep[CollectionOnSeq[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CollectionOnSeq[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object arr {
      def unapply(d: Def[_]): Option[Rep[CollectionOnSeq[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionOnSeqElem[_]] && method.getName == "arr" =>
          Some(receiver).asInstanceOf[Option[Rep[CollectionOnSeq[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CollectionOnSeq[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object lst {
      def unapply(d: Def[_]): Option[Rep[CollectionOnSeq[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionOnSeqElem[_]] && method.getName == "lst" =>
          Some(receiver).asInstanceOf[Option[Rep[CollectionOnSeq[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CollectionOnSeq[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[CollectionOnSeq[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionOnSeqElem[_]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[CollectionOnSeq[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CollectionOnSeq[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[CollectionOnSeq[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[CollectionOnSeqElem[_]] && method.getName == "apply"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[CollectionOnSeq[A]], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOnSeq[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object slice {
      def unapply(d: Def[_]): Option[(Rep[CollectionOnSeq[A]], Rep[Int], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(offset, length, _*), _) if receiver.elem.isInstanceOf[CollectionOnSeqElem[_]] && method.getName == "slice" =>
          Some((receiver, offset, length)).asInstanceOf[Option[(Rep[CollectionOnSeq[A]], Rep[Int], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOnSeq[A]], Rep[Int], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_many {
      def unapply(d: Def[_]): Option[(Rep[CollectionOnSeq[A]], Coll[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(indices, _*), _) if receiver.elem.isInstanceOf[CollectionOnSeqElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[CollectionOnSeq[A]], Coll[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOnSeq[A]], Coll[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[CollectionOnSeq[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CollectionOnSeqElem[_]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[CollectionOnSeq[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOnSeq[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[CollectionOnSeq[A]], RepMonoid[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[CollectionOnSeqElem[_]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[CollectionOnSeq[A]], RepMonoid[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOnSeq[A]], RepMonoid[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zip {
      def unapply(d: Def[_]): Option[(Rep[CollectionOnSeq[A]], Coll[B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(ys, _*), _) if receiver.elem.isInstanceOf[CollectionOnSeqElem[_]] && method.getName == "zip" =>
          Some((receiver, ys)).asInstanceOf[Option[(Rep[CollectionOnSeq[A]], Coll[B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOnSeq[A]], Coll[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object update {
      def unapply(d: Def[_]): Option[(Rep[CollectionOnSeq[A]], Rep[Int], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(idx, value, _*), _) if receiver.elem.isInstanceOf[CollectionOnSeqElem[_]] && method.getName == "update" =>
          Some((receiver, idx, value)).asInstanceOf[Option[(Rep[CollectionOnSeq[A]], Rep[Int], Rep[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOnSeq[A]], Rep[Int], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object updateMany {
      def unapply(d: Def[_]): Option[(Rep[CollectionOnSeq[A]], Coll[Int], Coll[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(idxs, vals, _*), _) if receiver.elem.isInstanceOf[CollectionOnSeqElem[_]] && method.getName == "updateMany" =>
          Some((receiver, idxs, vals)).asInstanceOf[Option[(Rep[CollectionOnSeq[A]], Coll[Int], Coll[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOnSeq[A]], Coll[Int], Coll[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[CollectionOnSeq[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CollectionOnSeqElem[_]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[CollectionOnSeq[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOnSeq[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMapBy {
      def unapply(d: Def[_]): Option[(Rep[CollectionOnSeq[A]], Rep[A => Collection[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CollectionOnSeqElem[_]] && method.getName == "flatMapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[CollectionOnSeq[A]], Rep[A => Collection[B]]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOnSeq[A]], Rep[A => Collection[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object append {
      def unapply(d: Def[_]): Option[(Rep[CollectionOnSeq[A]], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(value, _*), _) if receiver.elem.isInstanceOf[CollectionOnSeqElem[_]] && method.getName == "append" =>
          Some((receiver, value)).asInstanceOf[Option[(Rep[CollectionOnSeq[A]], Rep[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOnSeq[A]], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CollectionOnSeqCompanionMethods {
  }

  def mkCollectionOnSeq[A]
    (seq: Rep[SSeq[A]])(implicit eA: Elem[A]): Rep[CollectionOnSeq[A]] =
    new ExpCollectionOnSeq[A](seq)
  def unmkCollectionOnSeq[A:Elem](p: Rep[CollectionOnSeq[A]]) =
    Some((p.seq))

  case class ExpPairCollection[A, B]
      (override val as: Rep[Collection[A]], override val bs: Rep[Collection[B]])
      (implicit eA: Elem[A], eB: Elem[B])
    extends PairCollection[A, B](as, bs) with UserTypeDef[IPairCollection[A,B], PairCollection[A, B]] {
    lazy val selfType = element[PairCollection[A, B]].asInstanceOf[Elem[IPairCollection[A,B]]]
    override def mirror(t: Transformer) = ExpPairCollection[A, B](t(as), t(bs))
  }

  lazy val PairCollection: Rep[PairCollectionCompanionAbs] = new PairCollectionCompanionAbs with UserTypeDef[PairCollectionCompanionAbs, PairCollectionCompanionAbs] {
    lazy val selfType = element[PairCollectionCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object PairCollectionMethods {
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

    object lst {
      def unapply(d: Def[_]): Option[Rep[PairCollection[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairCollectionElem[_, _]] && method.getName == "lst" =>
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
      def unapply(d: Def[_]): Option[(Rep[PairCollection[A, B]], Coll[Int]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(indices, _*), _) if receiver.elem.isInstanceOf[PairCollectionElem[_, _]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[PairCollection[A, B]], Coll[Int]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollection[A, B]], Coll[Int]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[PairCollection[A, B]], Rep[((A, B)) => C]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[PairCollectionElem[_, _]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[PairCollection[A, B]], Rep[((A, B)) => C]) forSome {type A; type B; type C}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollection[A, B]], Rep[((A, B)) => C]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[PairCollection[A, B]], RepMonoid[(A, B)]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[PairCollectionElem[_, _]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[PairCollection[A, B]], RepMonoid[(A, B)]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollection[A, B]], RepMonoid[(A, B)]) forSome {type A; type B}] = exp match {
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

    object update {
      def unapply(d: Def[_]): Option[(Rep[PairCollection[A, B]], Rep[Int], Rep[(A, B)]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(idx, value, _*), _) if receiver.elem.isInstanceOf[PairCollectionElem[_, _]] && method.getName == "update" =>
          Some((receiver, idx, value)).asInstanceOf[Option[(Rep[PairCollection[A, B]], Rep[Int], Rep[(A, B)]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollection[A, B]], Rep[Int], Rep[(A, B)]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object updateMany {
      def unapply(d: Def[_]): Option[(Rep[PairCollection[A, B]], Coll[Int], Coll[(A, B)]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(idxs, vals, _*), _) if receiver.elem.isInstanceOf[PairCollectionElem[_, _]] && method.getName == "updateMany" =>
          Some((receiver, idxs, vals)).asInstanceOf[Option[(Rep[PairCollection[A, B]], Coll[Int], Coll[(A, B)]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollection[A, B]], Coll[Int], Coll[(A, B)]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[PairCollection[A, B]], Rep[((A, B)) => Boolean]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[PairCollectionElem[_, _]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[PairCollection[A, B]], Rep[((A, B)) => Boolean]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollection[A, B]], Rep[((A, B)) => Boolean]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMapBy {
      def unapply(d: Def[_]): Option[(Rep[PairCollection[A, B]], Rep[((A, B)) => Collection[C]]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[PairCollectionElem[_, _]] && method.getName == "flatMapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[PairCollection[A, B]], Rep[((A, B)) => Collection[C]]) forSome {type A; type B; type C}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollection[A, B]], Rep[((A, B)) => Collection[C]]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object append {
      def unapply(d: Def[_]): Option[(Rep[PairCollection[A, B]], Rep[(A, B)]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(value, _*), _) if receiver.elem.isInstanceOf[PairCollectionElem[_, _]] && method.getName == "append" =>
          Some((receiver, value)).asInstanceOf[Option[(Rep[PairCollection[A, B]], Rep[(A, B)]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollection[A, B]], Rep[(A, B)]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object PairCollectionCompanionMethods {
  }

  def mkPairCollection[A, B]
    (as: Rep[Collection[A]], bs: Rep[Collection[B]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairCollection[A, B]] =
    new ExpPairCollection[A, B](as, bs)
  def unmkPairCollection[A:Elem, B:Elem](p: Rep[PairCollection[A, B]]) =
    Some((p.as, p.bs))

  case class ExpCollectionOfPairs[A, B]
      (override val arr: Rep[Array[(A, B)]])
      (implicit eA: Elem[A], eB: Elem[B])
    extends CollectionOfPairs[A, B](arr) with UserTypeDef[IPairCollection[A,B], CollectionOfPairs[A, B]] {
    lazy val selfType = element[CollectionOfPairs[A, B]].asInstanceOf[Elem[IPairCollection[A,B]]]
    override def mirror(t: Transformer) = ExpCollectionOfPairs[A, B](t(arr))
  }

  lazy val CollectionOfPairs: Rep[CollectionOfPairsCompanionAbs] = new CollectionOfPairsCompanionAbs with UserTypeDef[CollectionOfPairsCompanionAbs, CollectionOfPairsCompanionAbs] {
    lazy val selfType = element[CollectionOfPairsCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object CollectionOfPairsMethods {
    object lst {
      def unapply(d: Def[_]): Option[Rep[CollectionOfPairs[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionOfPairsElem[_, _]] && method.getName == "lst" =>
          Some(receiver).asInstanceOf[Option[Rep[CollectionOfPairs[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CollectionOfPairs[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object as {
      def unapply(d: Def[_]): Option[Rep[CollectionOfPairs[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionOfPairsElem[_, _]] && method.getName == "as" =>
          Some(receiver).asInstanceOf[Option[Rep[CollectionOfPairs[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CollectionOfPairs[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object bs {
      def unapply(d: Def[_]): Option[Rep[CollectionOfPairs[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionOfPairsElem[_, _]] && method.getName == "bs" =>
          Some(receiver).asInstanceOf[Option[Rep[CollectionOfPairs[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CollectionOfPairs[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[CollectionOfPairs[A, B]], Rep[Int]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[CollectionOfPairsElem[_, _]] && method.getName == "apply"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[CollectionOfPairs[A, B]], Rep[Int]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOfPairs[A, B]], Rep[Int]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[CollectionOfPairs[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionOfPairsElem[_, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[CollectionOfPairs[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CollectionOfPairs[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object slice {
      def unapply(d: Def[_]): Option[(Rep[CollectionOfPairs[A, B]], Rep[Int], Rep[Int]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(offset, length, _*), _) if receiver.elem.isInstanceOf[CollectionOfPairsElem[_, _]] && method.getName == "slice" =>
          Some((receiver, offset, length)).asInstanceOf[Option[(Rep[CollectionOfPairs[A, B]], Rep[Int], Rep[Int]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOfPairs[A, B]], Rep[Int], Rep[Int]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_many {
      def unapply(d: Def[_]): Option[(Rep[CollectionOfPairs[A, B]], Coll[Int]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(indices, _*), _) if receiver.elem.isInstanceOf[CollectionOfPairsElem[_, _]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[CollectionOfPairs[A, B]], Coll[Int]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOfPairs[A, B]], Coll[Int]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[CollectionOfPairs[A, B]], Rep[((A, B)) => C]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CollectionOfPairsElem[_, _]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[CollectionOfPairs[A, B]], Rep[((A, B)) => C]) forSome {type A; type B; type C}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOfPairs[A, B]], Rep[((A, B)) => C]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[CollectionOfPairs[A, B]], RepMonoid[(A, B)]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[CollectionOfPairsElem[_, _]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[CollectionOfPairs[A, B]], RepMonoid[(A, B)]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOfPairs[A, B]], RepMonoid[(A, B)]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zip {
      def unapply(d: Def[_]): Option[(Rep[CollectionOfPairs[A, B]], Coll[C]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, Seq(ys, _*), _) if receiver.elem.isInstanceOf[CollectionOfPairsElem[_, _]] && method.getName == "zip" =>
          Some((receiver, ys)).asInstanceOf[Option[(Rep[CollectionOfPairs[A, B]], Coll[C]) forSome {type A; type B; type C}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOfPairs[A, B]], Coll[C]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object update {
      def unapply(d: Def[_]): Option[(Rep[CollectionOfPairs[A, B]], Rep[Int], Rep[(A, B)]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(idx, value, _*), _) if receiver.elem.isInstanceOf[CollectionOfPairsElem[_, _]] && method.getName == "update" =>
          Some((receiver, idx, value)).asInstanceOf[Option[(Rep[CollectionOfPairs[A, B]], Rep[Int], Rep[(A, B)]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOfPairs[A, B]], Rep[Int], Rep[(A, B)]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object updateMany {
      def unapply(d: Def[_]): Option[(Rep[CollectionOfPairs[A, B]], Coll[Int], Coll[(A, B)]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(idxs, vals, _*), _) if receiver.elem.isInstanceOf[CollectionOfPairsElem[_, _]] && method.getName == "updateMany" =>
          Some((receiver, idxs, vals)).asInstanceOf[Option[(Rep[CollectionOfPairs[A, B]], Coll[Int], Coll[(A, B)]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOfPairs[A, B]], Coll[Int], Coll[(A, B)]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[CollectionOfPairs[A, B]], Rep[((A, B)) => Boolean]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CollectionOfPairsElem[_, _]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[CollectionOfPairs[A, B]], Rep[((A, B)) => Boolean]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOfPairs[A, B]], Rep[((A, B)) => Boolean]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMapBy {
      def unapply(d: Def[_]): Option[(Rep[CollectionOfPairs[A, B]], Rep[((A, B)) => Collection[C]]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CollectionOfPairsElem[_, _]] && method.getName == "flatMapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[CollectionOfPairs[A, B]], Rep[((A, B)) => Collection[C]]) forSome {type A; type B; type C}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOfPairs[A, B]], Rep[((A, B)) => Collection[C]]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object append {
      def unapply(d: Def[_]): Option[(Rep[CollectionOfPairs[A, B]], Rep[(A, B)]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(value, _*), _) if receiver.elem.isInstanceOf[CollectionOfPairsElem[_, _]] && method.getName == "append" =>
          Some((receiver, value)).asInstanceOf[Option[(Rep[CollectionOfPairs[A, B]], Rep[(A, B)]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOfPairs[A, B]], Rep[(A, B)]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CollectionOfPairsCompanionMethods {
  }

  def mkCollectionOfPairs[A, B]
    (arr: Rep[Array[(A, B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[CollectionOfPairs[A, B]] =
    new ExpCollectionOfPairs[A, B](arr)
  def unmkCollectionOfPairs[A:Elem, B:Elem](p: Rep[CollectionOfPairs[A, B]]) =
    Some((p.arr))

  case class ExpNestedCollection[A]
      (override val values: Coll[A], override val segments: PairColl[Int,Int])
      (implicit eA: Elem[A])
    extends NestedCollection[A](values, segments) with UserTypeDef[INestedCollection[A], NestedCollection[A]] {
    lazy val selfType = element[NestedCollection[A]].asInstanceOf[Elem[INestedCollection[A]]]
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

    object lst {
      def unapply(d: Def[_]): Option[Rep[NestedCollection[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[NestedCollectionElem[_]] && method.getName == "lst" =>
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
      def unapply(d: Def[_]): Option[(Rep[NestedCollection[A]], Coll[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(indices, _*), _) if receiver.elem.isInstanceOf[NestedCollectionElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[NestedCollection[A]], Coll[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollection[A]], Coll[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

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

    object update {
      def unapply(d: Def[_]): Option[(Rep[NestedCollection[A]], Rep[Int], Rep[Collection[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(idx, value, _*), _) if receiver.elem.isInstanceOf[NestedCollectionElem[_]] && method.getName == "update" =>
          Some((receiver, idx, value)).asInstanceOf[Option[(Rep[NestedCollection[A]], Rep[Int], Rep[Collection[A]]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollection[A]], Rep[Int], Rep[Collection[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object updateMany {
      def unapply(d: Def[_]): Option[(Rep[NestedCollection[A]], Coll[Int], Coll[Collection[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(idxs, vals, _*), _) if receiver.elem.isInstanceOf[NestedCollectionElem[_]] && method.getName == "updateMany" =>
          Some((receiver, idxs, vals)).asInstanceOf[Option[(Rep[NestedCollection[A]], Coll[Int], Coll[Collection[A]]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollection[A]], Coll[Int], Coll[Collection[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[NestedCollection[A]], Rep[Collection[A] => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[NestedCollectionElem[_]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[NestedCollection[A]], Rep[Collection[A] => Boolean]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollection[A]], Rep[Collection[A] => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMapBy {
      def unapply(d: Def[_]): Option[(Rep[NestedCollection[A]], Rep[Collection[A] => Collection[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[NestedCollectionElem[_]] && method.getName == "flatMapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[NestedCollection[A]], Rep[Collection[A] => Collection[B]]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollection[A]], Rep[Collection[A] => Collection[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object append {
      def unapply(d: Def[_]): Option[(Rep[NestedCollection[A]], Rep[Collection[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(value, _*), _) if receiver.elem.isInstanceOf[NestedCollectionElem[_]] && method.getName == "append" =>
          Some((receiver, value)).asInstanceOf[Option[(Rep[NestedCollection[A]], Rep[Collection[A]]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollection[A]], Rep[Collection[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object NestedCollectionCompanionMethods {
    object fromJuggedArray {
      def unapply(d: Def[_]): Option[Rep[Array[Array[T]]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(arr, _*), _) if receiver.elem.isInstanceOf[NestedCollectionCompanionElem] && method.getName == "fromJuggedArray" =>
          Some(arr).asInstanceOf[Option[Rep[Array[Array[T]]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Array[Array[T]]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkNestedCollection[A]
    (values: Coll[A], segments: PairColl[Int,Int])(implicit eA: Elem[A]): Rep[NestedCollection[A]] =
    new ExpNestedCollection[A](values, segments)
  def unmkNestedCollection[A:Elem](p: Rep[NestedCollection[A]]) =
    Some((p.values, p.segments))

  object CollectionMethods {
    object length {
      def unapply(d: Def[_]): Option[Rep[Collection[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "length" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "arr" =>
          Some(receiver).asInstanceOf[Option[Rep[Collection[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Collection[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object lst {
      def unapply(d: Def[_]): Option[Rep[Collection[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "lst" =>
          Some(receiver).asInstanceOf[Option[Rep[Collection[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Collection[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object seq {
      def unapply(d: Def[_]): Option[Rep[Collection[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "seq" =>
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
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "apply"&& method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[Collection[A]], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_many {
      def unapply(d: Def[_]): Option[(Rep[Collection[A]], Coll[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(indices, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[Collection[A]], Coll[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[A]], Coll[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[Collection[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "mapBy" =>
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
        case MethodCall(receiver, method, Seq(ys, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "zip" =>
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
        case MethodCall(receiver, method, Seq(offset, length, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "slice" =>
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
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[Collection[A]], RepMonoid[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[A]], RepMonoid[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object update {
      def unapply(d: Def[_]): Option[(Rep[Collection[A]], Rep[Int], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(idx, value, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "update" =>
          Some((receiver, idx, value)).asInstanceOf[Option[(Rep[Collection[A]], Rep[Int], Rep[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[A]], Rep[Int], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object updateMany {
      def unapply(d: Def[_]): Option[(Rep[Collection[A]], Coll[Int], Coll[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(idxs, vals, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "updateMany" =>
          Some((receiver, idxs, vals)).asInstanceOf[Option[(Rep[Collection[A]], Coll[Int], Coll[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[A]], Coll[Int], Coll[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object indexes {
      def unapply(d: Def[_]): Option[Rep[Collection[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "indexes" =>
          Some(receiver).asInstanceOf[Option[Rep[Collection[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Collection[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[Collection[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Collection[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMapBy {
      def unapply(d: Def[_]): Option[(Rep[Collection[A]], Rep[A => Collection[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "flatMapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Collection[A]], Rep[A => Collection[B]]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[A]], Rep[A => Collection[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object append {
      def unapply(d: Def[_]): Option[(Rep[Collection[A]], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(value, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "append" =>
          Some((receiver, value)).asInstanceOf[Option[(Rep[Collection[A]], Rep[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[A]], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CollectionCompanionMethods {
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

    object indexRange {
      def unapply(d: Def[_]): Option[Rep[Int]] = d match {
        case MethodCall(receiver, method, Seq(l, _*), _) if receiver.elem.isInstanceOf[CollectionCompanionElem] && method.getName == "indexRange" =>
          Some(l).asInstanceOf[Option[Rep[Int]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Int]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}
