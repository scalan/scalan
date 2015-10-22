package scalan.collections

import scala.annotation.unchecked.uncheckedVariance
import scalan._
import scalan.arrays.ArrayOps
import scalan.common.OverloadHack.Overloaded1
import scala.collection.mutable
import scala.annotation.tailrec
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait CollectionsAbs extends Collections with scalan.Scalan {
  self: ScalanCommunityDsl =>

  // single proxy for each type family
  implicit def proxyCollection[Item](p: Rep[Collection[Item]]): Collection[Item] = {
    proxyOps[Collection[Item]](p)(scala.reflect.classTag[Collection[Item]])
  }

  // familyElem
  class CollectionElem[Item, To <: Collection[Item]](implicit _eItem: Elem[Item @uncheckedVariance])
    extends EntityElem[To] {
    def eItem = _eItem
    lazy val parent: Option[Elem[_]] = None
    lazy val entityDef: STraitOrClassDef = {
      val module = getModules("Collections")
      module.entities.find(_.name == "Collection").get
    }
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("Item" -> Left(eItem))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagAnnotatedItem = eItem.tag
      weakTypeTag[Collection[Item]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Collection[Item]] => convertCollection(x) }
      tryConvert(element[Collection[Item]], this, x, conv)
    }

    def convertCollection(x: Rep[Collection[Item]]): Rep[To] = {
      x.selfType1 match {
        case _: CollectionElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have CollectionElem[_, _], but got $e")
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def collectionElement[Item](implicit eItem: Elem[Item @uncheckedVariance]): Elem[Collection[Item]] =
    cachedElem[CollectionElem[Item, Collection[Item]]](eItem)

  implicit case object CollectionCompanionElem extends CompanionElem[CollectionCompanionAbs] {
    lazy val tag = weakTypeTag[CollectionCompanionAbs]
    protected def getDefaultRep = Collection
  }

  abstract class CollectionCompanionAbs extends CompanionDef[CollectionCompanionAbs] with CollectionCompanion {
    def selfType = CollectionCompanionElem
    override def toString = "Collection"
  }
  def Collection: Rep[CollectionCompanionAbs]
  implicit def proxyCollectionCompanion(p: Rep[CollectionCompanion]): CollectionCompanion =
    proxyOps[CollectionCompanion](p)

  // single proxy for each type family
  implicit def proxyPairCollection[A, B](p: Rep[PairCollection[A, B]]): PairCollection[A, B] = {
    proxyOps[PairCollection[A, B]](p)(scala.reflect.classTag[PairCollection[A, B]])
  }
  // familyElem
  class PairCollectionElem[A, B, To <: PairCollection[A, B]](implicit _eA: Elem[A], _eB: Elem[B])
    extends CollectionElem[(A, B), To] {
    def eA = _eA
    def eB = _eB
    override lazy val parent: Option[Elem[_]] = Some(collectionElement(pairElement(element[A],element[B])))
    override lazy val entityDef: STraitOrClassDef = {
      val module = getModules("Collections")
      module.entities.find(_.name == "PairCollection").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA), "B" -> Left(eB))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[PairCollection[A, B]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[PairCollection[A, B]] => convertPairCollection(x) }
      tryConvert(element[PairCollection[A, B]], this, x, conv)
    }

    def convertPairCollection(x: Rep[PairCollection[A, B]]): Rep[To] = {
      x.selfType1 match {
        case _: PairCollectionElem[_, _, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have PairCollectionElem[_, _, _], but got $e")
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def pairCollectionElement[A, B](implicit eA: Elem[A], eB: Elem[B]): Elem[PairCollection[A, B]] =
    cachedElem[PairCollectionElem[A, B, PairCollection[A, B]]](eA, eB)

  // single proxy for each type family
  implicit def proxyNestedCollection[A](p: Rep[NestedCollection[A]]): NestedCollection[A] = {
    proxyOps[NestedCollection[A]](p)(scala.reflect.classTag[NestedCollection[A]])
  }
  // familyElem
  class NestedCollectionElem[A, To <: NestedCollection[A]](implicit _eA: Elem[A])
    extends CollectionElem[Collection[A], To] {
    def eA = _eA
    override lazy val parent: Option[Elem[_]] = Some(collectionElement(collectionElement(element[A])))
    override lazy val entityDef: STraitOrClassDef = {
      val module = getModules("Collections")
      module.entities.find(_.name == "NestedCollection").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[NestedCollection[A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[NestedCollection[A]] => convertNestedCollection(x) }
      tryConvert(element[NestedCollection[A]], this, x, conv)
    }

    def convertNestedCollection(x: Rep[NestedCollection[A]]): Rep[To] = {
      x.selfType1 match {
        case _: NestedCollectionElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have NestedCollectionElem[_, _], but got $e")
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def nestedCollectionElement[A](implicit eA: Elem[A]): Elem[NestedCollection[A]] =
    cachedElem[NestedCollectionElem[A, NestedCollection[A]]](eA)

  abstract class AbsUnitCollection
      (length: Rep[Int])
    extends UnitCollection(length) with Def[UnitCollection] {
    lazy val selfType = element[UnitCollection]
  }
  // elem for concrete class
  class UnitCollectionElem(val iso: Iso[UnitCollectionData, UnitCollection])
    extends CollectionElem[Unit, UnitCollection]
    with ConcreteElem[UnitCollectionData, UnitCollection] {
    override lazy val parent: Option[Elem[_]] = Some(collectionElement(UnitElement))
    override lazy val entityDef = {
      val module = getModules("Collections")
      module.concreteSClasses.find(_.name == "UnitCollection").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }

    override def convertCollection(x: Rep[Collection[Unit]]) = UnitCollection(x.length)
    override def getDefaultRep = UnitCollection(0)
    override lazy val tag = {
      weakTypeTag[UnitCollection]
    }
  }

  // state representation type
  type UnitCollectionData = Int

  // 3) Iso for concrete class
  class UnitCollectionIso
    extends Iso[UnitCollectionData, UnitCollection] {
    override def from(p: Rep[UnitCollection]) =
      p.length
    override def to(p: Rep[Int]) = {
      val length = p
      UnitCollection(length)
    }
    lazy val eTo = new UnitCollectionElem(this)
  }
  // 4) constructor and deconstructor
  class UnitCollectionCompanionAbs extends CompanionDef[UnitCollectionCompanionAbs] with UnitCollectionCompanion {
    def selfType = UnitCollectionCompanionElem
    override def toString = "UnitCollection"

    def apply(length: Rep[Int]): Rep[UnitCollection] =
      mkUnitCollection(length)
  }
  object UnitCollectionMatcher {
    def unapply(p: Rep[Collection[Unit]]) = unmkUnitCollection(p)
  }
  lazy val UnitCollection: Rep[UnitCollectionCompanionAbs] = new UnitCollectionCompanionAbs
  implicit def proxyUnitCollectionCompanion(p: Rep[UnitCollectionCompanionAbs]): UnitCollectionCompanionAbs = {
    proxyOps[UnitCollectionCompanionAbs](p)
  }

  implicit case object UnitCollectionCompanionElem extends CompanionElem[UnitCollectionCompanionAbs] {
    lazy val tag = weakTypeTag[UnitCollectionCompanionAbs]
    protected def getDefaultRep = UnitCollection
  }

  implicit def proxyUnitCollection(p: Rep[UnitCollection]): UnitCollection =
    proxyOps[UnitCollection](p)

  implicit class ExtendedUnitCollection(p: Rep[UnitCollection]) {
    def toData: Rep[UnitCollectionData] = isoUnitCollection.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoUnitCollection: Iso[UnitCollectionData, UnitCollection] =
    cachedIso[UnitCollectionIso]()

  // 6) smart constructor and deconstructor
  def mkUnitCollection(length: Rep[Int]): Rep[UnitCollection]
  def unmkUnitCollection(p: Rep[Collection[Unit]]): Option[(Rep[Int])]

  abstract class AbsCollectionOverArray[Item]
      (arr: Rep[Array[Item]])(implicit eItem: Elem[Item])
    extends CollectionOverArray[Item](arr) with Def[CollectionOverArray[Item]] {
    lazy val selfType = element[CollectionOverArray[Item]]
  }
  // elem for concrete class
  class CollectionOverArrayElem[Item](val iso: Iso[CollectionOverArrayData[Item], CollectionOverArray[Item]])(implicit eItem: Elem[Item])
    extends CollectionElem[Item, CollectionOverArray[Item]]
    with ConcreteElem[CollectionOverArrayData[Item], CollectionOverArray[Item]] {
    override lazy val parent: Option[Elem[_]] = Some(collectionElement(element[Item]))
    override lazy val entityDef = {
      val module = getModules("Collections")
      module.concreteSClasses.find(_.name == "CollectionOverArray").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("Item" -> Left(eItem))
    }

    override def convertCollection(x: Rep[Collection[Item]]) = CollectionOverArray(x.arr)
    override def getDefaultRep = CollectionOverArray(element[Array[Item]].defaultRepValue)
    override lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[CollectionOverArray[Item]]
    }
  }

  // state representation type
  type CollectionOverArrayData[Item] = Array[Item]

  // 3) Iso for concrete class
  class CollectionOverArrayIso[Item](implicit eItem: Elem[Item])
    extends Iso[CollectionOverArrayData[Item], CollectionOverArray[Item]] {
    override def from(p: Rep[CollectionOverArray[Item]]) =
      p.arr
    override def to(p: Rep[Array[Item]]) = {
      val arr = p
      CollectionOverArray(arr)
    }
    lazy val eTo = new CollectionOverArrayElem[Item](this)
  }
  // 4) constructor and deconstructor
  class CollectionOverArrayCompanionAbs extends CompanionDef[CollectionOverArrayCompanionAbs] with CollectionOverArrayCompanion {
    def selfType = CollectionOverArrayCompanionElem
    override def toString = "CollectionOverArray"

    def apply[Item](arr: Rep[Array[Item]])(implicit eItem: Elem[Item]): Rep[CollectionOverArray[Item]] =
      mkCollectionOverArray(arr)
  }
  object CollectionOverArrayMatcher {
    def unapply[Item](p: Rep[Collection[Item]]) = unmkCollectionOverArray(p)
  }
  lazy val CollectionOverArray: Rep[CollectionOverArrayCompanionAbs] = new CollectionOverArrayCompanionAbs
  implicit def proxyCollectionOverArrayCompanion(p: Rep[CollectionOverArrayCompanionAbs]): CollectionOverArrayCompanionAbs = {
    proxyOps[CollectionOverArrayCompanionAbs](p)
  }

  implicit case object CollectionOverArrayCompanionElem extends CompanionElem[CollectionOverArrayCompanionAbs] {
    lazy val tag = weakTypeTag[CollectionOverArrayCompanionAbs]
    protected def getDefaultRep = CollectionOverArray
  }

  implicit def proxyCollectionOverArray[Item](p: Rep[CollectionOverArray[Item]]): CollectionOverArray[Item] =
    proxyOps[CollectionOverArray[Item]](p)

  implicit class ExtendedCollectionOverArray[Item](p: Rep[CollectionOverArray[Item]])(implicit eItem: Elem[Item]) {
    def toData: Rep[CollectionOverArrayData[Item]] = isoCollectionOverArray(eItem).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoCollectionOverArray[Item](implicit eItem: Elem[Item]): Iso[CollectionOverArrayData[Item], CollectionOverArray[Item]] =
    cachedIso[CollectionOverArrayIso[Item]](eItem)

  // 6) smart constructor and deconstructor
  def mkCollectionOverArray[Item](arr: Rep[Array[Item]])(implicit eItem: Elem[Item]): Rep[CollectionOverArray[Item]]
  def unmkCollectionOverArray[Item](p: Rep[Collection[Item]]): Option[(Rep[Array[Item]])]

  abstract class AbsCollectionOverList[Item]
      (lst: Rep[List[Item]])(implicit eItem: Elem[Item])
    extends CollectionOverList[Item](lst) with Def[CollectionOverList[Item]] {
    lazy val selfType = element[CollectionOverList[Item]]
  }
  // elem for concrete class
  class CollectionOverListElem[Item](val iso: Iso[CollectionOverListData[Item], CollectionOverList[Item]])(implicit eItem: Elem[Item])
    extends CollectionElem[Item, CollectionOverList[Item]]
    with ConcreteElem[CollectionOverListData[Item], CollectionOverList[Item]] {
    override lazy val parent: Option[Elem[_]] = Some(collectionElement(element[Item]))
    override lazy val entityDef = {
      val module = getModules("Collections")
      module.concreteSClasses.find(_.name == "CollectionOverList").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("Item" -> Left(eItem))
    }

    override def convertCollection(x: Rep[Collection[Item]]) = CollectionOverList(x.lst)
    override def getDefaultRep = CollectionOverList(element[List[Item]].defaultRepValue)
    override lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[CollectionOverList[Item]]
    }
  }

  // state representation type
  type CollectionOverListData[Item] = List[Item]

  // 3) Iso for concrete class
  class CollectionOverListIso[Item](implicit eItem: Elem[Item])
    extends Iso[CollectionOverListData[Item], CollectionOverList[Item]] {
    override def from(p: Rep[CollectionOverList[Item]]) =
      p.lst
    override def to(p: Rep[List[Item]]) = {
      val lst = p
      CollectionOverList(lst)
    }
    lazy val eTo = new CollectionOverListElem[Item](this)
  }
  // 4) constructor and deconstructor
  class CollectionOverListCompanionAbs extends CompanionDef[CollectionOverListCompanionAbs] with CollectionOverListCompanion {
    def selfType = CollectionOverListCompanionElem
    override def toString = "CollectionOverList"

    def apply[Item](lst: Rep[List[Item]])(implicit eItem: Elem[Item]): Rep[CollectionOverList[Item]] =
      mkCollectionOverList(lst)
  }
  object CollectionOverListMatcher {
    def unapply[Item](p: Rep[Collection[Item]]) = unmkCollectionOverList(p)
  }
  lazy val CollectionOverList: Rep[CollectionOverListCompanionAbs] = new CollectionOverListCompanionAbs
  implicit def proxyCollectionOverListCompanion(p: Rep[CollectionOverListCompanionAbs]): CollectionOverListCompanionAbs = {
    proxyOps[CollectionOverListCompanionAbs](p)
  }

  implicit case object CollectionOverListCompanionElem extends CompanionElem[CollectionOverListCompanionAbs] {
    lazy val tag = weakTypeTag[CollectionOverListCompanionAbs]
    protected def getDefaultRep = CollectionOverList
  }

  implicit def proxyCollectionOverList[Item](p: Rep[CollectionOverList[Item]]): CollectionOverList[Item] =
    proxyOps[CollectionOverList[Item]](p)

  implicit class ExtendedCollectionOverList[Item](p: Rep[CollectionOverList[Item]])(implicit eItem: Elem[Item]) {
    def toData: Rep[CollectionOverListData[Item]] = isoCollectionOverList(eItem).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoCollectionOverList[Item](implicit eItem: Elem[Item]): Iso[CollectionOverListData[Item], CollectionOverList[Item]] =
    cachedIso[CollectionOverListIso[Item]](eItem)

  // 6) smart constructor and deconstructor
  def mkCollectionOverList[Item](lst: Rep[List[Item]])(implicit eItem: Elem[Item]): Rep[CollectionOverList[Item]]
  def unmkCollectionOverList[Item](p: Rep[Collection[Item]]): Option[(Rep[List[Item]])]

  abstract class AbsCollectionOverSeq[Item]
      (seq: Rep[SSeq[Item]])(implicit eItem: Elem[Item])
    extends CollectionOverSeq[Item](seq) with Def[CollectionOverSeq[Item]] {
    lazy val selfType = element[CollectionOverSeq[Item]]
  }
  // elem for concrete class
  class CollectionOverSeqElem[Item](val iso: Iso[CollectionOverSeqData[Item], CollectionOverSeq[Item]])(implicit eItem: Elem[Item])
    extends CollectionElem[Item, CollectionOverSeq[Item]]
    with ConcreteElem[CollectionOverSeqData[Item], CollectionOverSeq[Item]] {
    override lazy val parent: Option[Elem[_]] = Some(collectionElement(element[Item]))
    override lazy val entityDef = {
      val module = getModules("Collections")
      module.concreteSClasses.find(_.name == "CollectionOverSeq").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("Item" -> Left(eItem))
    }

    override def convertCollection(x: Rep[Collection[Item]]) = CollectionOverSeq(x.seq)
    override def getDefaultRep = CollectionOverSeq(element[SSeq[Item]].defaultRepValue)
    override lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[CollectionOverSeq[Item]]
    }
  }

  // state representation type
  type CollectionOverSeqData[Item] = SSeq[Item]

  // 3) Iso for concrete class
  class CollectionOverSeqIso[Item](implicit eItem: Elem[Item])
    extends Iso[CollectionOverSeqData[Item], CollectionOverSeq[Item]] {
    override def from(p: Rep[CollectionOverSeq[Item]]) =
      p.seq
    override def to(p: Rep[SSeq[Item]]) = {
      val seq = p
      CollectionOverSeq(seq)
    }
    lazy val eTo = new CollectionOverSeqElem[Item](this)
  }
  // 4) constructor and deconstructor
  class CollectionOverSeqCompanionAbs extends CompanionDef[CollectionOverSeqCompanionAbs] with CollectionOverSeqCompanion {
    def selfType = CollectionOverSeqCompanionElem
    override def toString = "CollectionOverSeq"

    def apply[Item](seq: Rep[SSeq[Item]])(implicit eItem: Elem[Item]): Rep[CollectionOverSeq[Item]] =
      mkCollectionOverSeq(seq)
  }
  object CollectionOverSeqMatcher {
    def unapply[Item](p: Rep[Collection[Item]]) = unmkCollectionOverSeq(p)
  }
  lazy val CollectionOverSeq: Rep[CollectionOverSeqCompanionAbs] = new CollectionOverSeqCompanionAbs
  implicit def proxyCollectionOverSeqCompanion(p: Rep[CollectionOverSeqCompanionAbs]): CollectionOverSeqCompanionAbs = {
    proxyOps[CollectionOverSeqCompanionAbs](p)
  }

  implicit case object CollectionOverSeqCompanionElem extends CompanionElem[CollectionOverSeqCompanionAbs] {
    lazy val tag = weakTypeTag[CollectionOverSeqCompanionAbs]
    protected def getDefaultRep = CollectionOverSeq
  }

  implicit def proxyCollectionOverSeq[Item](p: Rep[CollectionOverSeq[Item]]): CollectionOverSeq[Item] =
    proxyOps[CollectionOverSeq[Item]](p)

  implicit class ExtendedCollectionOverSeq[Item](p: Rep[CollectionOverSeq[Item]])(implicit eItem: Elem[Item]) {
    def toData: Rep[CollectionOverSeqData[Item]] = isoCollectionOverSeq(eItem).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoCollectionOverSeq[Item](implicit eItem: Elem[Item]): Iso[CollectionOverSeqData[Item], CollectionOverSeq[Item]] =
    cachedIso[CollectionOverSeqIso[Item]](eItem)

  // 6) smart constructor and deconstructor
  def mkCollectionOverSeq[Item](seq: Rep[SSeq[Item]])(implicit eItem: Elem[Item]): Rep[CollectionOverSeq[Item]]
  def unmkCollectionOverSeq[Item](p: Rep[Collection[Item]]): Option[(Rep[SSeq[Item]])]

  abstract class AbsPairCollectionSOA[A, B]
      (as: Rep[Collection[A]], bs: Rep[Collection[B]])(implicit eA: Elem[A], eB: Elem[B])
    extends PairCollectionSOA[A, B](as, bs) with Def[PairCollectionSOA[A, B]] {
    lazy val selfType = element[PairCollectionSOA[A, B]]
  }
  // elem for concrete class
  class PairCollectionSOAElem[A, B](val iso: Iso[PairCollectionSOAData[A, B], PairCollectionSOA[A, B]])(implicit eA: Elem[A], eB: Elem[B])
    extends PairCollectionElem[A, B, PairCollectionSOA[A, B]]
    with ConcreteElem[PairCollectionSOAData[A, B], PairCollectionSOA[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(pairCollectionElement(element[A], element[B]))
    override lazy val entityDef = {
      val module = getModules("Collections")
      module.concreteSClasses.find(_.name == "PairCollectionSOA").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA), "B" -> Left(eB))
    }

    override def convertPairCollection(x: Rep[PairCollection[A, B]]) = PairCollectionSOA(x.as, x.bs)
    override def getDefaultRep = PairCollectionSOA(element[Collection[A]].defaultRepValue, element[Collection[B]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[PairCollectionSOA[A, B]]
    }
  }

  // state representation type
  type PairCollectionSOAData[A, B] = (Collection[A], Collection[B])

  // 3) Iso for concrete class
  class PairCollectionSOAIso[A, B](implicit eA: Elem[A], eB: Elem[B])
    extends Iso[PairCollectionSOAData[A, B], PairCollectionSOA[A, B]]()(pairElement(implicitly[Elem[Collection[A]]], implicitly[Elem[Collection[B]]])) {
    override def from(p: Rep[PairCollectionSOA[A, B]]) =
      (p.as, p.bs)
    override def to(p: Rep[(Collection[A], Collection[B])]) = {
      val Pair(as, bs) = p
      PairCollectionSOA(as, bs)
    }
    lazy val eTo = new PairCollectionSOAElem[A, B](this)
  }
  // 4) constructor and deconstructor
  class PairCollectionSOACompanionAbs extends CompanionDef[PairCollectionSOACompanionAbs] with PairCollectionSOACompanion {
    def selfType = PairCollectionSOACompanionElem
    override def toString = "PairCollectionSOA"
    def apply[A, B](p: Rep[PairCollectionSOAData[A, B]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairCollectionSOA[A, B]] =
      isoPairCollectionSOA(eA, eB).to(p)
    def apply[A, B](as: Rep[Collection[A]], bs: Rep[Collection[B]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairCollectionSOA[A, B]] =
      mkPairCollectionSOA(as, bs)
  }
  object PairCollectionSOAMatcher {
    def unapply[A, B](p: Rep[PairCollection[A, B]]) = unmkPairCollectionSOA(p)
  }
  lazy val PairCollectionSOA: Rep[PairCollectionSOACompanionAbs] = new PairCollectionSOACompanionAbs
  implicit def proxyPairCollectionSOACompanion(p: Rep[PairCollectionSOACompanionAbs]): PairCollectionSOACompanionAbs = {
    proxyOps[PairCollectionSOACompanionAbs](p)
  }

  implicit case object PairCollectionSOACompanionElem extends CompanionElem[PairCollectionSOACompanionAbs] {
    lazy val tag = weakTypeTag[PairCollectionSOACompanionAbs]
    protected def getDefaultRep = PairCollectionSOA
  }

  implicit def proxyPairCollectionSOA[A, B](p: Rep[PairCollectionSOA[A, B]]): PairCollectionSOA[A, B] =
    proxyOps[PairCollectionSOA[A, B]](p)

  implicit class ExtendedPairCollectionSOA[A, B](p: Rep[PairCollectionSOA[A, B]])(implicit eA: Elem[A], eB: Elem[B]) {
    def toData: Rep[PairCollectionSOAData[A, B]] = isoPairCollectionSOA(eA, eB).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoPairCollectionSOA[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[PairCollectionSOAData[A, B], PairCollectionSOA[A, B]] =
    cachedIso[PairCollectionSOAIso[A, B]](eA, eB)

  // 6) smart constructor and deconstructor
  def mkPairCollectionSOA[A, B](as: Rep[Collection[A]], bs: Rep[Collection[B]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairCollectionSOA[A, B]]
  def unmkPairCollectionSOA[A, B](p: Rep[PairCollection[A, B]]): Option[(Rep[Collection[A]], Rep[Collection[B]])]

  abstract class AbsPairCollectionAOS[A, B]
      (coll: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B])
    extends PairCollectionAOS[A, B](coll) with Def[PairCollectionAOS[A, B]] {
    lazy val selfType = element[PairCollectionAOS[A, B]]
  }
  // elem for concrete class
  class PairCollectionAOSElem[A, B](val iso: Iso[PairCollectionAOSData[A, B], PairCollectionAOS[A, B]])(implicit eA: Elem[A], eB: Elem[B])
    extends PairCollectionElem[A, B, PairCollectionAOS[A, B]]
    with ConcreteElem[PairCollectionAOSData[A, B], PairCollectionAOS[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(pairCollectionElement(element[A], element[B]))
    override lazy val entityDef = {
      val module = getModules("Collections")
      module.concreteSClasses.find(_.name == "PairCollectionAOS").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA), "B" -> Left(eB))
    }

    override def convertPairCollection(x: Rep[PairCollection[A, B]]) = PairCollectionAOS(x.coll)
    override def getDefaultRep = PairCollectionAOS(element[Collection[(A, B)]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[PairCollectionAOS[A, B]]
    }
  }

  // state representation type
  type PairCollectionAOSData[A, B] = Collection[(A, B)]

  // 3) Iso for concrete class
  class PairCollectionAOSIso[A, B](implicit eA: Elem[A], eB: Elem[B])
    extends Iso[PairCollectionAOSData[A, B], PairCollectionAOS[A, B]] {
    override def from(p: Rep[PairCollectionAOS[A, B]]) =
      p.coll
    override def to(p: Rep[Collection[(A, B)]]) = {
      val coll = p
      PairCollectionAOS(coll)
    }
    lazy val eTo = new PairCollectionAOSElem[A, B](this)
  }
  // 4) constructor and deconstructor
  class PairCollectionAOSCompanionAbs extends CompanionDef[PairCollectionAOSCompanionAbs] with PairCollectionAOSCompanion {
    def selfType = PairCollectionAOSCompanionElem
    override def toString = "PairCollectionAOS"

    def apply[A, B](coll: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairCollectionAOS[A, B]] =
      mkPairCollectionAOS(coll)
  }
  object PairCollectionAOSMatcher {
    def unapply[A, B](p: Rep[PairCollection[A, B]]) = unmkPairCollectionAOS(p)
  }
  lazy val PairCollectionAOS: Rep[PairCollectionAOSCompanionAbs] = new PairCollectionAOSCompanionAbs
  implicit def proxyPairCollectionAOSCompanion(p: Rep[PairCollectionAOSCompanionAbs]): PairCollectionAOSCompanionAbs = {
    proxyOps[PairCollectionAOSCompanionAbs](p)
  }

  implicit case object PairCollectionAOSCompanionElem extends CompanionElem[PairCollectionAOSCompanionAbs] {
    lazy val tag = weakTypeTag[PairCollectionAOSCompanionAbs]
    protected def getDefaultRep = PairCollectionAOS
  }

  implicit def proxyPairCollectionAOS[A, B](p: Rep[PairCollectionAOS[A, B]]): PairCollectionAOS[A, B] =
    proxyOps[PairCollectionAOS[A, B]](p)

  implicit class ExtendedPairCollectionAOS[A, B](p: Rep[PairCollectionAOS[A, B]])(implicit eA: Elem[A], eB: Elem[B]) {
    def toData: Rep[PairCollectionAOSData[A, B]] = isoPairCollectionAOS(eA, eB).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoPairCollectionAOS[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[PairCollectionAOSData[A, B], PairCollectionAOS[A, B]] =
    cachedIso[PairCollectionAOSIso[A, B]](eA, eB)

  // 6) smart constructor and deconstructor
  def mkPairCollectionAOS[A, B](coll: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairCollectionAOS[A, B]]
  def unmkPairCollectionAOS[A, B](p: Rep[PairCollection[A, B]]): Option[(Rep[Collection[(A, B)]])]

  abstract class AbsNestedCollectionFlat[A]
      (values: Coll[A], segments: PairColl[Int, Int])(implicit eA: Elem[A])
    extends NestedCollectionFlat[A](values, segments) with Def[NestedCollectionFlat[A]] {
    lazy val selfType = element[NestedCollectionFlat[A]]
  }
  // elem for concrete class
  class NestedCollectionFlatElem[A](val iso: Iso[NestedCollectionFlatData[A], NestedCollectionFlat[A]])(implicit eA: Elem[A])
    extends NestedCollectionElem[A, NestedCollectionFlat[A]]
    with ConcreteElem[NestedCollectionFlatData[A], NestedCollectionFlat[A]] {
    override lazy val parent: Option[Elem[_]] = Some(nestedCollectionElement(element[A]))
    override lazy val entityDef = {
      val module = getModules("Collections")
      module.concreteSClasses.find(_.name == "NestedCollectionFlat").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA))
    }

    override def convertNestedCollection(x: Rep[NestedCollection[A]]) = NestedCollectionFlat(x.values, x.segments)
    override def getDefaultRep = NestedCollectionFlat(element[Collection[A]].defaultRepValue, element[PairCollection[Int, Int]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[NestedCollectionFlat[A]]
    }
  }

  // state representation type
  type NestedCollectionFlatData[A] = (Collection[A], PairCollection[Int, Int])

  // 3) Iso for concrete class
  class NestedCollectionFlatIso[A](implicit eA: Elem[A])
    extends Iso[NestedCollectionFlatData[A], NestedCollectionFlat[A]]()(pairElement(implicitly[Elem[Collection[A]]], implicitly[Elem[PairCollection[Int, Int]]])) {
    override def from(p: Rep[NestedCollectionFlat[A]]) =
      (p.values, p.segments)
    override def to(p: Rep[(Collection[A], PairCollection[Int, Int])]) = {
      val Pair(values, segments) = p
      NestedCollectionFlat(values, segments)
    }
    lazy val eTo = new NestedCollectionFlatElem[A](this)
  }
  // 4) constructor and deconstructor
  class NestedCollectionFlatCompanionAbs extends CompanionDef[NestedCollectionFlatCompanionAbs] with NestedCollectionFlatCompanion {
    def selfType = NestedCollectionFlatCompanionElem
    override def toString = "NestedCollectionFlat"
    def apply[A](p: Rep[NestedCollectionFlatData[A]])(implicit eA: Elem[A]): Rep[NestedCollectionFlat[A]] =
      isoNestedCollectionFlat(eA).to(p)
    def apply[A](values: Coll[A], segments: PairColl[Int, Int])(implicit eA: Elem[A]): Rep[NestedCollectionFlat[A]] =
      mkNestedCollectionFlat(values, segments)
  }
  object NestedCollectionFlatMatcher {
    def unapply[A](p: Rep[NestedCollection[A]]) = unmkNestedCollectionFlat(p)
  }
  lazy val NestedCollectionFlat: Rep[NestedCollectionFlatCompanionAbs] = new NestedCollectionFlatCompanionAbs
  implicit def proxyNestedCollectionFlatCompanion(p: Rep[NestedCollectionFlatCompanionAbs]): NestedCollectionFlatCompanionAbs = {
    proxyOps[NestedCollectionFlatCompanionAbs](p)
  }

  implicit case object NestedCollectionFlatCompanionElem extends CompanionElem[NestedCollectionFlatCompanionAbs] {
    lazy val tag = weakTypeTag[NestedCollectionFlatCompanionAbs]
    protected def getDefaultRep = NestedCollectionFlat
  }

  implicit def proxyNestedCollectionFlat[A](p: Rep[NestedCollectionFlat[A]]): NestedCollectionFlat[A] =
    proxyOps[NestedCollectionFlat[A]](p)

  implicit class ExtendedNestedCollectionFlat[A](p: Rep[NestedCollectionFlat[A]])(implicit eA: Elem[A]) {
    def toData: Rep[NestedCollectionFlatData[A]] = isoNestedCollectionFlat(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoNestedCollectionFlat[A](implicit eA: Elem[A]): Iso[NestedCollectionFlatData[A], NestedCollectionFlat[A]] =
    cachedIso[NestedCollectionFlatIso[A]](eA)

  // 6) smart constructor and deconstructor
  def mkNestedCollectionFlat[A](values: Coll[A], segments: PairColl[Int, Int])(implicit eA: Elem[A]): Rep[NestedCollectionFlat[A]]
  def unmkNestedCollectionFlat[A](p: Rep[NestedCollection[A]]): Option[(Rep[Collection[A]], Rep[PairCollection[Int, Int]])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(Collections_Module.dump))
}

// Seq -----------------------------------
trait CollectionsSeq extends CollectionsDsl with scalan.ScalanSeq {
  self: ScalanCommunityDslSeq =>
  lazy val Collection: Rep[CollectionCompanionAbs] = new CollectionCompanionAbs {
  }

  case class SeqUnitCollection
      (override val length: Rep[Int])
    extends AbsUnitCollection(length) {
  }

  def mkUnitCollection
    (length: Rep[Int]): Rep[UnitCollection] =
    new SeqUnitCollection(length)
  def unmkUnitCollection(p: Rep[Collection[Unit]]) = p match {
    case p: UnitCollection @unchecked =>
      Some((p.length))
    case _ => None
  }

  case class SeqCollectionOverArray[Item]
      (override val arr: Rep[Array[Item]])(implicit eItem: Elem[Item])
    extends AbsCollectionOverArray[Item](arr) {
  }

  def mkCollectionOverArray[Item]
    (arr: Rep[Array[Item]])(implicit eItem: Elem[Item]): Rep[CollectionOverArray[Item]] =
    new SeqCollectionOverArray[Item](arr)
  def unmkCollectionOverArray[Item](p: Rep[Collection[Item]]) = p match {
    case p: CollectionOverArray[Item] @unchecked =>
      Some((p.arr))
    case _ => None
  }

  case class SeqCollectionOverList[Item]
      (override val lst: Rep[List[Item]])(implicit eItem: Elem[Item])
    extends AbsCollectionOverList[Item](lst) {
  }

  def mkCollectionOverList[Item]
    (lst: Rep[List[Item]])(implicit eItem: Elem[Item]): Rep[CollectionOverList[Item]] =
    new SeqCollectionOverList[Item](lst)
  def unmkCollectionOverList[Item](p: Rep[Collection[Item]]) = p match {
    case p: CollectionOverList[Item] @unchecked =>
      Some((p.lst))
    case _ => None
  }

  case class SeqCollectionOverSeq[Item]
      (override val seq: Rep[SSeq[Item]])(implicit eItem: Elem[Item])
    extends AbsCollectionOverSeq[Item](seq) {
  }

  def mkCollectionOverSeq[Item]
    (seq: Rep[SSeq[Item]])(implicit eItem: Elem[Item]): Rep[CollectionOverSeq[Item]] =
    new SeqCollectionOverSeq[Item](seq)
  def unmkCollectionOverSeq[Item](p: Rep[Collection[Item]]) = p match {
    case p: CollectionOverSeq[Item] @unchecked =>
      Some((p.seq))
    case _ => None
  }

  case class SeqPairCollectionSOA[A, B]
      (override val as: Rep[Collection[A]], override val bs: Rep[Collection[B]])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsPairCollectionSOA[A, B](as, bs) {
  }

  def mkPairCollectionSOA[A, B]
    (as: Rep[Collection[A]], bs: Rep[Collection[B]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairCollectionSOA[A, B]] =
    new SeqPairCollectionSOA[A, B](as, bs)
  def unmkPairCollectionSOA[A, B](p: Rep[PairCollection[A, B]]) = p match {
    case p: PairCollectionSOA[A, B] @unchecked =>
      Some((p.as, p.bs))
    case _ => None
  }

  case class SeqPairCollectionAOS[A, B]
      (override val coll: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsPairCollectionAOS[A, B](coll) {
  }

  def mkPairCollectionAOS[A, B]
    (coll: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairCollectionAOS[A, B]] =
    new SeqPairCollectionAOS[A, B](coll)
  def unmkPairCollectionAOS[A, B](p: Rep[PairCollection[A, B]]) = p match {
    case p: PairCollectionAOS[A, B] @unchecked =>
      Some((p.coll))
    case _ => None
  }

  case class SeqNestedCollectionFlat[A]
      (override val values: Coll[A], override val segments: PairColl[Int, Int])(implicit eA: Elem[A])
    extends AbsNestedCollectionFlat[A](values, segments) {
  }

  def mkNestedCollectionFlat[A]
    (values: Coll[A], segments: PairColl[Int, Int])(implicit eA: Elem[A]): Rep[NestedCollectionFlat[A]] =
    new SeqNestedCollectionFlat[A](values, segments)
  def unmkNestedCollectionFlat[A](p: Rep[NestedCollection[A]]) = p match {
    case p: NestedCollectionFlat[A] @unchecked =>
      Some((p.values, p.segments))
    case _ => None
  }
}

// Exp -----------------------------------
trait CollectionsExp extends CollectionsDsl with scalan.ScalanExp {
  self: ScalanCommunityDslExp =>
  lazy val Collection: Rep[CollectionCompanionAbs] = new CollectionCompanionAbs {
  }

  case class ExpUnitCollection
      (override val length: Rep[Int])
    extends AbsUnitCollection(length)

  object UnitCollectionMethods {
    object eItem {
      def unapply(d: Def[_]): Option[Rep[UnitCollection]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[UnitCollectionElem] && method.getName == "eItem" =>
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
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[UnitCollectionElem] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[UnitCollection], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[UnitCollection], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[UnitCollection], Rep[Unit => B @uncheckedVariance]) forSome {type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[UnitCollectionElem] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[UnitCollection], Rep[Unit => B @uncheckedVariance]) forSome {type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[UnitCollection], Rep[Unit => B @uncheckedVariance]) forSome {type B}] = exp match {
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
      def unapply(d: Def[_]): Option[(Rep[UnitCollection], RepMonoid[Unit @uncheckedVariance])] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[UnitCollectionElem] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[UnitCollection], RepMonoid[Unit @uncheckedVariance])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[UnitCollection], RepMonoid[Unit @uncheckedVariance])] = exp match {
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

    object sortBy {
      def unapply(d: Def[_]): Option[(Rep[UnitCollection], Rep[Unit => O], Ordering[O]) forSome {type O}] = d match {
        case MethodCall(receiver, method, Seq(by, o, _*), _) if receiver.elem.isInstanceOf[UnitCollectionElem] && method.getName == "sortBy" =>
          Some((receiver, by, o)).asInstanceOf[Option[(Rep[UnitCollection], Rep[Unit => O], Ordering[O]) forSome {type O}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[UnitCollection], Rep[Unit => O], Ordering[O]) forSome {type O}] = exp match {
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
  def unmkUnitCollection(p: Rep[Collection[Unit]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: UnitCollectionElem @unchecked =>
      Some((p.asRep[UnitCollection].length))
    case _ =>
      None
  }

  case class ExpCollectionOverArray[Item]
      (override val arr: Rep[Array[Item]])(implicit eItem: Elem[Item])
    extends AbsCollectionOverArray[Item](arr)

  object CollectionOverArrayMethods {
    object lst {
      def unapply(d: Def[_]): Option[Rep[CollectionOverArray[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionOverArrayElem[_]] && method.getName == "lst" =>
          Some(receiver).asInstanceOf[Option[Rep[CollectionOverArray[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CollectionOverArray[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[CollectionOverArray[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionOverArrayElem[_]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[CollectionOverArray[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CollectionOverArray[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverArray[Item]], Rep[Int]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[CollectionOverArrayElem[_]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[CollectionOverArray[Item]], Rep[Int]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverArray[Item]], Rep[Int]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverArray[Item]], Rep[Item => B @uncheckedVariance]) forSome {type Item; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CollectionOverArrayElem[_]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[CollectionOverArray[Item]], Rep[Item => B @uncheckedVariance]) forSome {type Item; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverArray[Item]], Rep[Item => B @uncheckedVariance]) forSome {type Item; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object slice {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverArray[Item]], Rep[Int], Rep[Int]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(offset, length, _*), _) if receiver.elem.isInstanceOf[CollectionOverArrayElem[_]] && method.getName == "slice" =>
          Some((receiver, offset, length)).asInstanceOf[Option[(Rep[CollectionOverArray[Item]], Rep[Int], Rep[Int]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverArray[Item]], Rep[Int], Rep[Int]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_many {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverArray[Item]], Coll[Int]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(indices, _*), _) if receiver.elem.isInstanceOf[CollectionOverArrayElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[CollectionOverArray[Item]], Coll[Int]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverArray[Item]], Coll[Int]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverArray[Item]], RepMonoid[Item @uncheckedVariance]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[CollectionOverArrayElem[_]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[CollectionOverArray[Item]], RepMonoid[Item @uncheckedVariance]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverArray[Item]], RepMonoid[Item @uncheckedVariance]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zip {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverArray[Item]], Coll[B]) forSome {type Item; type B}] = d match {
        case MethodCall(receiver, method, Seq(ys, _*), _) if receiver.elem.isInstanceOf[CollectionOverArrayElem[_]] && method.getName == "zip" =>
          Some((receiver, ys)).asInstanceOf[Option[(Rep[CollectionOverArray[Item]], Coll[B]) forSome {type Item; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverArray[Item]], Coll[B]) forSome {type Item; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object update {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverArray[Item]], Rep[Int], Rep[Item]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(idx, value, _*), _) if receiver.elem.isInstanceOf[CollectionOverArrayElem[_]] && method.getName == "update" =>
          Some((receiver, idx, value)).asInstanceOf[Option[(Rep[CollectionOverArray[Item]], Rep[Int], Rep[Item]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverArray[Item]], Rep[Int], Rep[Item]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object updateMany {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverArray[Item]], Coll[Int], Coll[Item]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(idxs, vals, _*), _) if receiver.elem.isInstanceOf[CollectionOverArrayElem[_]] && method.getName == "updateMany" =>
          Some((receiver, idxs, vals)).asInstanceOf[Option[(Rep[CollectionOverArray[Item]], Coll[Int], Coll[Item]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverArray[Item]], Coll[Int], Coll[Item]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverArray[Item]], Rep[Item @uncheckedVariance => Boolean]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CollectionOverArrayElem[_]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[CollectionOverArray[Item]], Rep[Item @uncheckedVariance => Boolean]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverArray[Item]], Rep[Item @uncheckedVariance => Boolean]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMapBy {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverArray[Item]], Rep[Item @uncheckedVariance => Collection[B]]) forSome {type Item; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CollectionOverArrayElem[_]] && method.getName == "flatMapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[CollectionOverArray[Item]], Rep[Item @uncheckedVariance => Collection[B]]) forSome {type Item; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverArray[Item]], Rep[Item @uncheckedVariance => Collection[B]]) forSome {type Item; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object append {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverArray[Item]], Rep[Item @uncheckedVariance]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(value, _*), _) if receiver.elem.isInstanceOf[CollectionOverArrayElem[_]] && method.getName == "append" =>
          Some((receiver, value)).asInstanceOf[Option[(Rep[CollectionOverArray[Item]], Rep[Item @uncheckedVariance]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverArray[Item]], Rep[Item @uncheckedVariance]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sortBy {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverArray[Item]], Rep[Item => O], Ordering[O]) forSome {type Item; type O}] = d match {
        case MethodCall(receiver, method, Seq(means, o, _*), _) if receiver.elem.isInstanceOf[CollectionOverArrayElem[_]] && method.getName == "sortBy" =>
          Some((receiver, means, o)).asInstanceOf[Option[(Rep[CollectionOverArray[Item]], Rep[Item => O], Ordering[O]) forSome {type Item; type O}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverArray[Item]], Rep[Item => O], Ordering[O]) forSome {type Item; type O}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CollectionOverArrayCompanionMethods {
  }

  def mkCollectionOverArray[Item]
    (arr: Rep[Array[Item]])(implicit eItem: Elem[Item]): Rep[CollectionOverArray[Item]] =
    new ExpCollectionOverArray[Item](arr)
  def unmkCollectionOverArray[Item](p: Rep[Collection[Item]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CollectionOverArrayElem[Item] @unchecked =>
      Some((p.asRep[CollectionOverArray[Item]].arr))
    case _ =>
      None
  }

  case class ExpCollectionOverList[Item]
      (override val lst: Rep[List[Item]])(implicit eItem: Elem[Item])
    extends AbsCollectionOverList[Item](lst)

  object CollectionOverListMethods {
    object length {
      def unapply(d: Def[_]): Option[Rep[CollectionOverList[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionOverListElem[_]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[CollectionOverList[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CollectionOverList[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverList[Item]], Rep[Int]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[CollectionOverListElem[_]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[CollectionOverList[Item]], Rep[Int]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverList[Item]], Rep[Int]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object arr {
      def unapply(d: Def[_]): Option[Rep[CollectionOverList[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionOverListElem[_]] && method.getName == "arr" =>
          Some(receiver).asInstanceOf[Option[Rep[CollectionOverList[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CollectionOverList[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverList[Item]], Rep[Item => B @uncheckedVariance]) forSome {type Item; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CollectionOverListElem[_]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[CollectionOverList[Item]], Rep[Item => B @uncheckedVariance]) forSome {type Item; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverList[Item]], Rep[Item => B @uncheckedVariance]) forSome {type Item; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object slice {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverList[Item]], Rep[Int], Rep[Int]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(offset, length, _*), _) if receiver.elem.isInstanceOf[CollectionOverListElem[_]] && method.getName == "slice" =>
          Some((receiver, offset, length)).asInstanceOf[Option[(Rep[CollectionOverList[Item]], Rep[Int], Rep[Int]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverList[Item]], Rep[Int], Rep[Int]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_many {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverList[Item]], Coll[Int]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(indices, _*), _) if receiver.elem.isInstanceOf[CollectionOverListElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[CollectionOverList[Item]], Coll[Int]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverList[Item]], Coll[Int]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverList[Item]], RepMonoid[Item @uncheckedVariance]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[CollectionOverListElem[_]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[CollectionOverList[Item]], RepMonoid[Item @uncheckedVariance]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverList[Item]], RepMonoid[Item @uncheckedVariance]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zip {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverList[Item]], Coll[B]) forSome {type Item; type B}] = d match {
        case MethodCall(receiver, method, Seq(ys, _*), _) if receiver.elem.isInstanceOf[CollectionOverListElem[_]] && method.getName == "zip" =>
          Some((receiver, ys)).asInstanceOf[Option[(Rep[CollectionOverList[Item]], Coll[B]) forSome {type Item; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverList[Item]], Coll[B]) forSome {type Item; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object update {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverList[Item]], Rep[Int], Rep[Item]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(idx, value, _*), _) if receiver.elem.isInstanceOf[CollectionOverListElem[_]] && method.getName == "update" =>
          Some((receiver, idx, value)).asInstanceOf[Option[(Rep[CollectionOverList[Item]], Rep[Int], Rep[Item]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverList[Item]], Rep[Int], Rep[Item]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object updateMany {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverList[Item]], Coll[Int], Coll[Item]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(idxs, vals, _*), _) if receiver.elem.isInstanceOf[CollectionOverListElem[_]] && method.getName == "updateMany" =>
          Some((receiver, idxs, vals)).asInstanceOf[Option[(Rep[CollectionOverList[Item]], Coll[Int], Coll[Item]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverList[Item]], Coll[Int], Coll[Item]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverList[Item]], Rep[Item @uncheckedVariance => Boolean]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CollectionOverListElem[_]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[CollectionOverList[Item]], Rep[Item @uncheckedVariance => Boolean]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverList[Item]], Rep[Item @uncheckedVariance => Boolean]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMapBy {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverList[Item]], Rep[Item @uncheckedVariance => Collection[B]]) forSome {type Item; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CollectionOverListElem[_]] && method.getName == "flatMapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[CollectionOverList[Item]], Rep[Item @uncheckedVariance => Collection[B]]) forSome {type Item; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverList[Item]], Rep[Item @uncheckedVariance => Collection[B]]) forSome {type Item; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object append {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverList[Item]], Rep[Item @uncheckedVariance]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(value, _*), _) if receiver.elem.isInstanceOf[CollectionOverListElem[_]] && method.getName == "append" =>
          Some((receiver, value)).asInstanceOf[Option[(Rep[CollectionOverList[Item]], Rep[Item @uncheckedVariance]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverList[Item]], Rep[Item @uncheckedVariance]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sortBy {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverList[Item]], Rep[Item => O], Ordering[O]) forSome {type Item; type O}] = d match {
        case MethodCall(receiver, method, Seq(by, o, _*), _) if receiver.elem.isInstanceOf[CollectionOverListElem[_]] && method.getName == "sortBy" =>
          Some((receiver, by, o)).asInstanceOf[Option[(Rep[CollectionOverList[Item]], Rep[Item => O], Ordering[O]) forSome {type Item; type O}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverList[Item]], Rep[Item => O], Ordering[O]) forSome {type Item; type O}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CollectionOverListCompanionMethods {
  }

  def mkCollectionOverList[Item]
    (lst: Rep[List[Item]])(implicit eItem: Elem[Item]): Rep[CollectionOverList[Item]] =
    new ExpCollectionOverList[Item](lst)
  def unmkCollectionOverList[Item](p: Rep[Collection[Item]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CollectionOverListElem[Item] @unchecked =>
      Some((p.asRep[CollectionOverList[Item]].lst))
    case _ =>
      None
  }

  case class ExpCollectionOverSeq[Item]
      (override val seq: Rep[SSeq[Item]])(implicit eItem: Elem[Item])
    extends AbsCollectionOverSeq[Item](seq)

  object CollectionOverSeqMethods {
    object arr {
      def unapply(d: Def[_]): Option[Rep[CollectionOverSeq[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionOverSeqElem[_]] && method.getName == "arr" =>
          Some(receiver).asInstanceOf[Option[Rep[CollectionOverSeq[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CollectionOverSeq[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object lst {
      def unapply(d: Def[_]): Option[Rep[CollectionOverSeq[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionOverSeqElem[_]] && method.getName == "lst" =>
          Some(receiver).asInstanceOf[Option[Rep[CollectionOverSeq[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CollectionOverSeq[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[CollectionOverSeq[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionOverSeqElem[_]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[CollectionOverSeq[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CollectionOverSeq[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverSeq[Item]], Rep[Int]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[CollectionOverSeqElem[_]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[CollectionOverSeq[Item]], Rep[Int]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverSeq[Item]], Rep[Int]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object slice {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverSeq[Item]], Rep[Int], Rep[Int]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(offset, length, _*), _) if receiver.elem.isInstanceOf[CollectionOverSeqElem[_]] && method.getName == "slice" =>
          Some((receiver, offset, length)).asInstanceOf[Option[(Rep[CollectionOverSeq[Item]], Rep[Int], Rep[Int]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverSeq[Item]], Rep[Int], Rep[Int]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_many {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverSeq[Item]], Coll[Int]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(indices, _*), _) if receiver.elem.isInstanceOf[CollectionOverSeqElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[CollectionOverSeq[Item]], Coll[Int]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverSeq[Item]], Coll[Int]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverSeq[Item]], Rep[Item => B @uncheckedVariance]) forSome {type Item; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CollectionOverSeqElem[_]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[CollectionOverSeq[Item]], Rep[Item => B @uncheckedVariance]) forSome {type Item; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverSeq[Item]], Rep[Item => B @uncheckedVariance]) forSome {type Item; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverSeq[Item]], RepMonoid[Item @uncheckedVariance]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[CollectionOverSeqElem[_]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[CollectionOverSeq[Item]], RepMonoid[Item @uncheckedVariance]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverSeq[Item]], RepMonoid[Item @uncheckedVariance]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zip {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverSeq[Item]], Coll[B]) forSome {type Item; type B}] = d match {
        case MethodCall(receiver, method, Seq(ys, _*), _) if receiver.elem.isInstanceOf[CollectionOverSeqElem[_]] && method.getName == "zip" =>
          Some((receiver, ys)).asInstanceOf[Option[(Rep[CollectionOverSeq[Item]], Coll[B]) forSome {type Item; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverSeq[Item]], Coll[B]) forSome {type Item; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object update {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverSeq[Item]], Rep[Int], Rep[Item]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(idx, value, _*), _) if receiver.elem.isInstanceOf[CollectionOverSeqElem[_]] && method.getName == "update" =>
          Some((receiver, idx, value)).asInstanceOf[Option[(Rep[CollectionOverSeq[Item]], Rep[Int], Rep[Item]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverSeq[Item]], Rep[Int], Rep[Item]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object updateMany {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverSeq[Item]], Coll[Int], Coll[Item]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(idxs, vals, _*), _) if receiver.elem.isInstanceOf[CollectionOverSeqElem[_]] && method.getName == "updateMany" =>
          Some((receiver, idxs, vals)).asInstanceOf[Option[(Rep[CollectionOverSeq[Item]], Coll[Int], Coll[Item]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverSeq[Item]], Coll[Int], Coll[Item]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverSeq[Item]], Rep[Item @uncheckedVariance => Boolean]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CollectionOverSeqElem[_]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[CollectionOverSeq[Item]], Rep[Item @uncheckedVariance => Boolean]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverSeq[Item]], Rep[Item @uncheckedVariance => Boolean]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMapBy {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverSeq[Item]], Rep[Item @uncheckedVariance => Collection[B]]) forSome {type Item; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CollectionOverSeqElem[_]] && method.getName == "flatMapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[CollectionOverSeq[Item]], Rep[Item @uncheckedVariance => Collection[B]]) forSome {type Item; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverSeq[Item]], Rep[Item @uncheckedVariance => Collection[B]]) forSome {type Item; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object append {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverSeq[Item]], Rep[Item @uncheckedVariance]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(value, _*), _) if receiver.elem.isInstanceOf[CollectionOverSeqElem[_]] && method.getName == "append" =>
          Some((receiver, value)).asInstanceOf[Option[(Rep[CollectionOverSeq[Item]], Rep[Item @uncheckedVariance]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverSeq[Item]], Rep[Item @uncheckedVariance]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sortBy {
      def unapply(d: Def[_]): Option[(Rep[CollectionOverSeq[Item]], Rep[Item => O], Ordering[O]) forSome {type Item; type O}] = d match {
        case MethodCall(receiver, method, Seq(by, o, _*), _) if receiver.elem.isInstanceOf[CollectionOverSeqElem[_]] && method.getName == "sortBy" =>
          Some((receiver, by, o)).asInstanceOf[Option[(Rep[CollectionOverSeq[Item]], Rep[Item => O], Ordering[O]) forSome {type Item; type O}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CollectionOverSeq[Item]], Rep[Item => O], Ordering[O]) forSome {type Item; type O}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CollectionOverSeqCompanionMethods {
  }

  def mkCollectionOverSeq[Item]
    (seq: Rep[SSeq[Item]])(implicit eItem: Elem[Item]): Rep[CollectionOverSeq[Item]] =
    new ExpCollectionOverSeq[Item](seq)
  def unmkCollectionOverSeq[Item](p: Rep[Collection[Item]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CollectionOverSeqElem[Item] @unchecked =>
      Some((p.asRep[CollectionOverSeq[Item]].seq))
    case _ =>
      None
  }

  case class ExpPairCollectionSOA[A, B]
      (override val as: Rep[Collection[A]], override val bs: Rep[Collection[B]])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsPairCollectionSOA[A, B](as, bs)

  object PairCollectionSOAMethods {
    object arr {
      def unapply(d: Def[_]): Option[Rep[PairCollectionSOA[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairCollectionSOAElem[_, _]] && method.getName == "arr" =>
          Some(receiver).asInstanceOf[Option[Rep[PairCollectionSOA[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairCollectionSOA[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object lst {
      def unapply(d: Def[_]): Option[Rep[PairCollectionSOA[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairCollectionSOAElem[_, _]] && method.getName == "lst" =>
          Some(receiver).asInstanceOf[Option[Rep[PairCollectionSOA[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairCollectionSOA[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object coll {
      def unapply(d: Def[_]): Option[Rep[PairCollectionSOA[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairCollectionSOAElem[_, _]] && method.getName == "coll" =>
          Some(receiver).asInstanceOf[Option[Rep[PairCollectionSOA[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairCollectionSOA[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionSOA[A, B]], Rep[Int]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[PairCollectionSOAElem[_, _]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[PairCollectionSOA[A, B]], Rep[Int]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionSOA[A, B]], Rep[Int]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[PairCollectionSOA[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairCollectionSOAElem[_, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[PairCollectionSOA[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairCollectionSOA[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object slice {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionSOA[A, B]], Rep[Int], Rep[Int]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(offset, length, _*), _) if receiver.elem.isInstanceOf[PairCollectionSOAElem[_, _]] && method.getName == "slice" =>
          Some((receiver, offset, length)).asInstanceOf[Option[(Rep[PairCollectionSOA[A, B]], Rep[Int], Rep[Int]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionSOA[A, B]], Rep[Int], Rep[Int]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_many {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionSOA[A, B]], Coll[Int]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(indices, _*), _) if receiver.elem.isInstanceOf[PairCollectionSOAElem[_, _]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[PairCollectionSOA[A, B]], Coll[Int]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionSOA[A, B]], Coll[Int]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionSOA[A, B]], Rep[(A, B) @uncheckedVariance => C]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[PairCollectionSOAElem[_, _]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[PairCollectionSOA[A, B]], Rep[(A, B) @uncheckedVariance => C]) forSome {type A; type B; type C}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionSOA[A, B]], Rep[(A, B) @uncheckedVariance => C]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionSOA[A, B]], RepMonoid[(A, B) @uncheckedVariance]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[PairCollectionSOAElem[_, _]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[PairCollectionSOA[A, B]], RepMonoid[(A, B) @uncheckedVariance]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionSOA[A, B]], RepMonoid[(A, B) @uncheckedVariance]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zip {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionSOA[A, B]], Coll[C]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, Seq(ys, _*), _) if receiver.elem.isInstanceOf[PairCollectionSOAElem[_, _]] && method.getName == "zip" =>
          Some((receiver, ys)).asInstanceOf[Option[(Rep[PairCollectionSOA[A, B]], Coll[C]) forSome {type A; type B; type C}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionSOA[A, B]], Coll[C]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object update {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionSOA[A, B]], Rep[Int], Rep[(A, B)]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(idx, value, _*), _) if receiver.elem.isInstanceOf[PairCollectionSOAElem[_, _]] && method.getName == "update" =>
          Some((receiver, idx, value)).asInstanceOf[Option[(Rep[PairCollectionSOA[A, B]], Rep[Int], Rep[(A, B)]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionSOA[A, B]], Rep[Int], Rep[(A, B)]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object updateMany {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionSOA[A, B]], Coll[Int], Coll[(A, B)]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(idxs, vals, _*), _) if receiver.elem.isInstanceOf[PairCollectionSOAElem[_, _]] && method.getName == "updateMany" =>
          Some((receiver, idxs, vals)).asInstanceOf[Option[(Rep[PairCollectionSOA[A, B]], Coll[Int], Coll[(A, B)]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionSOA[A, B]], Coll[Int], Coll[(A, B)]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionSOA[A, B]], Rep[(A, B) @uncheckedVariance => Boolean]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[PairCollectionSOAElem[_, _]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[PairCollectionSOA[A, B]], Rep[(A, B) @uncheckedVariance => Boolean]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionSOA[A, B]], Rep[(A, B) @uncheckedVariance => Boolean]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMapBy {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionSOA[A, B]], Rep[(A, B) @uncheckedVariance => Collection[C]]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[PairCollectionSOAElem[_, _]] && method.getName == "flatMapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[PairCollectionSOA[A, B]], Rep[(A, B) @uncheckedVariance => Collection[C]]) forSome {type A; type B; type C}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionSOA[A, B]], Rep[(A, B) @uncheckedVariance => Collection[C]]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object append {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionSOA[A, B]], Rep[(A, B) @uncheckedVariance]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(value, _*), _) if receiver.elem.isInstanceOf[PairCollectionSOAElem[_, _]] && method.getName == "append" =>
          Some((receiver, value)).asInstanceOf[Option[(Rep[PairCollectionSOA[A, B]], Rep[(A, B) @uncheckedVariance]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionSOA[A, B]], Rep[(A, B) @uncheckedVariance]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object innerJoin {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionSOA[A, B]], PairColl[A, C], Rep[((B, C)) => R], Ordering[A], Elem[R], Elem[B], Elem[C]) forSome {type A; type B; type C; type R}] = d match {
        case MethodCall(receiver, method, Seq(other, f, ordK, eR, eB, eC, _*), _) if receiver.elem.isInstanceOf[PairCollectionSOAElem[_, _]] && method.getName == "innerJoin" =>
          Some((receiver, other, f, ordK, eR, eB, eC)).asInstanceOf[Option[(Rep[PairCollectionSOA[A, B]], PairColl[A, C], Rep[((B, C)) => R], Ordering[A], Elem[R], Elem[B], Elem[C]) forSome {type A; type B; type C; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionSOA[A, B]], PairColl[A, C], Rep[((B, C)) => R], Ordering[A], Elem[R], Elem[B], Elem[C]) forSome {type A; type B; type C; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outerJoin {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionSOA[A, B]], PairColl[A, C], Rep[((B, C)) => R], Rep[B => R], Rep[C => R], Ordering[A], Elem[R], Elem[B], Elem[C]) forSome {type A; type B; type C; type R}] = d match {
        case MethodCall(receiver, method, Seq(other, f, f1, f2, ordK, eR, eB, eC, _*), _) if receiver.elem.isInstanceOf[PairCollectionSOAElem[_, _]] && method.getName == "outerJoin" =>
          Some((receiver, other, f, f1, f2, ordK, eR, eB, eC)).asInstanceOf[Option[(Rep[PairCollectionSOA[A, B]], PairColl[A, C], Rep[((B, C)) => R], Rep[B => R], Rep[C => R], Ordering[A], Elem[R], Elem[B], Elem[C]) forSome {type A; type B; type C; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionSOA[A, B]], PairColl[A, C], Rep[((B, C)) => R], Rep[B => R], Rep[C => R], Ordering[A], Elem[R], Elem[B], Elem[C]) forSome {type A; type B; type C; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sortBy {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionSOA[A, B]], Rep[((A, B)) => O], Ordering[O]) forSome {type A; type B; type O}] = d match {
        case MethodCall(receiver, method, Seq(means, o, _*), _) if receiver.elem.isInstanceOf[PairCollectionSOAElem[_, _]] && method.getName == "sortBy" =>
          Some((receiver, means, o)).asInstanceOf[Option[(Rep[PairCollectionSOA[A, B]], Rep[((A, B)) => O], Ordering[O]) forSome {type A; type B; type O}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionSOA[A, B]], Rep[((A, B)) => O], Ordering[O]) forSome {type A; type B; type O}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object PairCollectionSOACompanionMethods {
  }

  def mkPairCollectionSOA[A, B]
    (as: Rep[Collection[A]], bs: Rep[Collection[B]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairCollectionSOA[A, B]] =
    new ExpPairCollectionSOA[A, B](as, bs)
  def unmkPairCollectionSOA[A, B](p: Rep[PairCollection[A, B]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: PairCollectionSOAElem[A, B] @unchecked =>
      Some((p.asRep[PairCollectionSOA[A, B]].as, p.asRep[PairCollectionSOA[A, B]].bs))
    case _ =>
      None
  }

  case class ExpPairCollectionAOS[A, B]
      (override val coll: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsPairCollectionAOS[A, B](coll)

  object PairCollectionAOSMethods {
    object arr {
      def unapply(d: Def[_]): Option[Rep[PairCollectionAOS[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairCollectionAOSElem[_, _]] && method.getName == "arr" =>
          Some(receiver).asInstanceOf[Option[Rep[PairCollectionAOS[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairCollectionAOS[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object lst {
      def unapply(d: Def[_]): Option[Rep[PairCollectionAOS[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairCollectionAOSElem[_, _]] && method.getName == "lst" =>
          Some(receiver).asInstanceOf[Option[Rep[PairCollectionAOS[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairCollectionAOS[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object seq {
      def unapply(d: Def[_]): Option[Rep[PairCollectionAOS[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairCollectionAOSElem[_, _]] && method.getName == "seq" =>
          Some(receiver).asInstanceOf[Option[Rep[PairCollectionAOS[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairCollectionAOS[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object as {
      def unapply(d: Def[_]): Option[Rep[PairCollectionAOS[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairCollectionAOSElem[_, _]] && method.getName == "as" =>
          Some(receiver).asInstanceOf[Option[Rep[PairCollectionAOS[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairCollectionAOS[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object bs {
      def unapply(d: Def[_]): Option[Rep[PairCollectionAOS[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairCollectionAOSElem[_, _]] && method.getName == "bs" =>
          Some(receiver).asInstanceOf[Option[Rep[PairCollectionAOS[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairCollectionAOS[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionAOS[A, B]], Rep[Int]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[PairCollectionAOSElem[_, _]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[PairCollectionAOS[A, B]], Rep[Int]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionAOS[A, B]], Rep[Int]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[PairCollectionAOS[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairCollectionAOSElem[_, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[PairCollectionAOS[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairCollectionAOS[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object slice {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionAOS[A, B]], Rep[Int], Rep[Int]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(offset, length, _*), _) if receiver.elem.isInstanceOf[PairCollectionAOSElem[_, _]] && method.getName == "slice" =>
          Some((receiver, offset, length)).asInstanceOf[Option[(Rep[PairCollectionAOS[A, B]], Rep[Int], Rep[Int]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionAOS[A, B]], Rep[Int], Rep[Int]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_many {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionAOS[A, B]], Coll[Int]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(indices, _*), _) if receiver.elem.isInstanceOf[PairCollectionAOSElem[_, _]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[PairCollectionAOS[A, B]], Coll[Int]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionAOS[A, B]], Coll[Int]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionAOS[A, B]], Rep[(A, B) @uncheckedVariance => C]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[PairCollectionAOSElem[_, _]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[PairCollectionAOS[A, B]], Rep[(A, B) @uncheckedVariance => C]) forSome {type A; type B; type C}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionAOS[A, B]], Rep[(A, B) @uncheckedVariance => C]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionAOS[A, B]], RepMonoid[(A, B) @uncheckedVariance]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[PairCollectionAOSElem[_, _]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[PairCollectionAOS[A, B]], RepMonoid[(A, B) @uncheckedVariance]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionAOS[A, B]], RepMonoid[(A, B) @uncheckedVariance]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zip {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionAOS[A, B]], Coll[C]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, Seq(ys, _*), _) if receiver.elem.isInstanceOf[PairCollectionAOSElem[_, _]] && method.getName == "zip" =>
          Some((receiver, ys)).asInstanceOf[Option[(Rep[PairCollectionAOS[A, B]], Coll[C]) forSome {type A; type B; type C}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionAOS[A, B]], Coll[C]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object update {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionAOS[A, B]], Rep[Int], Rep[(A, B)]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(idx, value, _*), _) if receiver.elem.isInstanceOf[PairCollectionAOSElem[_, _]] && method.getName == "update" =>
          Some((receiver, idx, value)).asInstanceOf[Option[(Rep[PairCollectionAOS[A, B]], Rep[Int], Rep[(A, B)]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionAOS[A, B]], Rep[Int], Rep[(A, B)]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object updateMany {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionAOS[A, B]], Coll[Int], Coll[(A, B)]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(idxs, vals, _*), _) if receiver.elem.isInstanceOf[PairCollectionAOSElem[_, _]] && method.getName == "updateMany" =>
          Some((receiver, idxs, vals)).asInstanceOf[Option[(Rep[PairCollectionAOS[A, B]], Coll[Int], Coll[(A, B)]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionAOS[A, B]], Coll[Int], Coll[(A, B)]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionAOS[A, B]], Rep[(A, B) @uncheckedVariance => Boolean]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[PairCollectionAOSElem[_, _]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[PairCollectionAOS[A, B]], Rep[(A, B) @uncheckedVariance => Boolean]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionAOS[A, B]], Rep[(A, B) @uncheckedVariance => Boolean]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMapBy {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionAOS[A, B]], Rep[(A, B) @uncheckedVariance => Collection[C]]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[PairCollectionAOSElem[_, _]] && method.getName == "flatMapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[PairCollectionAOS[A, B]], Rep[(A, B) @uncheckedVariance => Collection[C]]) forSome {type A; type B; type C}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionAOS[A, B]], Rep[(A, B) @uncheckedVariance => Collection[C]]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object append {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionAOS[A, B]], Rep[(A, B) @uncheckedVariance]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(value, _*), _) if receiver.elem.isInstanceOf[PairCollectionAOSElem[_, _]] && method.getName == "append" =>
          Some((receiver, value)).asInstanceOf[Option[(Rep[PairCollectionAOS[A, B]], Rep[(A, B) @uncheckedVariance]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionAOS[A, B]], Rep[(A, B) @uncheckedVariance]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object innerJoin {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionAOS[A, B]], PairColl[A, C], Rep[((B, C)) => R], Ordering[A], Elem[R], Elem[B], Elem[C]) forSome {type A; type B; type C; type R}] = d match {
        case MethodCall(receiver, method, Seq(other, f, ordK, eR, eB, eC, _*), _) if receiver.elem.isInstanceOf[PairCollectionAOSElem[_, _]] && method.getName == "innerJoin" =>
          Some((receiver, other, f, ordK, eR, eB, eC)).asInstanceOf[Option[(Rep[PairCollectionAOS[A, B]], PairColl[A, C], Rep[((B, C)) => R], Ordering[A], Elem[R], Elem[B], Elem[C]) forSome {type A; type B; type C; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionAOS[A, B]], PairColl[A, C], Rep[((B, C)) => R], Ordering[A], Elem[R], Elem[B], Elem[C]) forSome {type A; type B; type C; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outerJoin {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionAOS[A, B]], PairColl[A, C], Rep[((B, C)) => R], Rep[B => R], Rep[C => R], Ordering[A], Elem[R], Elem[B], Elem[C]) forSome {type A; type B; type C; type R}] = d match {
        case MethodCall(receiver, method, Seq(other, f, f1, f2, ordK, eR, eB, eC, _*), _) if receiver.elem.isInstanceOf[PairCollectionAOSElem[_, _]] && method.getName == "outerJoin" =>
          Some((receiver, other, f, f1, f2, ordK, eR, eB, eC)).asInstanceOf[Option[(Rep[PairCollectionAOS[A, B]], PairColl[A, C], Rep[((B, C)) => R], Rep[B => R], Rep[C => R], Ordering[A], Elem[R], Elem[B], Elem[C]) forSome {type A; type B; type C; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionAOS[A, B]], PairColl[A, C], Rep[((B, C)) => R], Rep[B => R], Rep[C => R], Ordering[A], Elem[R], Elem[B], Elem[C]) forSome {type A; type B; type C; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sortBy {
      def unapply(d: Def[_]): Option[(Rep[PairCollectionAOS[A, B]], Rep[((A, B)) => O], Ordering[O]) forSome {type A; type B; type O}] = d match {
        case MethodCall(receiver, method, Seq(means, o, _*), _) if receiver.elem.isInstanceOf[PairCollectionAOSElem[_, _]] && method.getName == "sortBy" =>
          Some((receiver, means, o)).asInstanceOf[Option[(Rep[PairCollectionAOS[A, B]], Rep[((A, B)) => O], Ordering[O]) forSome {type A; type B; type O}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairCollectionAOS[A, B]], Rep[((A, B)) => O], Ordering[O]) forSome {type A; type B; type O}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object PairCollectionAOSCompanionMethods {
    object fromArray {
      def unapply(d: Def[_]): Option[Arr[(A, B)] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(arr, _*), _) if receiver.elem == PairCollectionAOSCompanionElem && method.getName == "fromArray" =>
          Some(arr).asInstanceOf[Option[Arr[(A, B)] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Arr[(A, B)] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkPairCollectionAOS[A, B]
    (coll: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairCollectionAOS[A, B]] =
    new ExpPairCollectionAOS[A, B](coll)
  def unmkPairCollectionAOS[A, B](p: Rep[PairCollection[A, B]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: PairCollectionAOSElem[A, B] @unchecked =>
      Some((p.asRep[PairCollectionAOS[A, B]].coll))
    case _ =>
      None
  }

  case class ExpNestedCollectionFlat[A]
      (override val values: Coll[A], override val segments: PairColl[Int, Int])(implicit eA: Elem[A])
    extends AbsNestedCollectionFlat[A](values, segments)

  object NestedCollectionFlatMethods {
    object length {
      def unapply(d: Def[_]): Option[Rep[NestedCollectionFlat[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[NestedCollectionFlatElem[_]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[NestedCollectionFlat[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[NestedCollectionFlat[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[NestedCollectionFlat[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[NestedCollectionFlatElem[_]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[NestedCollectionFlat[A]], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollectionFlat[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object arr {
      def unapply(d: Def[_]): Option[Rep[NestedCollectionFlat[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[NestedCollectionFlatElem[_]] && method.getName == "arr" =>
          Some(receiver).asInstanceOf[Option[Rep[NestedCollectionFlat[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[NestedCollectionFlat[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object lst {
      def unapply(d: Def[_]): Option[Rep[NestedCollectionFlat[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[NestedCollectionFlatElem[_]] && method.getName == "lst" =>
          Some(receiver).asInstanceOf[Option[Rep[NestedCollectionFlat[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[NestedCollectionFlat[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object slice {
      def unapply(d: Def[_]): Option[(Rep[NestedCollectionFlat[A]], Rep[Int], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(offset, length, _*), _) if receiver.elem.isInstanceOf[NestedCollectionFlatElem[_]] && method.getName == "slice" =>
          Some((receiver, offset, length)).asInstanceOf[Option[(Rep[NestedCollectionFlat[A]], Rep[Int], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollectionFlat[A]], Rep[Int], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_many {
      def unapply(d: Def[_]): Option[(Rep[NestedCollectionFlat[A]], Coll[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(indices, _*), _) if receiver.elem.isInstanceOf[NestedCollectionFlatElem[_]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[NestedCollectionFlat[A]], Coll[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollectionFlat[A]], Coll[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[NestedCollectionFlat[A]], Rep[Collection[A] => B @uncheckedVariance]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[NestedCollectionFlatElem[_]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[NestedCollectionFlat[A]], Rep[Collection[A] => B @uncheckedVariance]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollectionFlat[A]], Rep[Collection[A] => B @uncheckedVariance]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[NestedCollectionFlat[A]], RepMonoid[Collection[A @uncheckedVariance]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[NestedCollectionFlatElem[_]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[NestedCollectionFlat[A]], RepMonoid[Collection[A @uncheckedVariance]]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollectionFlat[A]], RepMonoid[Collection[A @uncheckedVariance]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zip {
      def unapply(d: Def[_]): Option[(Rep[NestedCollectionFlat[A]], Coll[B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(ys, _*), _) if receiver.elem.isInstanceOf[NestedCollectionFlatElem[_]] && method.getName == "zip" =>
          Some((receiver, ys)).asInstanceOf[Option[(Rep[NestedCollectionFlat[A]], Coll[B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollectionFlat[A]], Coll[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object update {
      def unapply(d: Def[_]): Option[(Rep[NestedCollectionFlat[A]], Rep[Int], Rep[Collection[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(idx, value, _*), _) if receiver.elem.isInstanceOf[NestedCollectionFlatElem[_]] && method.getName == "update" =>
          Some((receiver, idx, value)).asInstanceOf[Option[(Rep[NestedCollectionFlat[A]], Rep[Int], Rep[Collection[A]]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollectionFlat[A]], Rep[Int], Rep[Collection[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object updateMany {
      def unapply(d: Def[_]): Option[(Rep[NestedCollectionFlat[A]], Coll[Int], Coll[Collection[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(idxs, vals, _*), _) if receiver.elem.isInstanceOf[NestedCollectionFlatElem[_]] && method.getName == "updateMany" =>
          Some((receiver, idxs, vals)).asInstanceOf[Option[(Rep[NestedCollectionFlat[A]], Coll[Int], Coll[Collection[A]]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollectionFlat[A]], Coll[Int], Coll[Collection[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[NestedCollectionFlat[A]], Rep[Collection[A @uncheckedVariance] => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[NestedCollectionFlatElem[_]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[NestedCollectionFlat[A]], Rep[Collection[A @uncheckedVariance] => Boolean]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollectionFlat[A]], Rep[Collection[A @uncheckedVariance] => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMapBy {
      def unapply(d: Def[_]): Option[(Rep[NestedCollectionFlat[A]], Rep[Collection[A @uncheckedVariance] => Collection[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[NestedCollectionFlatElem[_]] && method.getName == "flatMapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[NestedCollectionFlat[A]], Rep[Collection[A @uncheckedVariance] => Collection[B]]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollectionFlat[A]], Rep[Collection[A @uncheckedVariance] => Collection[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object append {
      def unapply(d: Def[_]): Option[(Rep[NestedCollectionFlat[A]], Rep[Collection[A @uncheckedVariance]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(value, _*), _) if receiver.elem.isInstanceOf[NestedCollectionFlatElem[_]] && method.getName == "append" =>
          Some((receiver, value)).asInstanceOf[Option[(Rep[NestedCollectionFlat[A]], Rep[Collection[A @uncheckedVariance]]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollectionFlat[A]], Rep[Collection[A @uncheckedVariance]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sortBy {
      def unapply(d: Def[_]): Option[(Rep[NestedCollectionFlat[A]], Rep[Collection[A] => O], Ordering[O]) forSome {type A; type O}] = d match {
        case MethodCall(receiver, method, Seq(by, o, _*), _) if receiver.elem.isInstanceOf[NestedCollectionFlatElem[_]] && method.getName == "sortBy" =>
          Some((receiver, by, o)).asInstanceOf[Option[(Rep[NestedCollectionFlat[A]], Rep[Collection[A] => O], Ordering[O]) forSome {type A; type O}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[NestedCollectionFlat[A]], Rep[Collection[A] => O], Ordering[O]) forSome {type A; type O}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object NestedCollectionFlatCompanionMethods {
    object fromJuggedArray {
      def unapply(d: Def[_]): Option[Rep[Array[Array[T]]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(arr, _*), _) if receiver.elem == NestedCollectionFlatCompanionElem && method.getName == "fromJuggedArray" =>
          Some(arr).asInstanceOf[Option[Rep[Array[Array[T]]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Array[Array[T]]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkNestedCollectionFlat[A]
    (values: Coll[A], segments: PairColl[Int, Int])(implicit eA: Elem[A]): Rep[NestedCollectionFlat[A]] =
    new ExpNestedCollectionFlat[A](values, segments)
  def unmkNestedCollectionFlat[A](p: Rep[NestedCollection[A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: NestedCollectionFlatElem[A] @unchecked =>
      Some((p.asRep[NestedCollectionFlat[A]].values, p.asRep[NestedCollectionFlat[A]].segments))
    case _ =>
      None
  }

  object CollectionMethods {
    object length {
      def unapply(d: Def[_]): Option[Rep[Collection[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[Collection[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Collection[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object arr {
      def unapply(d: Def[_]): Option[Rep[Collection[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "arr" =>
          Some(receiver).asInstanceOf[Option[Rep[Collection[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Collection[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object lst {
      def unapply(d: Def[_]): Option[Rep[Collection[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "lst" =>
          Some(receiver).asInstanceOf[Option[Rep[Collection[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Collection[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object seq {
      def unapply(d: Def[_]): Option[Rep[Collection[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "seq" =>
          Some(receiver).asInstanceOf[Option[Rep[Collection[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Collection[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[Collection[Item]], Rep[Int]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[Collection[Item]], Rep[Int]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[Item]], Rep[Int]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_many {
      def unapply(d: Def[_]): Option[(Rep[Collection[Item]], Coll[Int]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(indices, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, indices)).asInstanceOf[Option[(Rep[Collection[Item]], Coll[Int]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[Item]], Coll[Int]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[Collection[Item]], Rep[Item => B @uncheckedVariance]) forSome {type Item; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Collection[Item]], Rep[Item => B @uncheckedVariance]) forSome {type Item; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[Item]], Rep[Item => B @uncheckedVariance]) forSome {type Item; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zip {
      def unapply(d: Def[_]): Option[(Rep[Collection[Item]], Coll[B]) forSome {type Item; type B}] = d match {
        case MethodCall(receiver, method, Seq(ys, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "zip" =>
          Some((receiver, ys)).asInstanceOf[Option[(Rep[Collection[Item]], Coll[B]) forSome {type Item; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[Item]], Coll[B]) forSome {type Item; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object slice {
      def unapply(d: Def[_]): Option[(Rep[Collection[Item]], Rep[Int], Rep[Int]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(offset, length, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "slice" =>
          Some((receiver, offset, length)).asInstanceOf[Option[(Rep[Collection[Item]], Rep[Int], Rep[Int]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[Item]], Rep[Int], Rep[Int]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[Collection[Item]], RepMonoid[Item @uncheckedVariance]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(m, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "reduce" =>
          Some((receiver, m)).asInstanceOf[Option[(Rep[Collection[Item]], RepMonoid[Item @uncheckedVariance]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[Item]], RepMonoid[Item @uncheckedVariance]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object update {
      def unapply(d: Def[_]): Option[(Rep[Collection[Item]], Rep[Int], Rep[Item @uncheckedVariance]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(idx, value, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "update" =>
          Some((receiver, idx, value)).asInstanceOf[Option[(Rep[Collection[Item]], Rep[Int], Rep[Item @uncheckedVariance]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[Item]], Rep[Int], Rep[Item @uncheckedVariance]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object updateMany {
      def unapply(d: Def[_]): Option[(Rep[Collection[Item]], Coll[Int], Coll[Item @uncheckedVariance]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(idxs, vals, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "updateMany" =>
          Some((receiver, idxs, vals)).asInstanceOf[Option[(Rep[Collection[Item]], Coll[Int], Coll[Item @uncheckedVariance]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[Item]], Coll[Int], Coll[Item @uncheckedVariance]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object indexes {
      def unapply(d: Def[_]): Option[Rep[Collection[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "indexes" =>
          Some(receiver).asInstanceOf[Option[Rep[Collection[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Collection[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filterBy {
      def unapply(d: Def[_]): Option[(Rep[Collection[Item]], Rep[Item @uncheckedVariance => Boolean]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "filterBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Collection[Item]], Rep[Item @uncheckedVariance => Boolean]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[Item]], Rep[Item @uncheckedVariance => Boolean]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMapBy {
      def unapply(d: Def[_]): Option[(Rep[Collection[Item]], Rep[Item @uncheckedVariance => Collection[B]]) forSome {type Item; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "flatMapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Collection[Item]], Rep[Item @uncheckedVariance => Collection[B]]) forSome {type Item; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[Item]], Rep[Item @uncheckedVariance => Collection[B]]) forSome {type Item; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object append {
      def unapply(d: Def[_]): Option[(Rep[Collection[Item]], Rep[Item @uncheckedVariance]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, Seq(value, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "append" =>
          Some((receiver, value)).asInstanceOf[Option[(Rep[Collection[Item]], Rep[Item @uncheckedVariance]) forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[Item]], Rep[Item @uncheckedVariance]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object foldLeft {
      def unapply(d: Def[_]): Option[(Rep[Collection[Item]], Rep[S], Rep[((S, Item)) => S]) forSome {type Item; type S}] = d match {
        case MethodCall(receiver, method, Seq(init, f, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "foldLeft" =>
          Some((receiver, init, f)).asInstanceOf[Option[(Rep[Collection[Item]], Rep[S], Rep[((S, Item)) => S]) forSome {type Item; type S}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[Item]], Rep[S], Rep[((S, Item)) => S]) forSome {type Item; type S}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sortBy {
      def unapply(d: Def[_]): Option[(Rep[Collection[Item]], Rep[Item => O], Ordering[O]) forSome {type Item; type O}] = d match {
        case MethodCall(receiver, method, Seq(by, o, _*), _) if receiver.elem.isInstanceOf[CollectionElem[_, _]] && method.getName == "sortBy" =>
          Some((receiver, by, o)).asInstanceOf[Option[(Rep[Collection[Item]], Rep[Item => O], Ordering[O]) forSome {type Item; type O}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Collection[Item]], Rep[Item => O], Ordering[O]) forSome {type Item; type O}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CollectionCompanionMethods {
    // WARNING: Cannot generate matcher for method `manager`: Method's return type CollectionManager is not a Rep

    object apply {
      def unapply(d: Def[_]): Option[Rep[Array[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(arr, _*), _) if receiver.elem == CollectionCompanionElem && method.getName == "apply" =>
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
        case MethodCall(receiver, method, Seq(arr, _*), _) if receiver.elem == CollectionCompanionElem && method.getName == "fromArray" =>
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
        case MethodCall(receiver, method, Seq(arr, _*), _) if receiver.elem == CollectionCompanionElem && method.getName == "fromList" =>
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
        case MethodCall(receiver, method, Seq(len, v, _*), _) if receiver.elem == CollectionCompanionElem && method.getName == "replicate" =>
          Some((len, v)).asInstanceOf[Option[(Rep[Int], Rep[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Int], Rep[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object empty {
      def unapply(d: Def[_]): Option[Unit forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem == CollectionCompanionElem && method.getName == "empty" =>
          Some(()).asInstanceOf[Option[Unit forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object singleton {
      def unapply(d: Def[_]): Option[Rep[T] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem == CollectionCompanionElem && method.getName == "singleton" =>
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
        case MethodCall(receiver, method, Seq(l, _*), _) if receiver.elem == CollectionCompanionElem && method.getName == "indexRange" =>
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

object Collections_Module {
  val packageName = "scalan.collections"
  val name = "Collections"
  val dump = "H4sIAAAAAAAAANVYTWwbRRSe3dhxbFdpm1at2lISUkNpVeK0AvVQocpxEyhy4qibVshUlcbribPt7uxmdxzZHCrghEBcEFeEeu+tF6RKvSAkxIETAiTOnEoRqoCKA4g3sz/e9XodJ2oC+LDanZn3973vPc/M3Uco7djoBUfFOqYzBmF4RhHvJYcVlHnKNNZZNBstnVwiq+8d+lxdpHOOjPbW0Ogadi45eg1l3Zf5thW8K2S9grKYqsRhpu0w9FxFWCiqpq4TlWkmLWqG0WK4rpNiRXPYhQpK1c1GZx3dRlIF7VNNqtqEEaWsY8chjjc+RrhHWvCdFd+dqtW1QYs8imIoihUbawzcBxv73PVXiKV0qEk7BkPjnmtVi7sFazKaYZk2801kQN2a2fA/UxTDAJqo3MQbuAgmmkWF2RptgmTewuot3CRLsIQvT4HDDtFXVzqW+B6poJxD1gGgy4ali5G2hRCCDJwTTsx08ZkJ8Jnh+BQUYmtY197GfHLZNtsd5P6kEYTaFqg4s4kKXwOZp43CB9fVt54oeUPmwm3uSkZEOAqKJhPYIFIBOH515WPn8Wt3zssoV0M5zSnVHWZjlYVT7qGVx5SaTPgcAIjtJmRrOilbwkoJ1vRQIquahoUpaPKg3AN50jVVY3wxH9vjZScB+gyziL9UaltSEO9UQryCN2Ws68sPj7z0/M/zb8pIjprIgkoFiG/7ShnKlQP8AwMnkgxYZNnWDCD0Bnnli/tXf32wlBY2JhpkFbd0dg3rLeLSy7PYtc6NyS+eYih1lWqMD2Xb3WdmQFwBwicf/tL4chZdl4O8eGEMRwVQkXZ++C7/7amLMhqricJZ0HGzBqlx5nViVO2ySVkNjZkbxHZnMhtY5299qZHxwvYSFkZ6BJBmaCqxxC3C03BBlJPkA5B3K2LJpKSwsFz4Q/n6k7uc8Dba4864Nf+3dv6vH8dXmagFhkZ1QptsTTi1l6ERaBYeHvx5gCFpFkYv076Y51zFimmQ/dOPtRt3PmQCXakd7RnV+k0gyQUh9+wAoP3e9XttVv7tyPefySgLeNY1ZmCrMDtkxe1gFaEQNADWeNnr24Igsz2TnKjd8hCQxTAMHpOQisNRiXLY98lu0zosXvsaCK3KS1F34pXqTaQuM2L09841F1p+NOCbsAbEwLadQB0YSZdsG3eGNRc2elI8Tw/E+2x08kA3wCoUoDA9BOjP9BFLRD4MhdQDRZrwyIIweUcYJvJkxyeDvnQ8uaMCOQ9dqRzUH118IKP0Gyi9Cu3GqaB03WzRhs962F8w0mZz/pgUZT2wHNvYCFguflOoG33c+d3kmO6wZI6l+J/9LlFsIpopbnkIhh2LS+0uwRLcngxJX/+v5Rw2jgNyrije9M7nfH8UPDA8RMqPxoR2N+P9nX46CR9fxpqdlHSp1A2Nf85tlwAyZCIx/4mc88yH7SWor29Lff9wtsqwcz0MiwKqVONB9GFYTGhbDJNJaQC9InAmKZgbpCAOWFK8m5IzTK3+C+Z6vf13qJviZ9KtsGvAftgiKy1LJy/f//PG+++8bonNdewYtp04doK5paqydeaC0P+VuV68u8HcfUtw7CGNLfS9Ick6usEP3UEzTJV97g6CslcJnJqaBqEsUDPmA+Wq4o9C8Hgqf8sHewGBY/Ywm7Hj/eR2moA+fxKdHpZCYXUhkV7kRuAIvXnTGXBPU3JP1aRx4t67j16tn/lI3NOkxWGbi7tXFOL1GL9E2N+i6hpRb5HGNWxr4pDuQ7PZvmwzFuT5aXwBG5reOZsYUy9Do/u2RUxxk9g9XkR5ETqE9WGDFbMWz5g0xF6qN7i+Ow1J3n5DjwbV83+3U81qqKi2um9LiinWCbdbPxzple4ab2E+5CKcnLwS6d4yOx5VbTSdUD6Kd4MENXj7yadLp7+595Oonhy/izIpb5NchRS93YziMeHqAyYaLaqxziVHD/kP5OLXVML3fwBU7ike0xgAAA=="
}
}

