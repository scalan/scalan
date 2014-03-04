
package scalan.arrays

import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance
import scalan.common.Default
import Default._
import scalan._
import scala.reflect.runtime.universe._

trait PArraysAbs extends PArrays
{ self: PArraysDsl =>

  // single proxy for each type family
  implicit def proxyPArray[A:Elem](p: PA[A]): PArray[A] = {
    implicit val ctA = element[A].classTag;
    proxyOps[PArray[A]](p)
  }


  trait PArrayElem[From,To] extends ViewElem[From, To]
  trait PArrayCompanionElem extends CompanionElem[PArrayCompanionAbs]

  implicit def defaultPArrayElement[A:Elem]: Elem[PArray[A]] = element[A] match {
    case baseE: BaseElem[a] => element[BaseArray[A]].asElem[PArray[A]]
    case _ => ???
  }
  implicit lazy val PArrayCompanionElem: PArrayCompanionElem = new PArrayCompanionElem {
    def tag = typeTag[PArrayCompanionAbs]
    def defaultRep = defaultVal(PArray)
  }

  trait PArrayCompanionAbs extends PArrayCompanionOps {}
  def PArray: Rep[PArrayCompanionAbs]  // singleton for PArray class methods
  implicit def defaultOfPArray[A](implicit da: Elem[A]): Default[Rep[PArray[A]]] = PArray.defaultOf[A]

  implicit def proxyPArrayCompanion(p: Rep[PArrayCompanionOps]): PArrayCompanionOps = {
    proxyOps[PArrayCompanionOps](p, Some(true))
  }


  //------------------------------- BaseArray ------------------------------------
  // elem for concrete class
  trait BaseArrayElem[A] extends PArrayElem[BaseArrayData[A], BaseArray[A]]
  trait BaseArrayCompanionElem extends CompanionElem[BaseArrayCompanionAbs]

  // state representation type
  type BaseArrayData[A] = Array[A]

  // 3) Iso for concrete class
  abstract class BaseArrayIso[A](implicit  eA: Elem[A])
    extends IsoBase[BaseArrayData[A], BaseArray[A]] {
    override def from(p: Rep[BaseArray[A]]) = unmkBaseArray(p) match { case Some(arr) => arr }
    override def to(p: Rep[Array[A]]) = {
      val arr = p
      mkBaseArray(arr)
    }
    lazy val tag = {
      implicit val tA = eA.tag
      typeTag[BaseArray[A]]
    }
    import TagImplicits._
    lazy val defaultRepTo = defaultVal[Rep[BaseArray[A]]](mkBaseArray(element[Array[A]].defaultRepValue))
  }

  // 4) constructor and deconstructor
  trait BaseArrayCompanionAbs extends BaseArrayCompanionOps {
    //def apply[A](p: PArray[A])(implicit  eA: Elem[A]): Rep[BaseArray[A]]
    //    = mkBaseArray(p.arr)
    def apply[A]
          (arr: Rep[Array[A]])
          (implicit  eA: Elem[A]): Rep[BaseArray[A]]
        = mkBaseArray(arr)
    def unapply[A:Elem](p: Rep[BaseArray[A]]) = unmkBaseArray(p)
  }
  def BaseArray: Rep[BaseArrayCompanionAbs]
  implicit def proxyBaseArrayCompanion(p: Rep[BaseArrayCompanionAbs]): BaseArrayCompanionAbs = {
    proxyOps[BaseArrayCompanionAbs](p, Some(true))
  }
  implicit lazy val BaseArrayCompanionElem: BaseArrayCompanionElem = new BaseArrayCompanionElem {
    def tag = typeTag[BaseArrayCompanionAbs]
    def defaultRep = defaultVal(BaseArray)
  }

  implicit def proxyBaseArray[A](p: Rep[BaseArray[A]])(implicit  eA: Elem[A]): BaseArrayOps[A] = {
    implicit val ctA = eA.classTag
    proxyOps[BaseArrayOps[A]](p)
  }

  implicit def extendBaseArray[A](p: Rep[BaseArray[A]])(implicit  eA: Elem[A]) = new {
    def toData: Rep[BaseArrayData[A]] = isoBaseArray(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoBaseArray[A](implicit  eA: Elem[A]): Iso[BaseArrayData[A], BaseArray[A]]

  // 6) smart constructor and deconstructor
  def mkBaseArray[A](arr: Rep[Array[A]])(implicit  eA: Elem[A]): Rep[BaseArray[A]]
  def unmkBaseArray[A](p: Rep[BaseArray[A]])(implicit  eA: Elem[A]): Option[(Rep[Array[A]])]


  //------------------------------- PairArray ------------------------------------
  // elem for concrete class
  trait PairArrayElem[A, B] extends PArrayElem[PairArrayData[A, B], PairArray[A, B]]
  trait PairArrayCompanionElem extends CompanionElem[PairArrayCompanionAbs]

  // state representation type
  type PairArrayData[A, B] = (PArray[A], PArray[B])

  // 3) Iso for concrete class
  abstract class PairArrayIso[A, B](implicit  eA: Elem[A],  eB: Elem[B])
    extends IsoBase[PairArrayData[A, B], PairArray[A, B]] {
    override def from(p: Rep[PairArray[A,B]]) = unmkPairArray(p) match { case Some((as, bs)) => Pair(as, bs) }
    override def to(p: Rep[(PArray[A], PArray[B])]) = {
      val Pair(as, bs) = p
      mkPairArray(as, bs)
    }
    lazy val tag = {
      implicit val tA = eA.tag
      implicit val tB = eB.tag
      typeTag[PairArray[A, B]]
    }
    lazy val defaultRepTo = {
      implicit val dA = eA.defaultRep
      implicit val dB = eB.defaultRep
      val as = Default.defaultOf[Rep[PArray[A]]]
      val bs = Default.defaultOf[Rep[PArray[B]]]
      defaultVal[Rep[PairArray[A, B]]](mkPairArray(as, bs))
    }
  }

  // 4) constructor and deconstructor
  trait PairArrayCompanionAbs extends PairArrayCompanionOps {
    def apply[A, B](p: Rep[PairArrayData[A, B]])(implicit  eA: Elem[A],  eB: Elem[B]): Rep[PairArray[A, B]]
        = isoPairArray(eA, eB).to(p)
    def apply[A, B]
          (as: PA[A], bs: PA[B])
          (implicit  eA: Elem[A],  eB: Elem[B]): Rep[PairArray[A, B]]
        = mkPairArray(as, bs)
    def unapply[A:Elem, B:Elem](p: Rep[PairArray[A, B]]) = unmkPairArray(p)
  }
  def PairArray: Rep[PairArrayCompanionAbs]
  implicit def proxyPairArrayCompanion(p: Rep[PairArrayCompanionAbs]): PairArrayCompanionAbs = {
    proxyOps[PairArrayCompanionAbs](p, Some(true))
  }
  implicit lazy val PairArrayCompanionElem: PairArrayCompanionElem = new PairArrayCompanionElem {
    def tag = typeTag[PairArrayCompanionAbs]
    def defaultRep = defaultVal(PairArray)
  }

  implicit def proxyPairArray[A, B](p: Rep[PairArray[A, B]])(implicit  eA: Elem[A],  eB: Elem[B]): PairArrayOps[A, B] = {
    implicit val ctA = eA.classTag
    implicit val ctB = eB.classTag
    proxyOps[PairArrayOps[A, B]](p)
  }

  implicit def extendPairArray[A, B](p: Rep[PairArray[A, B]])(implicit  eA: Elem[A],  eB: Elem[B]) = new {
    def toData: Rep[PairArrayData[A, B]] = isoPairArray(eA, eB).from(p)
  }

  // 4) implicit resolution of Iso
  implicit def isoPairArray[A, B](implicit  eA: Elem[A],  eB: Elem[B]): Iso[PairArrayData[A, B], PairArray[A, B]]

  // 5) smart constructor and deconstructor
  def mkPairArray[A, B](as: PA[A], bs: PA[B])(implicit  eA: Elem[A],  eB: Elem[B]): Rep[PairArray[A, B]]
  def unmkPairArray[A, B](p: Rep[PairArray[A, B]])(implicit  eA: Elem[A],  eB: Elem[B]): Option[(Rep[PArray[A]], Rep[PArray[B]])]

}


trait PArraysSeq extends PArraysAbs
{ self: ScalanSeq with PArraysDsl =>


  lazy val PArray = new PArrayCompanionAbs with UserTypeSeq[PArrayCompanionAbs, PArrayCompanionAbs] {
    def selfType: Elem[PArrayCompanionAbs] = element[PArrayCompanionAbs]
  }

  case class SeqBaseArray[A]
      (override val arr: Rep[Array[A]])
      (implicit override val eT: Elem[A])
      extends BaseArray[A](arr) with BaseArrayOps[A]
         with UserTypeSeq[PArray[A], BaseArray[A]] {
    def selfType: Elem[PArray[A]] = element[BaseArray[A]].asInstanceOf[Elem[PArray[A]]]
  }

  lazy val BaseArray = new BaseArrayCompanionAbs with UserTypeSeq[BaseArrayCompanionAbs, BaseArrayCompanionAbs] {
    def selfType: Elem[BaseArrayCompanionAbs] = element[BaseArrayCompanionAbs]
  }

  implicit def isoBaseArray[A](implicit  eA: Elem[A]): Iso[BaseArrayData[A], BaseArray[A]]
    = new BaseArrayIso[A] with SeqIso[BaseArrayData[A], BaseArray[A]] { i =>
        // should use i as iso reference
        override lazy val eTo = new SeqViewElem[BaseArrayData[A], BaseArray[A]]
                                    with BaseArrayElem[A] { val iso = i }
      }


  def mkBaseArray[A]
      (arr: Rep[Array[A]])
      (implicit  eA: Elem[A])
      = new SeqBaseArray[A](arr)
  def unmkBaseArray[A](p: Rep[BaseArray[A]])
      (implicit  eA: Elem[A])
    = Some((p.arr))


  case class SeqPairArray[A, B]
      (override val as: PA[A], override val bs: PA[B])
      (implicit override val eA: Elem[A], override val eB: Elem[B])
      extends PairArray[A, B](as, bs)
          with PairArrayOps[A,B]
          with UserTypeSeq[PArray[(A,B)], PairArray[A,B]] {
    def selfType = element[PairArray[A, B]].asInstanceOf[Elem[PArray[(A,B)]]]
  }

  lazy val PairArray = new PairArrayCompanionAbs with UserTypeSeq[PairArrayCompanionAbs, PairArrayCompanionAbs] {
    def selfType: Elem[PairArrayCompanionAbs] = element[PairArrayCompanionAbs]
  }

  implicit def isoPairArray[A, B](implicit  eA: Elem[A],  eB: Elem[B]): Iso[PairArrayData[A, B], PairArray[A, B]]
    = new PairArrayIso[A, B] with SeqIso[PairArrayData[A, B], PairArray[A, B]] { i =>
        // should use i as iso reference
        override lazy val eTo = new SeqViewElem[PairArrayData[A, B], PairArray[A, B]]
                                    with PairArrayElem[A, B] { val iso = i }
      }


  def mkPairArray[A, B]
      (as: PA[A], bs: PA[B])
      (implicit  eA: Elem[A],  eB: Elem[B])
      = new SeqPairArray[A, B](as, bs)
  def unmkPairArray[A, B](p: Rep[PairArray[A, B]])
      (implicit  eA: Elem[A],  eB: Elem[B])
    = Some((p.as, p.bs))

}


trait PArraysExp extends PArraysAbs with ProxyExp with ViewsExp
{ self: ScalanStaged with PArraysDsl =>

  object ExpPArrayCompanion extends Def[PArrayCompanionAbs] with PArrayCompanionAbs {
    def uniqueOpId = this.getClass.getSimpleName
    def selfType = ???
  }

  lazy val PArray: Exp[PArrayCompanionAbs] = new PArrayCompanionAbs with UserTypeExp[PArrayCompanionAbs, PArrayCompanionAbs] {
    lazy val selfType = element[PArrayCompanionAbs]
    override def mirror(t: Transformer): Rep[_] = this
  }

  case class ExpBaseArray[A]
      (override val arr: Rep[Array[A]])
      (implicit override val eT: Elem[A])
    extends BaseArray[A](arr) with BaseArrayOps[A]
       with UserTypeExp[PArray[A], BaseArray[A]] {
    lazy val selfType = element[BaseArray[A]].asInstanceOf[Elem[PArray[A]]]
    //def elem: Elem[BaseArray[A]] = selfType //.asInstanceOf[Elem[PArray[A]]]
    override def mirror(t: Transformer): Rep[_] = ExpBaseArray[A](t(arr))
  }
  lazy val BaseArray: Exp[BaseArrayCompanionAbs] = new BaseArrayCompanionAbs with UserTypeExp[BaseArrayCompanionAbs, BaseArrayCompanionAbs] {
    lazy val selfType = element[BaseArrayCompanionAbs]
    override def mirror(t: Transformer): Rep[_] = this
  }
  addUserType[ExpBaseArray[_]]


  def mkBaseArray[A]
      (arr: Rep[Array[A]])
      (implicit  eA: Elem[A])
      = new ExpBaseArray[A](arr)
  def unmkBaseArray[A]
      (p: Rep[BaseArray[A]])
      (implicit  eA: Elem[A])
    = Some((p.arr))


  implicit def isoBaseArray[A](implicit  eA: Elem[A]): Iso[BaseArrayData[A], BaseArray[A]]
    = new BaseArrayIso[A] with StagedIso[BaseArrayData[A], BaseArray[A]] { i =>
        // should use i as iso reference
        override lazy val eTo = new StagedViewElem[BaseArrayData[A], BaseArray[A]]
                                    with BaseArrayElem[A] { val iso = i }
      }


  case class ExpPairArray[A, B]
      (override val as: PA[A], override val bs: PA[B])
      (implicit override val eA: Elem[A], override val eB: Elem[B])
    extends PairArray[A, B](as, bs) with PairArrayOps[A,B]
       with UserTypeExp[PArray[(A,B)],PairArray[A, B]] {
    lazy val selfType = element[PairArray[A, B]].asInstanceOf[Elem[PArray[(A,B)]]]
    //def elem: Elem[PairArray[A, B]] = selfType//.asInstanceOf[Elem[PArray[(A,B)]]]
    override def mirror(t: Transformer): Rep[_] = ExpPairArray[A, B](t(as), t(bs))
  }
  lazy val PairArray: Exp[PairArrayCompanionAbs] = new PairArrayCompanionAbs with UserTypeExp[PairArrayCompanionAbs, PairArrayCompanionAbs] {
    lazy val selfType = element[PairArrayCompanionAbs]
    override def mirror(t: Transformer): Rep[_] = this
  }
  addUserType[ExpPairArray[_,_]]


  def mkPairArray[A, B]
      (as: PA[A], bs: PA[B])
      (implicit  eA: Elem[A],  eB: Elem[B])
      = new ExpPairArray[A, B](as, bs)
  def unmkPairArray[A, B]
      (p: Rep[PairArray[A, B]])
      (implicit  eA: Elem[A],  eB: Elem[B])
    = Some((p.as, p.bs))


  implicit def isoPairArray[A, B](implicit  eA: Elem[A],  eB: Elem[B]): Iso[PairArrayData[A, B], PairArray[A, B]]
    = new PairArrayIso[A, B] with StagedIso[PairArrayData[A, B], PairArray[A, B]] { i =>
        // should use i as iso reference
        override lazy val eTo = new StagedViewElem[PairArrayData[A, B], PairArray[A, B]]
                                    with PairArrayElem[A, B] { val iso = i }
      }

}
