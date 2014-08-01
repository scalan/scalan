package scalan.arrays


import scala.reflect.runtime.universe._
import scalan.common.Default
import scalan.common.Default._
import scalan.staged.BaseExp
import scalan.{ScalanStaged, Scalan, Elems}

trait ArrayElems extends Elems { self: Scalan =>
  implicit def ArrayElemExtensions[A](eArr: Elem[Array[A]]): ArrayElem[A] = eArr.asInstanceOf[ArrayElem[A]]

  implicit def arrayRepDefault[A](implicit e: Elem[A]): Default[Rep[Array[A]]] = {
    implicit val aCT = e.classTag
    defaultVal[Rep[Array[A]]](scala.Array.empty[A])
  }

  class ArrayElem[A](implicit val ea: Elem[A]) extends Element[Array[A]] {
    lazy val tag = {
      implicit val tag1 = ea.tag
      implicitly[TypeTag[Array[A]]]
    }
    lazy val defaultRep: Default[Rep[Array[A]]] = arrayRepDefault[A]
  }

  implicit def arrayElement[A](implicit eA: Elem[A]): Elem[Array[A]] = new ArrayElem[A]
  //  abstract class UnitArrayElem extends Element[PArray[Unit]] {
//    def createPA(len: IntRep): PA[Unit]
//  }
//  abstract class PArrayElem[A](val ea: Elem[A]) extends Element[PArray[A]] {
//    //PA factory methods
//    def replicate(count: IntRep, v: Rep[A]): PA[A]               // (3,v) -> (v,v,v)  SVM (distribute)
//    def replicateSeg(count: IntRep, v: PA[A]): PA[A]             // (3,(a,b)) -> (a,b,a,b,a,b)
//    def tabulate(len: IntRep)(f:IntRep => Rep[A]): PA[A]
//    def tabulateSeg(len: IntRep)(f:IntRep => PA[A]): PA[A]
//    def empty: PA[A]
//    def singleton(v:Rep[A]): PA[A] = replicate(element[Int].toRep(1),v)
//    def fromArray(arr: Rep[Array[A]]): PA[A] = ???
//    def createNA(segments: PA[(Int, Int)], values: PA[A]): NA[A]
//  }
//  abstract class PairArrayElem[A,B](val ea: Elem[A], val eb: Elem[B]) extends PArrayElem[(A,B)] {
//    private implicit val eA: Elem[A] = ea
//    private implicit val eB: Elem[B] = eb
//    def createPA(as: PA[A], bs: PA[B]): PA[(A,B)]
//    def empty = createPA(ea.empty, eb.empty)
//    def replicate(count: IntRep, v: Rep[(A, B)]) =  createPA(ea.replicate(count, v._1), eb.replicate(count, v._2))
//    override def replicateSeg(count: IntRep, v: PA[(A, B)]) = {
//      val (a, b) = unzip(v)
//      createPA(ea.replicateSeg(count, a), eb.replicateSeg(count, b))
//    }
//  }
//  abstract class SumArrayElem [A,B](val ea: Elem[A], val eb: Elem[B]) extends Element[PArray[(A|B)]] {
//    def createPA(flags: PA[Boolean], as: PA[A], bs: PA[B]): PA[(A|B)]
//  }

//  def emptyArrayOf[T:Elem] = element[T].empty

//  object PArrayElem {
//    def unapply[A](e: Elem[A]): Option[Elem[_]] = e match {
//      case e: PArrayElem[_] => Some(e.ea)
//      case _ => None
//    }
//  }
//  object NArrayElem {
//    def unapply[A](e: Elem[A]): Option[Elem[_]] = e match {
//      case PArrayElem(e: PArrayElem[_]) => Some(e.ea)
//      case _ => None
//    }
//  }
//
//  implicit def PArrayElemExtensions[A](ePA: Elem[PArray[A]]): PArrayElem[A] = ePA match {
//    case pae: PArrayElem[_] => pae.asInstanceOf[PArrayElem[A]]
//    case ve: ViewElem[_,_] => ve.iso.eB.asInstanceOf[PArrayElem[A]]
//  }

}

trait ArrayElemsExp extends ArrayElems with BaseExp { self: ScalanStaged =>
  override def toRep[A](x: A)(implicit eA: Elem[A]) = eA match {
    case _: ArrayElem[_] => Const(x)
    case _ => super.toRep(x)(eA)
  }
}