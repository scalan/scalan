/**
 * User: Alexander Slesarenko
 * Date: 11/23/13
 */
package scalan.arrays

import scalan._

trait ArrayDescriptors extends TypeDescriptors { self: Scalan =>

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
