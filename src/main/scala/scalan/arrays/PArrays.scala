/**
 * User: Alexander Slesarenko
 * Date: 11/23/13
 */
package scalan.arrays

import scalan.{ScalanStaged, Scalan, Base}

trait PArrays extends Base { self: Scalan =>

}

trait PArraysExp extends PArrays { self: ScalanStaged =>

//  def unzipPAForEach[A](arr: PA[(A,Any)], list: List[Exp[Any]])
//                       (implicit eA: Elem[A], eAny: Elem[Any]): List[(Exp[Any], PA[Any])] =
//    list match {
//      case x :: y :: Nil => List(x -> arr.as.asPA[Any], y -> arr.bs)
//      case x :: y :: xs => {
//        (x, arr.as.asPA[Any]) :: arr.bs.asPairArray({ eB:Elem[Any] => eC:Elem[Any] => pairs: PA[(Any,Any)] =>
//          unzipPAForEach(pairs, y :: xs)(eB, eC)
//        })
//      }
//      case _ => !!!("Expected PairArray but was %s".format(arr))
//    }


}
