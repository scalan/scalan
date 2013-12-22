/**
 * User: s00747473
 * Date: 11/16/13
 */
package tests.scalan.primitives

import scalan.ScalanCtxStaged
import shapeless._
import tests.BaseTests

class HListTests extends BaseTests with ScalanCtxStaged {
  import poly._

//  object choose extends (Set ~> Option) {
//    def apply[T](s : Set[T]) = s.headOption
//  }
//
//  object size1 extends Poly1 {
//    implicit def caseInt = at[Rep[Int]](x => 1)
//    implicit def caseString = at[Rep[String]](x => 10)
//    implicit def caseTuple[T, U]
//    (implicit st : Case.Aux[Rep[T], Int], su : Case.Aux[Rep[U], Int]) =
//      at[(Rep[T], Rep[U])](t => size1(t._1)+size1(t._2))
//  }
//
//  object addSize extends Poly2 {
//    implicit def default[T](implicit st: size1.Case.Aux[Rep[T], Int]) =
//      at[Int, Rep[T]]{ (acc, t) => acc + size1(t) }
//  }
//
//
//  test("stagedTuples") {
//    val l = toRep(23) :: toRep("foo") :: HNil //(toRep(13), toRep("wibble")) :: HNil
//    val sz = l.foldLeft(0)(addSize)
//    sz shouldBe 11
//
//  }

 }
