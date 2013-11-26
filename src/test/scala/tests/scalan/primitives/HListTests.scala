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

  object choose extends (Set ~> Option) {
    def apply[T](s : Set[T]) = s.headOption
  }

  object size extends Poly1 {
    implicit def caseInt = at[Rep[Int]](x => 1)
    implicit def caseString = at[Rep[String]](x => 10)
    implicit def caseTuple[T, U]
    (implicit st : Case.Aux[Rep[T], Int], su : Case.Aux[Rep[U], Int]) =
      at[(Rep[T], Rep[U])](t => size(t._1)+size(t._2))
  }

  object addSize extends Poly2 {
    implicit def default[T](implicit st: size.Case.Aux[Rep[T], Int]) =
      at[Int, Rep[T]]{ (acc, t) => acc + size(t) }
  }


  test("stagedTuples") {
    val l = toRep(23) :: toRep("foo") :: HNil //(toRep(13), toRep("wibble")) :: HNil
    val sz = l.foldLeft(0)(addSize)
    assertResult(11)(sz)

  }

 }
