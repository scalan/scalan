package tests.scalan.rx

import scalan.{ScalanCtxStaged, ScalanCtxShallow}
import scalan.rx._
import tests.BaseTests
import shapeless._

trait ReactiveSamples extends ReactiveDsl {

  val e = element[ObservableImpl[Int]]

  def sample(xs: Obs[Int]) = {
    val e: Elem[Observable[Int]] = xs.Elem
    e
  }

}

class ReactiveTests extends BaseTests with ScalanCtxStaged with ReactiveDslExp {

//  test("element for concrete class should be resolvable") {
//    val e = element[ObservableImpl[Int]]
//    assertNotNull(e)
//
//    // element[Observable[Int]] no such element
//  }
  test("element_for_concrete_class_should_be_resolvable") {

    val e = element[ObservableImpl[Int]]
    e should not be(null)

  }

  test("methods should be resolvable for abstract and concrete class") {

    val e = element[ObservableImpl[Int]]
    val v1 = e.defaultOf.value  // concrete
    val v2: Obs[Int] = v1       // abstract

    val i1 = v1.index
    val i2 = v2.index

    val e1 = v1.Elem
    val e2 = v2.Elem
  }

}
