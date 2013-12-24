package tests.scalan.rx

import scalan.{ScalanCtxStaged, ScalanCtxShallow}
import scalan.rx._
import tests.BaseTests
import shapeless._

trait ReactiveSamples extends ReactiveDsl {

  //val e1 = element[Observable[Int]]  // No Element available ...
  val e2 = element[ObservableImpl[Int]]

//  def getObsElem(xs: Obs[Int]): E[Observable[Int]] = xs.Elem
//  def getObsImplElem(xs: Rep[ObservableImpl[Int]]): E[Observable[Int]] = xs.Elem

  def getObsField(xs: Obs[Int]): Rep[Int] = xs.index
  def getObsImplField(xs: Rep[ObservableImpl[Int]]): Rep[Int] = xs.index

  def getAbstractFromConcrete(xs: Rep[ObservableImpl[Int]]): Obs[Int] = xs
}

class ReactiveTests extends BaseTests with ScalanCtxStaged with ReactiveDslExp with ReactiveSamples {

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

  test("in staged context: methods should be resolvable for abstract and concrete classes") {

    val e = element[ObservableImpl[Int]]
    val v1 = e.defaultOf.value  // concrete
    val v2: Obs[Int] = v1       // abstract

    val i1 = v1.index
    val i2 = v2.index
    val e1 = v1.Elem
    val e2 = v2.Elem
  }

  test("abstract code applied in staged context") {
    val e = element[ObservableImpl[Int]]
    val v1 = e.defaultOf.value  // concrete
    val v2 = getAbstractFromConcrete(v1)       // abstract

    val i1 = getObsImplField(v1)
    val i2 = getObsField(v2)
//    val e1 = getObsImplElem(v1)
//    val e2 = getObsElem(v2)
    all(List(i1, i2/*, e1, e2*/)) should not be(null)
  }

}
