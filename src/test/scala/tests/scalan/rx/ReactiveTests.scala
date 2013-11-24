/**
 * User: s00747473
 * Date: 11/16/13
 */
package tests.scalan.rx

import org.junit.Test
import org.junit.Assert._
import scalan.{ScalanCtxStaged, ScalanCtxShallow}
import scalan.rx.ReactiveExp
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

//@RunWith(classOf[JUnitRunner])
class ReactiveTests extends /*FunSuite with*/ ScalanCtxStaged with ReactiveExp {

//  test("element for concrete class should be resolvable") {
//    val e = element[ObservableImpl[Int]]
//    assertNotNull(e)
//
//    // element[Observable[Int]] no such element
//  }
  @Test def element_for_concrete_class_should_be_resolvable {
    val e = element[ObservableImpl[Int]]
    assertNotNull(e)

    // element[Observable[Int]] no such element
  }

 }
