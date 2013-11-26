/**
 * User: s00747473
 * Date: 11/16/13
 */
package tests.scalan.rx

import scalan.ScalanCtxStaged
import scalan.rx.ReactiveExp
import tests.BaseTests

//@RunWith(classOf[JUnitRunner])
class ReactiveTests extends BaseTests with ScalanCtxStaged with ReactiveExp {

//  test("element for concrete class should be resolvable") {
//    val e = element[ObservableImpl[Int]]
//    assertNotNull(e)
//
//    // element[Observable[Int]] no such element
//  }
  test("element for concrete class should be resolvable") {
    val e = element[ObservableImpl[Int]]
    assert(e != null)

    // element[Observable[Int]] no such element
  }

 }
