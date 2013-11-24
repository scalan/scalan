/**
 * User: s00747473
 * Date: 11/16/13
 */
package tests.scalan.rx

import org.junit.Test
import org.junit.Assert._
import scalan.{ScalanCtxStaged, ScalanCtxShallow}
import scalan.rx.ReactiveExp


class ReactiveTests extends ScalanCtxStaged with ReactiveExp {
  val seq = new ScalanCtxShallow

  @Test def observableElementCreated() {
    val e = element[ObservableImpl[Int]]
    assertNotNull(e)

    // element[Observable[Int]] no such element
  }

 }
