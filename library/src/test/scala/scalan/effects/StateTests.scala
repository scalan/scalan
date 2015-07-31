package scalan.effects

import java.lang.reflect.Method

import scala.language.reflectiveCalls
import scalan.monads.{MonadsDslExp, MonadsDsl}
import scalan.{BaseCtxTests, ScalanCtxExp}

class StateTests extends BaseCtxTests {

  test("zipWithIndex") {
    val ctx = new TestContext("zipWithIndex") with MonadsDslExp with StateExamples {
      val State = new State0Manager[Int]
      override def isInvokeEnabled(d: Def[_], m: Method) = true
    }
    ctx.emit("zipWithIndex", ctx.zipArrayWithIndexW)
  }


}
