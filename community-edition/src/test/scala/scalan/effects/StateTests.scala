package scalan.effects

import java.lang.reflect.Method

import scala.language.reflectiveCalls
import scalan.monads.{MonadsDslExp, MonadsDsl}
import scalan.{BaseTests, ScalanCtxExp, TestContext}

class StateTests extends BaseTests {

  test("zipWithIndex") {
    val ctx = new TestContext(this, "zipWithIndex") with MonadsDslExp with StateExamples {
      val F = state0Monad[Int]
      override def isInvokeEnabled(d: Def[_], m: Method) = true
    }
    ctx.emit("zipWithIndex", ctx.zipArrayWithIndexW)
  }


}
