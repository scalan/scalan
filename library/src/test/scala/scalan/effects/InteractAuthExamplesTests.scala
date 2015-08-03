package scalan.examples

import java.io.File
import java.lang.reflect.Method

import scala.language.reflectiveCalls
import scalan.effects.{CrossDomainExample, InteractExample}
import scalan.{BaseCtxTests, ScalanCtxExp}

class InteractAuthExamplesTests extends BaseCtxTests {

  test("interactsStaged") {
    val ctx = new TestContext with InteractionsDslExp with InteractExample {
      override def isInvokeEnabled(d: Def[_], m: Method) = true
    }
    ctx.emit("app", ctx.app)
    ctx.emit("runApp", ctx.runApp)
  }


  // TODO test passes but takes multiple minutes and has huge IDs in the runApp graph (s1932149!)
  test("crossDomainStaged") {
    pending // TODO: Long running test
    val ctx = new TestContext with CrossDomainExample with InteractionsDslExp with AuthenticationsDslExp {
      override def isInvokeEnabled(d: Def[_], m: Method) = true
    }
    ctx.emit("app", ctx.app)
    ctx.emit("runApp", ctx.runApp)
  }

}
