package scalan.common

import scala.language.reflectiveCalls
import scalan._

class KindsTests extends BaseTests { suite =>

  class ConvProgStaged(testName: String) extends TestContext(this, testName) with KindsExamples with KindsDslExp {
  }
  class ConvProgSeq(testName: String) extends ScalanCtxSeq with  KindsExamples with KindsDslSeq {
  }

  test("simple kinds tests") {
    val ctx = new ConvProgStaged("simple kinds tests")
    ctx.emit("t1", ctx.t1)
  }

  test("kindMap") {
    pending
    val name = "kindMap"
    val ctx = new ConvProgStaged(name)
    ctx.emit(name, ctx.kindMap)
  }

}
