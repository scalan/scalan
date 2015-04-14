package scalan.common

import scala.language.reflectiveCalls
import scalan._

class ConverterTests extends BaseTests { suite =>

  trait ConvProg extends CommonExamples {
  }

  class ConvProgStaged(testName: String) extends TestContext(this, testName) with  ConvProg with SegmentsDslExp {
  }
  class ConvProgSeq(testName: String) extends ScalanCtxSeq with  ConvProg with SegmentsDslSeq {
  }

  test("simple converter tests") {
    val ctx = new ConvProgStaged("simple converter tests")
    ctx.emit("t1", ctx.t1)
    ctx.emit("t2", ctx.t2)
    ctx.emit("t3", ctx.t3)
    ctx.emit("t4", ctx.t4)
    ctx.emit("t5", ctx.t5)
  }

  ignore("convertToCentered") {
    val ctx = new ConvProgStaged("convertToCentered")
    ctx.emit("t6", ctx.t6)
  }

  test("convertSeq") {
    val ctx = new ConvProgSeq("convertSeq")
    val res = ctx.t4((10,20))
    assertResult((10,30))(res)
  }

  test("converIfThenElse") {
    val ctx = new ConvProgStaged("converIfThenElse")
    ctx.emit("t7", ctx.t7)
    ctx.emit("t9", ctx.t9)
  }

  test("converIfThenElseWithPair") {
    val ctx = new ConvProgStaged("converIfThenElseWithPair")
    ctx.emit("t8", ctx.t8)
  }

  test("converIfThenElseWithOption") {
    val ctx = new ConvProgStaged("converIfThenElseWithSum")
    ctx.emit("t10", ctx.t10)
  }

  test("converIfThenElseWithSum") {
    val ctx = new ConvProgStaged("converIfThenElseWithSum")
    ctx.emit("t11", ctx.t11)
    ctx.emit("t12", ctx.t12)
  }

  test("convertSumFold") {
    val ctx = new ConvProgStaged("converIfThenElseWithSum")
    ctx.emit("t13", ctx.t13)
  }

}
