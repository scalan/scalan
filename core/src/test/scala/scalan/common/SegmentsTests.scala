package scalan.common

import scala.language.reflectiveCalls
import scalan._

class ConverterTests extends BaseTests { suite =>

  trait ConvProg extends Scalan with SegmentsDsl {
    lazy val t1 = fun { (in: Rep[Interval]) => in.convertTo[Slice] }
    lazy val t2 = fun { (in: Rep[Slice]) => in.convertTo[Interval] }
  }

  class ConvProgStaged(testName: String) extends TestContext(this, testName) with  ConvProg with SegmentsDslExp {
  }

  test("convert") {
    val ctx = new ConvProgStaged("start")
    ctx.emit("t1", ctx.t1)
    ctx.emit("t2", ctx.t2)
  }

}
