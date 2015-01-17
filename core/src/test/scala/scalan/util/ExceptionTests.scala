package scalan.util

import java.io.File
import java.lang.reflect.Method

import scala.language.reflectiveCalls
import scalan.common.{SegmentsDsl, SegmentsDslExp}
import scalan.{TestContext, BaseTests, Scalan, ScalanCtxExp}

class ExceptionTests extends BaseTests { suite =>
  val prefix = new File("test-out/scalan/util/exception/")

  trait MyProg extends Scalan {
    val prefix = suite.prefix
    val subfolder = "/myprog"

  }

  test("exceptions") {
    val ctx = new TestContext with  MyProg {
      def test() = {
        assert(!isInlineThunksOnForce, "precondition for tests")

      }
    }
    ctx.test
    //ctx.emit("t1", ctx.t1)
  }

}
