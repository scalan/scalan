package scalan.util

import java.io.File
import java.lang.reflect.Method

import scala.language.reflectiveCalls
import scalan.common.{SegmentsDsl, SegmentsDslExp}
import scalan._

class ExceptionTests extends BaseTests { suite =>
  val prefix = new File("test-out/scalan/util/exceptions/")

  trait ThrowableExamples extends ScalanDsl {
    val prefix = suite.prefix
    val subfolder = "/throwables"
    lazy val tElem = element[Throwable]
    lazy val defaultRep = tElem.defaultRepValue

    lazy val t1 = fun { (t: Rep[Throwable]) => t.getMessage }
  }

  test("throwablesStaged") {
    val ctx = new TestContext with  ThrowableExamples {
      def test() = {
        //assert(!isInlineThunksOnForce, "precondition for tests")
        {
//TODO make this work (recognizer should deal with BaseElemEx)
//          val Def(Lambda(_, _, x, SThrowableMethods.getMessage(obj))) = t1
//          assert(x == obj)
        }

      }
    }
    ctx.test
    ctx.emit("defaultRep", ctx.defaultRep)
    ctx.emit("t1", ctx.t1)
  }

  test("throwablesSeq") {
    val ctx = new ScalanCtxSeq with  ThrowableExamples {
      def test() = {
        //assert(!isInlineThunksOnForce, "precondition for tests")

      }
    }
    ctx.test
    val d = ctx.defaultRep
    val res = ctx.t1(new Throwable("test"))
    assertResult("test")(res)
  }
}
