package scalan.util

import java.io.File
import java.lang.reflect.Method

import scala.language.reflectiveCalls
import scalan.common.{SegmentsDsl, SegmentsDslExp}
import scalan._

class ExceptionTests extends BaseTests { suite =>
  trait ThrowableExamples extends ScalanDsl {
    lazy val tElem = element[Throwable]
    lazy val defaultRep = tElem.defaultRepValue

    lazy val t1 = fun { (t: Rep[SThrowable]) => t.getMessage }
    lazy val t2 = fun { (t: Rep[SThrowable]) => t.getMessage }
    lazy val t3 = fun { (t: Rep[SThrowable]) => SException(t.wrappedValueOfBaseType)}
    lazy val t4 = fun { (t: Rep[SThrowable]) => SException(t.wrappedValueOfBaseType).wrappedValueOfBaseType}
    lazy val t5 = fun { (t: Rep[SThrowable]) => SThrowableImpl(t.wrappedValueOfBaseType)}
    lazy val t6 = fun { (t: Rep[SThrowable]) => SThrowableImpl(t.wrappedValueOfBaseType).wrappedValueOfBaseType}

    lazy val t7 = fun { (msg: Rep[String]) => SThrowable(msg)}
    lazy val t8 = fun { (msg: Rep[String]) => SThrowable(msg).getMessage }


  }

  test("throwablesStaged") {
    val ctx = new TestContext(this, "throwablesStaged") with  ThrowableExamples {
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
    ctx.emit("t2", ctx.t2)
    ctx.emit("t3", ctx.t3)
    ctx.emit("t4", ctx.t4)
    ctx.emit("t5", ctx.t5)
    ctx.emit("t6", ctx.t6)
  }

  test("createThrowableStaged") {
    val ctx = new TestContext(this, "createThrowableStaged") with  ThrowableExamples {
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
    ctx.emit("t7", ctx.t7)
    ctx.emit("t8", ctx.t8)
  }

  test("throwablesSeq") {
    val ctx = new ScalanCtxSeq with  ThrowableExamples {
      def test() = {
        //assert(!isInlineThunksOnForce, "precondition for tests")

      }
    }
    ctx.test
    val d = ctx.defaultRep
    val res = ctx.t1(ctx.SThrowable("test"))
    assertResult("test")(res)
  }
}
