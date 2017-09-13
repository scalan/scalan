package scalan.util

import java.io.File
import java.lang.reflect.Method

import scala.language.reflectiveCalls
import scalan._

class ExceptionTests extends BaseCtxTests {
  trait ThrowableExamples extends ScalanDsl {
    lazy val tElem = element[Throwable]

    lazy val t1 = fun { (t: Rep[SThrowable]) => t.getMessage }
    lazy val t2 = fun { (t: Rep[SThrowable]) => t.getMessage }

    lazy val t3 = fun { (t: Rep[SThrowable]) => SThrowableImpl(t.wrappedValue)}
    lazy val t4 = fun { (t: Rep[SThrowable]) => SThrowableImpl(t.wrappedValue).wrappedValue}

    lazy val t5 = fun { (msg: Rep[String]) => SThrowable(msg)}
    lazy val t6 = fun { (msg: Rep[String]) => SThrowable(msg).getMessage }


  }

  test("throwablesStaged") {
    val ctx = new TestContext with ThrowableExamples {
      def test() = {
        //assert(!isInlineThunksOnForce, "precondition for tests")
        {
//TODO make this work (recognizer should deal with BaseTypeElem)
//          val Def(Lambda(_, _, x, SThrowableMethods.getMessage(obj))) = t1
//          assert(x == obj)
        }
      }
    }
    ctx.test()
    ctx.emit("t1", ctx.t1)
    ctx.emit("t2", ctx.t2)
    ctx.emit("t3", ctx.t3)
    ctx.emit("t4", ctx.t4)
  }

  test("createThrowableStaged") {
    val ctx = new TestContext with ThrowableExamples {
      def test() = {
        //assert(!isInlineThunksOnForce, "precondition for tests")
        {
//TODO make this work (recognizer should deal with BaseTypeElem)
//          val Def(Lambda(_, _, x, SThrowableMethods.getMessage(obj))) = t1
//          assert(x == obj)
        }

      }
    }
    ctx.test
    ctx.emit("t5", ctx.t5)
    ctx.emit("t6", ctx.t6)
  }

//  test("throwablesSeq") {
//    val ctx = new ScalanDslExp with ThrowableExamples {
//      def test() = {
//        //assert(!isInlineThunksOnForce, "precondition for tests")
//
//      }
//    }
//    ctx.test
//    val res = ctx.compile(ctx.t1)(ctx.SThrowable("test"))
//    assertResult("test")(res)
//  }
}
