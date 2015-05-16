package scalan.primitives

import java.io.File
import java.lang.reflect.Method

import scala.io.StdIn
import scala.language.reflectiveCalls
import scalan._
import scalan.common.{SegmentsDsl, SegmentsDslExp}

class EffectsTests extends BaseTests { suite =>
//  trait ConsoleDsl extends Scalan {
//    def print(s: Rep[String]): Rep[Unit]
//    def read: Rep[String]
//  }

  trait MyProg extends Scalan {
    lazy val t1 = fun { (in: Rep[String]) => Thunk {
        console_printlnE(in)
    }}
    lazy val t2 = fun { (in: Rep[String]) => Thunk {
      console_printlnE(in)
      console_printlnE(in + in)
    }}
    lazy val t3 = fun { (in: Rep[String]) => Thunk {
      Thunk { console_printlnE(in) }
      console_printlnE(in + in)
      console_printlnE(in + in)
      console_printlnE(in + in)
      console_printlnE(in + in)
    }}

    lazy val t4 = fun { (in: Rep[String]) =>
      IF (in.contains("abc")) THEN { console_printlnE(in) } ELSE { console_printlnE(in) }
    }
  }

  abstract class MyProgStaged(testName: String) extends TestCompilerContext(this, testName) with  MyProg with EffectfulCompiler {
  }

  test("simpleEffectsStaged") {
    val ctx = new MyProgStaged("simpleEffectsStaged") {
      def test() = { }
    }
    ctx.test
    ctx.test("t1", ctx.t1)
    ctx.test("t2", ctx.t2)
  }

  test("nestedThunksStaged") {
    val ctx = new MyProgStaged("nestedThunksStaged") {
      def test() = { }
    }
    ctx.test
    ctx.test("t3", ctx.t3)
  }

  test("IfThenElseWithEffectsSimple") {
    val ctx = new MyProgStaged("IfThenElseWithEffectsSimple") {
      def test() = {
        val Def(lam : Lambda[_,_]) = t4
        val b = lam.branches
        assert(true)

      }
    }
    ctx.test
    ctx.test("t4", ctx.t4)
  }
  trait MyDomainProg extends Scalan with SegmentsDsl {
//    lazy val t1 = fun { (in: Rep[Int]) =>
//      Thunk { Interval(in, in) }.force.length
//    }

  }

  test("simpleEffectsWithIsoLiftingStaged") {
    val ctx = new TestContext(this, "simpleEffectsWithIsoLiftingStaged") with SegmentsDslExp with MyDomainProg {
      isInlineThunksOnForce = false

      def test() = {
//        assert(!isInlineThunksOnForce, ": precondition for tests");
//        {
//          val Def(Lambda(_, _, x, Def(ApplyBinOp(op, _, _)))) = t1
//          assert(op.isInstanceOf[NumericMinus[_]])
//        }

      }
    }
    ctx.test
   // ctx.test("t1", ctx.t1)
  }

  test("throwablesSeq") {
    val ctx = new ScalanCtxSeq with  MyProg {
      def test() = {
        //assert(!isInlineThunksOnForce, "precondition for tests")
      }
    }
    ctx.test
//    val res = ctx.t1(new Throwable("test"))
//    assertResult("test")(res)
  }

}
