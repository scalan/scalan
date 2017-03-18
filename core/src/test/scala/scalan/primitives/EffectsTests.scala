package scalan.primitives

import scala.language.reflectiveCalls
import scalan._
import scalan.common.{SegmentsDsl, SegmentsDslExp}
import scalan.compilation.DummyCompiler

class EffectsTests extends BaseCtxTests {
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

  class Ctx(testName: String) extends TestCompilerContext(testName) {
    override val compiler = new DummyCompiler(new ScalanDslExp with MyProg) with EffectfulCompiler[ScalanDslExp with MyProg]
  }

  test("simpleEffectsStaged") {
    val ctx = new Ctx("simpleEffectsStaged") {
      import compiler.scalan._
      test("t1", t1)
      test("t2", t2)
    }
  }

  test("nestedThunksStaged") {
    val ctx = new Ctx("nestedThunksStaged") {
      import compiler.scalan._
      test("t3", t3)
    }
  }

  test("IfThenElseWithEffectsSimple") {
    val ctx = new Ctx("IfThenElseWithEffectsSimple") {
      import compiler.scalan._

      def test() = {
        val Def(lam : Lambda[_,_]) = t4
        val b = lam.branches
        assert(true)
      }

      test()

      test("t4", t4)
    }
  }

  trait MyDomainProg extends Scalan with SegmentsDsl {
//    lazy val t1 = fun { (in: Rep[Int]) =>
//      Thunk { Interval(in, in) }.force.length
//    }

  }

  test("simpleEffectsWithIsoLiftingStaged") {
    val ctx = new TestContext with SegmentsDslExp with MyDomainProg {
      isInlineThunksOnForce = false

      def test() = {
//        assert(!isInlineThunksOnForce, ": precondition for tests");
//        {
//          val Def(Lambda(_, _, x, Def(ApplyBinOp(op, _, _)))) = t1
//          assert(op.isInstanceOf[NumericMinus[_]])
//        }

      }
    }
    ctx.test()
   // ctx.test("t1", ctx.t1)
  }

  test("throwablesSeq") {
    val ctx = new ScalanDslExp with MyProg {
      def test() = {
        //assert(!isInlineThunksOnForce, "precondition for tests")
      }
    }
    ctx.test()
//    val res = ctx.t1(new Throwable("test"))
//    assertResult("test")(res)
  }

}
