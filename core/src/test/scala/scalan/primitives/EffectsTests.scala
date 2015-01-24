package scalan.primitives

import java.io.File
import java.lang.reflect.Method

import scala.language.reflectiveCalls
import scalan._
import scalan.common.{SegmentsDsl, SegmentsDslExp}

class EffectsTests extends BaseTests { suite =>
  val prefix = new File("test-out/scalan/primitives/effects/")

  trait ConsoleDsl extends Scalan {
    def print(s: Rep[String]): Rep[Unit]
    def read: Rep[String]
  }

  trait MyProg extends Scalan with ConsoleDsl {
    val prefix = suite.prefix
    val subfolder = ""

    lazy val t1 = fun { (in: Rep[String]) =>
      Thunk {
        print(in)
      }
    }

  }

  test("simpleEffectsStaged") {
    val ctx = new TestContext with  MyProg  with ConsoleDsl {

      def print(s: Rep[String]): Rep[Unit] =
        reflectEffect(Print(s))
      def read: Rep[String] =
        reflectEffect(Read())

      case class Print(s: Rep[String]) extends BaseDef[Unit]  {
        override def uniqueOpId = name(selfType)
        override def mirror(t: Transformer) = Print(t(s))
      }

      case class Read() extends BaseDef[String]  {
        override def uniqueOpId = name(selfType)
        override def mirror(t: Transformer) = Read()
      }

      def test() = {
      }
    }
    ctx.test
    ctx.emit("t1", ctx.t1)
  }

  trait MyDomainProg extends Scalan with SegmentsDsl {
    val prefix = suite.prefix
    val subfolder = "/isolifting"

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
    ctx.test
   // ctx.emit("t1", ctx.t1)
  }

  test("throwablesSeq") {
    val ctx = new ScalanCtxSeq with  MyProg {
      def print(s: Rep[String]): Rep[Unit] = print(s)
      def read: Rep[String] = Console.readLine()

      def test() = {
        //assert(!isInlineThunksOnForce, "precondition for tests")

      }
    }
    ctx.test
//    val res = ctx.t1(new Throwable("test"))
//    assertResult("test")(res)
  }

}
