package tests.scalan.primitives

import tests.BaseTests

import scalan.{ScalanCtxStaged, ScalanCtxSeq}
import scalan.codegen.GraphVizExport
import scalan.monads.{MonadsDslExp, MonadsDslSeq, ApplicativeExp, ApplicativeSeq}
import scala.language.reflectiveCalls

/**
 * User: Alexander Slesarenko   
 * Date: 8/3/14
 */
class FreeTests extends BaseTests {
  val prefix = "test-out/scalan/primitives/"

  test("testSeq") {
    val ctx = new ApplicativeSeq {}
    ctx.runApp
  }

  test("testStaged") {
    pending
    val ctx = new ApplicativeExp {}
    ctx.runApp
  }

  test("monadsSeq") {
    val ctx = new ScalanCtxSeq with MonadsDslSeq {
      def computation = {
        for (i <- IdMonad.point(1)) yield i
      }
    }
    (ctx.computation.run) should  be(1)
  }

  test("monadsStaged") {
    val ctx = new ScalanCtxStaged with MonadsDslExp with GraphVizExport {
      invokeEnabled = true
      def computation = {
        val res = for {
          (i: Rep[Int]) <- IdMonad.point(toRep(1))
          j <- IdMonad.point(i + 1)
        } yield Pair(i, j)
        res.run
      }
    }
    ctx.emitDepGraph(ctx.computation, s"${prefix}IdMonad.dot", false)
  }


  test("stateStaged") {
    val ctx = new ScalanCtxStaged with MonadsDslExp with GraphVizExport {
      invokeEnabled = true
      val S = proxyStateMonadCompanion(StateMonad);
      import S._

      def computation = fun { (x: Rep[Int]) =>
        val res = for {
          s  <- get[Int]
          _  <- modify[Int](s => s + 1)
          s2 <- get[Int]
          _  <- put(s2 + s)
        } yield x + s + s2
        res.runFrom(x)
      }
    }
    ctx.emitDepGraph(ctx.computation, s"${prefix}StateMonad.dot", false)
  }


  test("ioStaged") {
    val ctx = new ScalanCtxStaged with MonadsDslExp with GraphVizExport {
      invokeEnabled = true
      val IO = proxyIOMonadCompanion(IOMonad);
      import IO._

      def computation = fun { (s: Rep[String]) =>
        val res = for {
          _  <- println(s)
          _  <- println(s)
        } yield ()
        res.runIO
      }
    }
    ctx.emitDepGraph(ctx.computation, s"${prefix}IOMonad.dot", false)
  }

}
