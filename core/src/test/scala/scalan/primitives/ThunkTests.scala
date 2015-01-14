package scalan.primitives

import java.io.File
import java.lang.reflect.Method

import scalan.BaseTests

import scalan.{Scalan, ScalanCtxExp, ScalanCtxSeq}

class ThunkTests extends BaseTests {
  val prefix = new File("test-out/scalan/primitives/thunk/")

  trait MyProg extends Scalan {
    lazy val t1 = fun { (in: Rep[Int]) =>
      Thunk { in }
    }

    lazy val t2 = fun { (in: Rep[Int]) =>
      Thunk { in + 1 }
    }

    lazy val t3 = fun { (in: Rep[Int]) =>
      Thunk { in + in + 1 }
    }
  }
  test("thunkStaged") {
    val ctx = new ScalanCtxExp with  MyProg {
      override def isInvokeEnabled(d: Def[_], m: Method) = true
      def emit(name: String, ss: Exp[_]*) =
        emitDepGraph(ss.toList, new File(prefix, s"$name.dot"), false)

      def test = {
        {
          val Def(Lambda(_, _, x, Def(DefBlock(res, sch)))) = t1
          assert(x == res && sch.isEmpty)
        }
        {
          val Def(Lambda(_, _, x, Def(DefBlock(res, sch)))) = t2
          assert(sch.size == 2)
        }
      }
    }
    ctx.test
    ctx.emit("t1", ctx.t1)
    ctx.emit("t2", ctx.t2)
    ctx.emit("t3", ctx.t3)
    ctx.emit("t2t3", ctx.t2, ctx.t3) // see shared constant
  }
}
