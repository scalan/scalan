package scalan.primitives

import java.lang.reflect.Method

import scala.language.reflectiveCalls
import scalan._
import scalan.common.{SegmentsDsl, SegmentsDslExp, Lazy}

class StructTests extends BaseCtxTests {
  trait MyProg extends Scalan with SegmentsDsl {
    val eInt = IntElement
    lazy val t1 = fun({ (in: Rep[Int]) =>
      struct("in" -> in)
    })(Lazy(eInt), structElement(Seq("in" -> eInt.asElem[Any])))

//    lazy val t2 = fun { (in: Rep[Int]) =>
//      Thunk { in + 1 }
//    }
//
//    lazy val t3 = fun { (in: Rep[Int]) =>
//      Thunk { in + in + 1 }
//    }
//
//    lazy val t4 = fun { (in: Rep[Int]) =>
//      Thunk { in + Thunk { in + 1 }.force }
//    }
//    lazy val t5 = fun { (in: Rep[Int]) =>
//      Thunk { in + 1 }.force + Thunk { in + 1 }.force
//    }
//    lazy val t6 = fun { (in: Rep[Int]) =>
//      Thunk { Thunk {in + 1}.force + 1 }.force + 1
//    }
//
//    def f7(x: => Rep[Int]) = Thunk { x }
//    lazy val t7 = fun { (in: Rep[Int]) =>
//      f7 {in + 1}
//    }
//    def f8(x: Rep[Int]) = Thunk { x }
//    lazy val t8 = fun { (in: Rep[Int]) =>
//      f8 {in + 1}
//    }
//
//    lazy val t9 = fun { (in: Rep[Int]) =>
//      Thunk { (in + 1) + Thunk { in + 1 }.force }
//    }
//
//    lazy val t10 = fun { (in: Rep[Int]) =>
//      Thunk { Thunk { Thunk { in + 1 }}}.force.force
//    }
//
//    def to(x: Rep[Int]): Rep[Int] = x * x
//    lazy val t11 = fun { (in: Rep[Int]) =>
//      val x = Thunk { in + 1 }
//      Thunk { to(x.force) }              // test for ThunkIso.to
//    }
  }

  test("thunksWithoutInlining") {
    val ctx = new TestContext with MyProg with SegmentsDslExp {
      def test() = {
//        {
//          val Def(Lambda(_, _, x, Def(th@ThunkDef(res, sch)))) = t1
//          assert(x == res && sch.isEmpty && th.freeVars.isEmpty)
//        }
      }
    }
    ctx.test
    ctx.emit("t1", ctx.t1)
  }
}
