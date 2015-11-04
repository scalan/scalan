package scalan.primitives

import java.lang.reflect.Method

import scala.language.reflectiveCalls
import scalan._
import scalan.common.{SegmentsDsl, SegmentsDslExp, Lazy}

class StructTests extends BaseCtxTests {
  trait MyProg extends Scalan with SegmentsDsl {
    val eInt = IntElement.asElem[Any]
    lazy val t1 = fun({ (in: Rep[Int]) =>
      struct("in" -> in).asRep[Any]
    })(Lazy(element[Int]), structElement(Seq("in" -> eInt)).asElem[Any])

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

  test("StructElem equality") {
    val ctx = new TestContext with MyProg with SegmentsDslExp {
    }
    import ctx._
    val e1 = structElement(Seq("a" -> eInt))
    val e2 = structElement(Seq("a" -> eInt))
    val e3 = structElement(Seq("b" -> eInt))
    assert(e1 == e2, "should be equal")
    assert(e1 != e3, "should not be equal")
    val t1 = e1.tag
    val t2 = e2.tag
    val t3 = e3.tag
    assert(t1 == t2, "should be equal")
    assert(t1 != t3, "should not be equal")
  }

  test("StructElem as result type") {
    val ctx = new TestContext with MyProg with SegmentsDslExp {
      def test() = {
//        {
//          val Def(Lambda(_, _, x, Def(th@ThunkDef(res, sch)))) = t1
//          assert(x == res && sch.isEmpty && th.freeVars.isEmpty)
//        }
      }
    }
    ctx.test
    val t1 = ctx.t1
    ctx.emit("t1", t1)
  }
}
