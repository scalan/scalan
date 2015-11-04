package scalan.primitives

import java.lang.reflect.Method

import scala.language.reflectiveCalls
import scalan._
import scalan.common.{SegmentsDsl, SegmentsDslExp, Lazy}

class StructTests extends BaseCtxTests {
  trait MyProg extends Scalan  {
    val eInt = IntElement.asElem[Any]
    lazy val t1 = fun({ (in: Rep[Int]) =>
      struct("in" -> in).asRep[Any]
    })(Lazy(element[Int]), structElement(Seq("in" -> eInt)).asElem[Any])

    lazy val t2 = fun({ (in: Rep[Int]) =>
      field(struct("in" -> in).asRep[Any], "in").asRep[Int]
    })

    lazy val t3 = fun({ (in: Rep[Int]) =>
      val b = in + toRep(1)
      val c = in + in
      fields(struct("a" -> in, "b" -> b, "c" -> c).asRep[Any], Seq("a", "c")).asRep[Any]
    })(Lazy(element[Int]), structElement(Seq("a" -> eInt, "c" -> eInt)).asElem[Any])
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
    // TODO this inconsistensy can potentially lead to some problems
    // and should be fixed with better implementation of StructElem.tag
    assert(t1 == t3, "should be equal as well even though e1 != e3 !!!")
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
    ctx.emit("t1", ctx.t1)
    ctx.emit("t2", ctx.t2)
  }

  test("ProjectionStruct") {
    val ctx = new TestContext with MyProg with SegmentsDslExp {
      def test() = {
      }
    }
    ctx.test
    ctx.emit("t3", ctx.t3)
  }
}
