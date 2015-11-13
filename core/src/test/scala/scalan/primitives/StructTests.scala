package scalan.primitives

import java.lang.reflect.Method

import scala.language.reflectiveCalls
import scalan._
import scalan.common.{SegmentsDsl, SegmentsDslExp, Lazy}
import scalan.compilation.DummyCompiler

class StructTests extends BaseCtxTests {
  trait MyProg extends Scalan with SegmentsDsl {
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

    lazy val t4 = fun({ (in: Rep[Int]) =>
      Pair(in, in)
    })

    lazy val t5 = fun({ (in: Rep[Int]) =>
      Pair(in, Pair(in, in + 1))
    })


    lazy val t6 = structWrapper(fun { (in: Rep[(Int,Int)]) => Interval(in).shift(10).toData })

    lazy val t2x2_7 = fun { (in: Rep[(Int,Int)]) =>
      Pair(in._1 + in._2, in._1 - in._2)
    }
    lazy val t7 = structWrapper(t2x2_7)

    lazy val t8 = structWrapper(fun { (in: Rep[(Int,Int)]) =>
      Pair(in._2, in._1)
    })
    lazy val t9 = structWrapper(fun { (in: Rep[((Int,Int),Int)]) =>
      val Pair(Pair(x, y), z) = in
      Pair(x + y, Pair(x - z, y - z))
    })
    lazy val t10 = structWrapper(fun { (in: Rep[((Int,Int),Int)]) =>
      val Pair(Pair(x, y), z) = in
      val i = Interval(x,y)
      Pair(i.length, i.shift(z).toData)
    })
    lazy val t11 = structWrapper(fun { (in: Rep[(Array[(Int,Int)],Int)]) =>
      val Pair(segs, z) = in
      val intervals = segs.map(Interval(_))
      Pair(intervals.map(_.length), intervals.length)
    })
  }

  class Ctx extends TestCompilerContext {
    class ScalanCake extends ScalanCtxExp with MyProg with SegmentsDslExp {
      def noTuples[A,B](f: Rep[A=>B]): Boolean = {
        val g = new PGraph(f)
        !g.scheduleAll.exists(tp => tp.rhs match {
          case First(_) => true
          case Second(_) => true
          case Tup(_,_) => true
          case _ => false
        })
      }

      def testFlattening[T](e: StructElem[T], expectedStructFields: Seq[Elem[_]]) = {
        val iso = flatteningIso(e)
        val eFrom = iso.eFrom.asInstanceOf[StructElem[_]]
        assertResult(expectedStructFields)(eFrom.fields.map(_._2))
        iso
      }
    }
    override val compiler = new DummyCompiler(new ScalanCake)
                           with StructsCompiler[ScalanCtxExp with MyProg]
  }

  test("StructElem equality") {
    val ctx = new Ctx
    import ctx.compiler.scalan._
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
    val ctx = new Ctx {
      def test() = {
//        {
//          val Def(Lambda(_, _, x, Def(th@ThunkDef(res, sch)))) = t1
//          assert(x == res && sch.isEmpty && th.freeVars.isEmpty)
//        }
      }
    }
    import ctx.compiler.scalan._
    ctx.test
    ctx.test("t1", t1)
    ctx.test("t2", t2)
  }

  test("ProjectionStruct") {
    val ctx = new Ctx {
      def test() = {
      }
    }
    import ctx.compiler.scalan._
    ctx.test
    ctx.test("t3", t3)
  }

  test("StructsRewriting") {
    val ctx = new Ctx {
      def test() = {
      }
    }
    import ctx.compiler.scalan._
    ctx.test
    ctx.test("t4", t4)
    ctx.test("t5", t5)
  }

  test("structWrapper") {
    val ctx = new Ctx {
      import compiler.scalan._
      override def test[A,B](functionName: String, f: Exp[A => B]): compiler.CompilerOutput[A, B] = {
        val out = super.test(functionName, f)
        assert(noTuples(out.common.graph.roots(0).asRep[A => B]))
        out
      }
    }
    import ctx.compiler.scalan._
    ctx.test("t6", t6)
    ctx.test("t7", t7)
    ctx.test("t8", t8)
    ctx.test("t9", t9)
    ctx.test("t10", t10)
//    ctx.test("t11", t11)
  }

  test("flatteningIso") {
    val ctx = new Ctx
    import ctx.compiler.scalan._
    {
      val iso = testFlattening(structElem2[Int,Int], Seq(element[Int], element[Int]))
      assert(iso.isIdentity, "when flattening is not necessary should return identity iso")
    }
    {
      val iso = testFlattening(structElem2(element[Int], structElem2[Double,Boolean]),
        Seq(element[Int], element[Double], element[Boolean]))
        ctx.test("t1_iso.to", iso.toFun)
        ctx.test("t1_iso.from", iso.fromFun)
    }
    {
      val iso = testFlattening(structElem2(structElem2[Int,Char], structElem2[Double,Boolean]),
        Seq(element[Int], element[Char], element[Double], element[Boolean]))
        ctx.test("t2_iso.to", iso.toFun)
        ctx.test("t2_iso.from", iso.fromFun)
    }
    {
      val iso = testFlattening(structElem2(element[Int], structElem2(element[Char], structElem2(element[Double], element[Boolean]))),
        Seq(element[Int], element[Char], element[Double], element[Boolean]))
        ctx.test("t3_iso.to", iso.toFun)
        ctx.test("t3_iso.from", iso.fromFun)
    }
    {
      val iso = testFlattening(structElem2(structElem2(element[Short],element[Int]), structElem2(element[Char], structElem2(element[Double], element[Boolean]))),
        Seq(element[Short], element[Int], element[Char], element[Double], element[Boolean]))
        ctx.test("t4_iso.to", iso.toFun)
        ctx.test("t4_iso.from", iso.fromFun)
    }
  }

  test("structIso") {
    val ctx = new Ctx
    import ctx.compiler.scalan._
    {
      val eFrom = structElement(Seq(element[(Int,Int)], element[Double], element[Boolean]))
      val eTo = structElement(Seq("a" -> element[Interval].asElem[Any], "b" -> element[Double].asElem[Any], "c" -> element[Boolean].asElem[Any]))
      val iso = new StructIso(
          eFrom, eTo,
          Seq(getIsoByElem(element[Interval]), identityIso[Double], identityIso[Boolean]))

      ctx.test("t5_iso.to", iso.toFun)
      ctx.test("t5_iso.from", iso.fromFun)
    }
  }

  test("Elem.toStructElem") {
    val ctx = new Ctx
    import ctx.compiler.scalan._
    {
      val eIntInt = structElement(Seq(element[Int], element[Int]))
      val eDoubleBool = structElement(Seq(element[Double], element[Boolean]))
      val nested = structElement(Seq(eIntInt, eDoubleBool))
      val nested2 = structElement(Seq(nested, nested))
      assertResult(nested)(element[((Int,Int),(Double,Boolean))].toStructElemShallow)
      assertResult(nested2)(element[(((Int,Int),(Double,Boolean)),((Int,Int),(Double,Boolean)))].toStructElemShallow)
    }
  }

}

