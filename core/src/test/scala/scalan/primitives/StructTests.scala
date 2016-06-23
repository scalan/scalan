package scalan.primitives

import scala.language.reflectiveCalls
import scalan._
import scalan.common._
import scalan.compilation.{DummyCompiler, StructsCompiler}
import scalan.it.BaseItTests

trait StructExamples extends Scalan with SegmentsDsl with MetaTestsDsl {
  def eInt = IntElement

  lazy val t1 = fun({ (in: Rep[Int]) =>
    struct("in" -> in)
  })(Lazy(element[Int]), structElement(Seq("in" -> eInt)))

  lazy val singleFieldStructIn = fun({ (in: Rep[Struct]) =>
    in.get[Int]("in")
  })(Lazy(structElement(Seq("in" -> eInt))), eInt)

  lazy val manyFieldsStructIn = fun({ (in: Rep[Struct]) =>
    in.get[Int]("in1") - in.get[Int]("in2")
  })(Lazy(structElement(Seq("in1" -> eInt, "in2" -> eInt))), eInt)

  lazy val structInOut = fun({ (in: Rep[Struct]) =>
    val outVal = in.get[Int]("in") - 1
    struct("out" -> outVal)
  })(Lazy(structElement(Seq("in" -> eInt))), structElement(Seq("out" -> eInt)))

  lazy val crossFields = fun({ (in: Rep[Struct]) =>
    val in1 = in.get[Int]("in1")
    val in2 = in.get[Int]("in2")
    struct("out1" -> in2, "out2" -> in1)
  })(Lazy(structElement(Seq("in1" -> eInt, "in2" -> eInt))), structElement(Seq("out1" -> eInt, "out2" -> eInt)))

  lazy val structInside = fun { (n: Rep[Int]) =>
    // This should ensure we get structs left inside
    // If rewritings are added which optimize this to `if (n > 0) then n else n + 1`, change this!
    val s1 = struct("a" -> n, "b" -> n)
    val s2 = struct("a" -> (n + 1), "b" -> (n + 2))
    val s3 = IF (n > 0) THEN s1 ELSE s2
    s3.get[Int]("a") + s3.get[Int]("b")
  }

  lazy val t2 = fun({ (in: Rep[Int]) =>
    struct("in" -> in).get[Int]("in")
  })

  lazy val t3 = fun({ (in: Rep[Int]) =>
    val b = in + toRep(1)
    val c = in + in
    fields(struct("a" -> in, "b" -> b, "c" -> c), Seq("a", "c"))
  })(Lazy(element[Int]), structElement(Seq("a" -> eInt, "c" -> eInt)))

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
  lazy val t9f = fun { (in: Rep[((Int,Int),Int)]) =>
    val Pair(Pair(x, y), z) = in
    Pair(x + y, Pair(x - z, y - z))
  }
  lazy val t9 = structWrapper(t9f)
  lazy val t10 = structWrapper(fun { (in: Rep[((Int,Int),Int)]) =>
    val Pair(Pair(x, y), z) = in
    val i = Interval(x,y)
    Pair(i.length, i.shift(z).toData)
  })
  lazy val t11 = structWrapper(fun { (in: Rep[(Array[(Int,Int)],Int)]) =>
    val Pair(segs, z) = in
    Pair(segs, segs.length + z)
  })
  lazy val t12 = structWrapper(fun { (in: Rep[(Array[(Int,Int)],Int)]) =>
    val Pair(segs, z) = in
    val sums = segs.map(p => p._1 + p._2)
    Pair(sums, sums.length)
  })
  lazy val t13 = structWrapper(fun { (in: Rep[(Array[(Int,Int)],Int)]) =>
    val Pair(segs, z) = in
    val intervals = segs.map(Interval(_))
    Pair(intervals.map(_.length), intervals.length)
  })
  lazy val t14 = fun { (in: Rep[Int]) =>
    Pair(in, in)
  }
  lazy val t15 = structWrapper(fun { (in: Rep[(Int,Int)]) =>
    val Pair(x, y) = in
    IF (x > y) { Pair(x,y) } ELSE { Pair(y,x) }
  })
  lazy val t16 = structWrapper(fun { (in: Rep[((Array[(Int,Int)],Array[((Int,Int), Boolean)]),Int)]) =>
    val Pair(Pair(segs1, segs2), z) = in
    val intervals1 = segs1.map(Interval(_))
    val intervals2 = segs2.map(t => IF (t._2) { Interval(t._1).asRep[Segment] } ELSE { Slice(t._1).asRep[Segment]})
    Pair(intervals1.map(_.length), intervals2.map(_.length))
  })
}

class StructTests extends BaseViewTests {

  class Ctx extends TestCompilerContext {
    class ScalanCake extends ScalanDslExp with StructExamples with SegmentsDslExp with MetaTestsDslExp {

      def containsTuples(g: PGraph): Boolean = {
        g.scheduleAll.exists(tp => tp.rhs match {
          case First(_) => true
          case Second(_) => true
          case Tup(_,_) => true
          case _ => false
        })
      }

      def testFlattening[T](e: Elem[T], expected: Elem[_]) = {
        val iso = getFlatteningIso(e)
        val eFrom = iso.eFrom
        assertResult(expected)(eFrom)
        iso
      }

      // several elems which should have the flat result
      val es1 = tupleStructElement(element[Int],
        tupleStructElement(element[Char], tupleStructElement(element[Double], element[Boolean])))

      val es1again = tupleStructElement(element[Int],
        tupleStructElement(element[Char], tupleStructElement(element[Double], element[Boolean])))

      val es2 = tupleStructElement(tupleStructElement(element[Int], element[Char]),
        tupleStructElement(element[Double], element[Boolean]))

      val es3 = tupleStructElement(tupleStructElement(element[Int], element[Char], element[Double]),
        element[Boolean])
    }
    override val compiler = new DummyCompiler(new ScalanCake)
                           with StructsCompiler[ScalanDslExp with StructExamples]
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
    // TODO this inconsistency can potentially lead to some problems
    // and should be fixed with better implementation of StructElem.tag
    assert(t1 == t3, "should be equal as well even though e1 != e3 !!!")

    assert(es1 == es1again)
    assert(es1 != es2)
    assert(es1 != es3)
    assert(es2 != es3)
  }

  test("Structs as generic parameters for entities") {
    val ctx = new Ctx
    import ctx.compiler.scalan._

    val se1 = structElement(Seq("a" -> eInt))
    val se2 = structElement(Seq("a" -> eInt))
    val se3 = structElement(Seq("b" -> eInt))

    val me1 = metaTestElement(se1)
    val me2 = metaTestElement(se2)
    val me3 = metaTestElement(se3)

    assert(me1 == me2)
    assert(me1 != me3)

    val ae1 = arrayElement(se1)
    val ae2 = arrayElement(se2)
    val ae3 = arrayElement(se3)

    assert(ae1 == ae2)
    assert(ae1 != ae3)
  }

  test("Structs as type parameters are included in names") {
    val ctx = new Ctx
    import ctx.compiler.scalan._
    val structElem = structElement(Seq("a" -> eInt))

    val arrayOfStructsElem = arrayElement(structElem)
    assert(arrayOfStructsElem.name.contains(structElem.name))

    val arrayOfArraysOfStructsElem = arrayElement(arrayOfStructsElem)
    assert(arrayOfArraysOfStructsElem.name.contains(structElem.name))
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
      def testWrapper[A,B](functionName: String,
                             f: => Exp[A => B], expectTuples: Boolean = false): compiler.CompilerOutput[A, B] = {
        val out = super.test(functionName, f)
        val hasTuples = containsTuples(out.common.graph)
        assert(expectTuples == hasTuples)
        out
      }
    }
    import ctx.compiler.scalan._
    ctx.testWrapper("t6", t6)
    ctx.testWrapper("t7", t7)
    ctx.testWrapper("t8", t8)
    ctx.emit("t9f", t9f)
    ctx.testWrapper("t9", t9)
    ctx.testWrapper("t10", t10)
    ctx.testWrapper("t11", t11)
    ctx.testWrapper("t12", t12)
    ctx.testWrapper("t13", t13)
    ctx.testWrapper("t14", t14, true)
    ctx.testWrapper("t14_in", structWrapperIn(t14), true)
    ctx.testWrapper("t14_inout", structWrapper(t14), false)
    ctx.testWrapper("t15", t15)
    ctx.testWrapper("t16", t16)
  }

  test("Rewrite rule for SameStructAs works") {
    val ctx = new Ctx

    import ctx.compiler.scalan._

    val eStruct1 = structElement(Seq("in1" -> eInt, "in2" -> eInt))
    val eStruct2 = structElement(Seq("in2" -> eInt, "in1" -> eInt))
    val eStruct3 = structElement(Seq("in1" -> eInt))

    val f1 = fun[Struct, Struct] {
      x => struct("in1" -> x.getUntyped("in1"), "in2" -> x.getUntyped("in2"))
    }(Lazy(eStruct1), eStruct1)

    val f2 = fun[Struct, Struct] {
      x => struct("in1" -> x.getUntyped("in2"), "in2" -> x.getUntyped("in1"))
    }(Lazy(eStruct1), eStruct1)

    val f3 = fun[Struct, Struct] {
      x => struct("in2" -> x.getUntyped("in2"), "in1" -> x.getUntyped("in1"))
    }(Lazy(eStruct1), eStruct2)

    val f4 = fun[Struct, Struct] {
      x => struct("in1" -> x.getUntyped("in1"))
    }(Lazy(eStruct1), eStruct3)

    f1 should matchPattern { case Def(IdentityLambda()) => }
    f2 shouldNot matchPattern { case Def(IdentityLambda()) => }
    f3 shouldNot matchPattern { case Def(IdentityLambda()) => }
    f4 shouldNot matchPattern { case Def(IdentityLambda()) => }
  }

  test("structWrapper_IfThenElse") {
    val ctx = new CtxForStructs with StructExamples {
    }
    import ctx._
    emit("t14", t14)
    emit("t15", t15)
  }

  test("flatteningIso") {
    val ctx = new Ctx
    import ctx.compiler.scalan._
    {
      val iso = testFlattening(tuple2StructElement[Int,Int], tuple2StructElement[Int,Int])
      assert(iso.isIdentity, "when flattening is not necessary should return identity iso")
    }
    {
      val iso = testFlattening(tupleStructElement(element[Int], tuple2StructElement[Double,Boolean]),
        tuple3StructElement[Int, Double, Boolean])
        ctx.test("t1_iso.to", iso.toFun)
        ctx.test("t1_iso.from", iso.fromFun)
    }
    {
      val iso = testFlattening(tupleStructElement(tuple2StructElement[Int,Char], tuple2StructElement[Double,Boolean]),
        tupleStructElement(element[Int], element[Char], element[Double], element[Boolean]))
        ctx.test("t2_iso.to", iso.toFun)
        ctx.test("t2_iso.from", iso.fromFun)
    }
    {
      val iso = testFlattening(tupleStructElement(element[Int], tupleStructElement(element[Char], tupleStructElement(element[Double], element[Boolean]))),
        tupleStructElement(element[Int], element[Char], element[Double], element[Boolean]))
        ctx.test("t3_iso.to", iso.toFun)
        ctx.test("t3_iso.from", iso.fromFun)
    }
    {
      val iso = testFlattening(tupleStructElement(tupleStructElement(element[Short],element[Int]), tupleStructElement(element[Char], tupleStructElement(element[Double], element[Boolean]))),
        tupleStructElement(element[Short], element[Int], element[Char], element[Double], element[Boolean]))
        ctx.test("t4_iso.to", iso.toFun)
        ctx.test("t4_iso.from", iso.fromFun)
    }
    // arrays
    {
      val iso = testFlattening(arrayElement(tupleStructElement(element[Int], tuple2StructElement[Double,Boolean])),
        arrayElement(tupleStructElement(element[Int], element[Double], element[Boolean])))
      ctx.test("a1_iso.to", iso.toFun)
      ctx.test("a1_iso.from", iso.fromFun)
    }
    {
      val iso = testFlattening(arrayElement(tuple2StructElement(tuple2StructElement[Int,Char], tuple2StructElement[Double,Boolean])),
        arrayElement(tupleStructElement(element[Int], element[Char], element[Double], element[Boolean])))
      ctx.test("a2_iso.to", iso.toFun)
      ctx.test("a2_iso.from", iso.fromFun)
    }
    {
      val iso = testFlattening(arrayElement(tupleStructElement(element[Int], tupleStructElement(element[Char], tupleStructElement(element[Double], element[Boolean])))),
        arrayElement(tupleStructElement(element[Int], element[Char], element[Double], element[Boolean])))
      ctx.test("a3_iso.to", iso.toFun)
      ctx.test("a3_iso.from", iso.fromFun)
    }
    {
      val iso = testFlattening(arrayElement(tupleStructElement(tupleStructElement(element[Short],element[Int]), tupleStructElement(element[Char], tupleStructElement(element[Double], element[Boolean])))),
        arrayElement(tupleStructElement(element[Short], element[Int], element[Char], element[Double], element[Boolean])))
      ctx.test("a4_iso.to", iso.toFun)
      ctx.test("a4_iso.from", iso.fromFun)
    }
  }

  test("structIso") {
    val ctx = new Ctx
    import ctx.compiler.scalan._
    {
      val eFrom = tupleStructElement(element[(Int,Int)], element[Double], element[Boolean])
      val eTo = structElement(Seq("a" -> element[Interval], "b" -> element[Double], "c" -> element[Boolean]))
      val iso = new StructIso[Struct, Struct](
          eFrom, eTo,
          Seq[Iso[_, _]](getIsoByElem(element[Interval]), identityIso[Double], identityIso[Boolean]))

      ctx.test("t5_iso.to", iso.toFun)
      ctx.test("t5_iso.from", iso.fromFun)
    }
  }

  test("Elem.toStructElem") {
    val ctx = new Ctx
    import ctx.compiler.scalan._
    {
      val eIntInt = tuple2StructElement[Int, Int]
      val eDoubleBool = tuple2StructElement[Double, Boolean]
      val nested = tupleStructElement(eIntInt, eDoubleBool)
      val nested2 = tupleStructElement(nested, nested)
      assertResult(nested)(element[((Int,Int),(Double,Boolean))].toStructElemShallow)
      assertResult(nested2)(element[(((Int,Int),(Double,Boolean)),((Int,Int),(Double,Boolean)))].toStructElemShallow)
    }
  }

  // TODO switch to nested tests
  test("More flattening") {
    val ctx = new Ctx
    import ctx.compiler.scalan._

    val fis = Seq(es1, es1again, es2, es3).map(flatteningIso)
    val Seq(fi1, fi1again, fi2, fi3) = fis

    assert(fi1 == fi1again)
    assert(fi1 != fi2)
    assert(fi1 != fi3)

    val expectedFlat = tupleStructElement(element[Int], element[Char], element[Double], element[Boolean])

    val flatElems = fis.map(_.eFrom)

    all(flatElems) should equal(expectedFlat)
  }

  test("mergeIso") {
    val ctx = new Ctx
    import ctx.compiler.scalan._

    val nested1 = structElement(Seq("a" -> element[Int], "b" -> element[Int]))
    val nested2 = structElement(Seq("c" -> element[Int], "d" -> element[Int]))
    val se = structElement(Seq("_1" -> nested1, "_2" -> nested2))
    val iso = getStructMergeIso(se)
    ctx.test("iso.to", iso.toFun)
    ctx.test("iso.from", iso.fromFun)
  }
}

abstract class StructItTests extends BaseItTests[StructExamples](new ScalanDslStd with SegmentsDslStd with MetaTestsDslStd with StructExamples) {
  import progStd._

  test("struct out") {
    compareOutputWithStd(s => s.t1.asInstanceOf[s.Rep[Int => Struct]])(100)
  }

  test("struct with one field in") {
    compareOutputWithStd(s => s.singleFieldStructIn.asInstanceOf[s.Rep[Struct => Int]])(struct("in" -> 100))
  }

  test("struct with many fields in") {
    compareOutputWithStd(s => s.manyFieldsStructIn.asInstanceOf[s.Rep[Struct => Int]])(struct("in1" -> 200, "in2" -> 50))
  }

  test("struct in and out") {
    compareOutputWithStd(s => s.structInOut.asInstanceOf[s.Rep[Struct => Struct]])(struct("in" -> 100))
  }

  test("struct with many fields in and out") {
    compareOutputWithStd(s => s.crossFields.asInstanceOf[s.Rep[Struct => Struct]])(struct("in1" -> 200, "in2" -> 50))
  }

  test("struct used inside") {
    compareOutputWithStd(_.structInside)(100)
  }
}
