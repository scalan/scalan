package scalan.staged

import scalan.common.Lazy
import scalan.compilation.{DummyCompiler, SlicingCompiler}
import scalan.{BaseTests, Scalan, TestContexts}

abstract class AbstractSlicingTests extends BaseTests with TestContexts {

  class Ctx extends TestContext with Slicing {
    def createSliceAnalyzer = new SliceAnalyzer

    val eInt = element[Int]
    val eString = element[String]
    val eDouble = element[Double]
    val eBoolean = element[Boolean]
    val eEmpty  = structElement(Seq())
    val eIn = structElement(Seq("a" -> eInt, "b" -> eString, "c" -> eDouble))
    val eNested = structElement(Seq("e" -> eInt, "f" -> eString))
    val eNested2 = structElement(Seq("f" -> eString, "g" -> eBoolean))
    val eNestedAll = structElement(Seq("e" -> eInt, "f" -> eString, "g" -> eBoolean))
    val eIn2 = structElement(Seq("b" -> eString, "c" -> eDouble, "d" -> eNested))
    val eSliced = structElement(Seq("a" -> eInt, "b" -> eString))
    val eJoined = structElement(Seq("a" -> eInt, "b" -> eString, "c" -> eDouble, "d" -> eNested))
    val eKV = structElement(Seq("key" -> eString, "val" -> eInt))

    val mEmpty     = EmptyMarking(eIn)
    val mAll       = AllMarking(eIn)
    val mNoFields  = StructMarking(Seq())(eIn)
    val mIn        = eIn.toMarking
    val mIn2       = eIn2.toMarking
    val mJoined    = eJoined.toMarking
    val mSlicedIn  = StructMarking(Seq("a" -> eIn("a").toMarking, "b" -> eIn("b").toMarking))(eIn)
//    val mArrAll    = ArrayMarking(KeyPath.All, mIn)
//    val mArrSome   = ArrayMarking(KeyPath.All, mSlicedIn)
//    val mArrNone   = ArrayMarking(KeyPath.All, mNoFields)
//    val mArrJoined = ArrayMarking(KeyPath.All, mJoined)

    lazy val funOneField = fun { in: Rep[Struct] => in.get[Int]("a") }(Lazy(eIn))
    lazy val funTwoFields = fun { in: Rep[Struct] =>
      Pair(in.get[Int]("a"), in.get[String]("b")) }(Lazy(eIn))
    lazy val funNestedFields = fun { in: Rep[Struct] =>
      in.getUnchecked[Struct]("d").get[Int]("e") }(Lazy(eJoined))
    lazy val funPlus = fun { in: Rep[Struct] =>
      in.get[Int]("a") + in.getUnchecked[Struct]("d").get[Int]("e") }(Lazy(eJoined))
    lazy val funMap = fun { in: Rep[Struct] =>
      Pair(
        in.get[Int]("a") + in.getUnchecked[Struct]("d").get[Int]("e"),
        in.get[Int]("a") * in.get[String]("b").toInt
      )
    }(Lazy(eJoined))

    lazy val funKey = fun { in: Rep[Struct] => in.get[String]("b") }(Lazy(eJoined))
    lazy val funReduce = fun {
      in: Rep[(Int, Struct)] => in._1 + in._2.getUnchecked[Struct]("d").get[Int]("e")
    }(Lazy(pairElement(IntElement, eJoined)))

    lazy val funPred = fun { in: Rep[Struct] =>
      in.get[Int]("a") < in.getUnchecked[Struct]("d").get[Int]("e")
    }(Lazy(eJoined))
  }

  class TestHelper(val ctx: Ctx) {
    val compiler = new DummyCompiler[ctx.type](ctx) with SlicingCompiler[ctx.type]
    import compiler._
    import compiler.scalan._

    def getFuncMarking[A,B](f: scalan.Exp[A => B], mInitial: SliceMarking[B]): FuncMarking[A,B] = {
      val slicingPassBuilder = SlicingPass.makePass(false)
      val g = new PGraph(f)
      val pass = slicingPassBuilder(g)
      val l = f.getLambda

      // this setup of starting marking for lambda result is important part of propagation process
      // it have to be setup for each analysis (here we set it up for slicing)
      sliceAnalyzer.updateOutboundMarking(l.y, mInitial)
      pass.backwardAnalyse(g)
      val res = sliceAnalyzer.getMark(f) match { case fm: FuncMarking[A,B]@unchecked => fm }
      res
    }

    def testFuncMark[A,B](f: scalan.Exp[A => B], mInitial: SliceMarking[B], mExpected: SliceMarking[A]) = {
      val fm = getFuncMarking(f, mInitial)
      val mA = fm.mDom
      assertResult(mExpected)(mA)
      assertResult(FuncMarking(mA,mInitial))(fm)
    }

    def testFuncSlice[A,B](name: String, f: scalan.Exp[A => B], mInitial: SliceMarking[B]) = {
      import scalan._
      val fm = getFuncMarking(f, mInitial)
      val fsliced = sliceFunc(f, fm)
      val Def(l: Lambda[_,_]) = fsliced
      ctx.emitF(name, () => f, () => fsliced)
      assertResult(mInitial.projectedElem)(l.eB)
      assert(l.scheduleAll.collectFirst { case TableEntry(s, _: Sliced[_,_]) => s }.isEmpty)
      typecheck(new PGraph(fsliced))
    }

    def typecheck(graph: PGraph): Unit = {
      for (te <- graph.scheduleAll) {
        te.rhs match {
//          case ArrayFilter(_xs, p: RFunc[a,Boolean]@unchecked) =>
//            val xs = _xs.asRep[Array[a]]
//            assert(xs.elem.asInstanceOf[ArrayElem[a]].eItem == p.elem.eDom)
          case _ =>
        }
      }
    }
  }
}

class SlicingTests extends AbstractSlicingTests {

  test("isEmpty") {
    val ctx = new Ctx
    import ctx._
    assert(mEmpty.isEmpty)
    assert(!mAll.isEmpty)
    assert(mNoFields.isEmpty)
    assert(!mIn.isEmpty)
    assert(!mSlicedIn.isEmpty)
//    assert(!mArrAll.isEmpty)
//    assert(!mArrSome.isEmpty)
//    assert(mArrNone.isEmpty)
  }

  test("isIdentity") {
    val ctx = new Ctx
    import ctx._
    assert(!mEmpty.isIdentity)
    assert(mAll.isIdentity)
    assert(!mNoFields.isIdentity)
    assert(mIn.isIdentity)
    assert(!mSlicedIn.isIdentity)
//    assert(mArrAll.isIdentity)
//    assert(!mArrSome.isIdentity)
//    assert(!mArrNone.isIdentity)
  }

  test("projectedElem") {
    val ctx = new Ctx
    import ctx._
    assertResult(eEmpty)(mEmpty.projectedElem)
    assertResult(eIn)(mAll.projectedElem)
    assertResult(eEmpty)(mNoFields.projectedElem)
    assertResult(eIn)(mIn.projectedElem)
    assertResult(eSliced)(mSlicedIn.projectedElem)
//    assertResult(arrayElement(eIn))(mArrAll.projectedElem)
//    assertResult(arrayElement(eSliced))(mArrSome.projectedElem)
//    assertResult(arrayElement(eEmpty))(mArrNone.projectedElem)
  }

  test("join") {
    val ctx = new Ctx
    import ctx._
    assertResult(mIn)(mIn.join(mSlicedIn))
    assertResult(mSlicedIn)(mSlicedIn.join(mSlicedIn))

    val m1 = StructMarking(eSliced.toStructMarking.fields)(eJoined)
    val m2 = StructMarking(eIn2.toStructMarking.fields)(eJoined)
    assertResult(mJoined)(m1.join(m2))

//    assertResult(mArrAll)(mArrSome.join(mArrAll))

//    val am1 = ArrayMarking(KeyPath.All, m1)
//    val am2 = ArrayMarking(KeyPath.All, m2)
//    assertResult(mArrJoined)(am1.join(am2))

    {
      val eJoined = structElement(Seq("a" -> eInt, "b" -> eString, "c" -> eNestedAll))
      val mLeft = StructMarking(Seq(
        "a" -> eInt.toMarking,
        "c" -> StructMarking(eNested.toStructMarking.fields)(eNestedAll)))(eJoined)
      val mRight = StructMarking(Seq(
        "b" -> eString.toMarking,
        "c" -> StructMarking(eNested2.toStructMarking.fields)(eNestedAll)))(eJoined)
//      val mArrJoined = ArrayMarking(eJoined.toMarking)

//      val am1 = ArrayMarking(mLeft)
//      val am2 = ArrayMarking(mRight)
//      val res = am1.join(am2)
//      assertResult(mArrJoined)(res)
    }

    assertResult(mAll)(mEmpty.join(mAll))
    assertResult(mAll)(mAll.join(mEmpty))
    assertResult(mNoFields)(mNoFields.join(mEmpty))
    assertResult(mNoFields)(mEmpty.join(mNoFields))
  }


  test("SliceAnalyzer") {
    val helper = new TestHelper(new Ctx)
    import helper.testFuncMark
    import helper.ctx._

    testFuncMark(funOneField, eInt.toMarking, StructMarking(Seq("a" -> AllMarking(eInt)))(eIn))
    testFuncMark(funTwoFields, element[(Int,String)].toMarking,
      StructMarking(
        Seq("a" -> AllMarking(eInt),
          "b" -> AllMarking(eString)))(eIn))
    testFuncMark(funNestedFields, eInt.toMarking,
      StructMarking(
        Seq("d" -> StructMarking(
          Seq("e" -> AllMarking(eInt))
        )(eNested)
        ))(eJoined))
    testFuncMark(funPlus, eInt.toMarking,
      StructMarking(
        Seq("a" -> AllMarking(eInt),
          "d" -> StructMarking(Seq(
            "e" -> AllMarking(eInt)))(eNested)))(eJoined))
    testFuncMark(funMap, PairMarking(eInt.toMarking, EmptyMarking(eInt)),
      StructMarking(
        Seq("a" -> AllMarking(eInt),
          "d" -> StructMarking(Seq(
            "e" -> AllMarking(eInt)))(eNested)))(eJoined))
    testFuncMark(funPred, eBoolean.toMarking,
      StructMarking(
        Seq("a" -> AllMarking(eInt),
          "d" -> StructMarking(Seq(
            "e" -> AllMarking(eInt)))(eNested)))(eJoined))
  }

  test("FuncSlicing") {
    val helper = new TestHelper(new Ctx)
    import helper.testFuncSlice
    import helper.ctx._

    testFuncSlice("1", funOneField, eInt.toMarking)
    testFuncSlice("2", funTwoFields, element[(Int,String)].toMarking)
    testFuncSlice("3", funNestedFields, eInt.toMarking)
    testFuncSlice("4", funPlus, eInt.toMarking)
    testFuncSlice("5", funMap, PairMarking(eInt.toMarking, EmptyMarking(eInt)))
    testFuncSlice("6", funPred, eBoolean.toMarking)
  }
}