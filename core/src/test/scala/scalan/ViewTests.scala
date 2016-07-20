package scalan

import scala.language.reflectiveCalls
import scalan.common.{CommonExamples, SegmentsDslExp, ViewExamples}
import scalan.meta.MetaTestsDslExp

abstract class BaseViewTests extends BaseCtxTests {
  class ViewTestsCtx extends TestContext {
    def testLambdaResultHasViewsWithDataType[A,B](msg: String, f: Rep[A => B], expectedDataElem: Elem[_]) =
      _testLambdaResultHasViews(msg, f, Some(expectedDataElem))

    def testLambdaResultHasViews[A,B](msg: String, f: Rep[A => B]) =
      _testLambdaResultHasViews(msg, f, None)

    private def _testLambdaResultHasViews[A,B](msg: String, f: Rep[A => B], expectedDataElem: Option[Elem[_]]) = {
      val actualDataElem = f match {
        case LambdaResultHasViews(f, iso) =>
          Some(iso.eFrom)
        case _ =>
          None
      }
      assert(expectedDataElem == actualDataElem, msg)
    }

    def testGetIso[From, To](e: Elem[To], eFromExpected: Elem[From]) = {
      val iso = getIsoByElem(e)
      assertResult(eFromExpected)(iso.eFrom)
      iso
    }

    def testGetIsoWithEmit[From, To](name: String, e: Elem[To], eFromExpected: Elem[From]) = {
      val iso = testGetIso(e, eFromExpected)
      emit(name + ".from", iso.fromFun)
      emit(name + ".to", iso.toFun)
    }

    def testHasViews[T](s: Rep[T], eExpected: Elem[_]) = {
      val HasViews(source, iso) = s
      assertResult(eExpected)(iso.eFrom)
      assertResult(eExpected)(source.elem)
    }

    def testNoViews[T](s: Rep[T]) = {
      s match {
        case HasViews(source, iso) =>
          assert(false, s"no views expected, but found ($source, $iso)")
        case _ => // ok
      }
    }
  }
  class CtxForStructs extends ViewTestsCtx with SegmentsDslExp with MetaTestsDslExp {
    override def shouldUnpack(e: Elem[_])  = true
    override val currentPass = new Pass {
      val name = "test"
      override val config = PassConfig(true) // turn on tuple unpacking
    }
  }
}

class ViewTests extends BaseViewTests {

  test("LambdaResultHasViews") {
    val ctx = new ViewTestsCtx with ViewExamples with CommonExamples with SegmentsDslExp
    import ctx._
    testLambdaResultHasViewsWithDataType("t1", t1, element[(Int,Int)])
    testLambdaResultHasViewsWithDataType("t2", t2, element[(Int,Int)])
    testLambdaResultHasViews("t3", t3)
    testLambdaResultHasViews("t4", t4)
    testLambdaResultHasViews("t5", t5)
    testLambdaResultHasViews("t7", t7)
    testLambdaResultHasViews("t8", t8)
    testLambdaResultHasViewsWithDataType("v1", v1, element[(Int,Int)])
    testLambdaResultHasViewsWithDataType("v2", v2, element[((Int,Int),(Int, Int))])
    testLambdaResultHasViewsWithDataType("v3", v3, element[((Int,Int), Int)])
    testLambdaResultHasViewsWithDataType("v4", v4, element[(Int, (Int,Int))])
    testLambdaResultHasViews("v5", v5)
    testLambdaResultHasViews("v6", v6)
    testLambdaResultHasViews("v7", v7)
    testLambdaResultHasViewsWithDataType("v8", v8, element[((Int,Int) | Unit)])
    testLambdaResultHasViewsWithDataType("v9", v9, element[((Int,Int) | (Int,Int))])
  }

  test("LambdaResultHasViews_Sums") {
    val ctx = new ViewTestsCtx with SegmentsDslExp {
      lazy val v1 = fun { (in: Rep[Unit]) => in.asLeft[Slice] }
      lazy val v2 = fun { (in: Rep[(Int,Int)]) => SumView(in.asRight[Unit])(identityIso[Unit], isoSlice) }
    }
    import ctx._
    testLambdaResultHasViewsWithDataType("v1", v1, element[Unit | (Int,Int)])
    testLambdaResultHasViewsWithDataType("v2", v2, element[Unit | (Int,Int)])
    emit("v2", v2)
  }


  test("getIsoByElem") {
    val ctx = new ViewTestsCtx with SegmentsDslExp
    import ctx._

    testGetIso(element[Int], element[Int])
    testGetIso(element[(Int,Int)], element[(Int,Int)])
    testGetIso(element[(Int|Int)], element[(Int|Int)])
    testGetIso(element[Segment], element[Segment])
    testGetIso(element[Interval], element[(Int, Int)])
    testGetIso(element[(Slice, Int)], element[((Int, Int),Int)])
    testGetIso(element[(Unit, Int)], element[Int])
    testGetIso(element[(Int,Unit)], element[Int])
    testGetIso(element[((Int,Unit),Unit)], element[Int])
    testGetIso(element[(((Int,Unit),Unit),Unit)], element[Int])
    testGetIsoWithEmit("t1", element[(((Int,(Unit,Int)),Unit),Unit)], element[(Int,Int)])

    // Array
    testGetIso(element[Array[Int]], element[Array[Int]])
    testGetIso(element[Array[(Int,Int)]], element[Array[(Int,Int)]])
    testGetIso(element[Array[(Int,Unit)]], element[Array[Int]])

    testGetIso(element[Array[Segment]], element[Array[Segment]])
    testGetIso(element[Array[Interval]], element[Array[(Int,Int)]])

    // List
    testGetIso(element[List[Int]], element[List[Int]])
    testGetIso(element[List[(Int,Int)]], element[List[(Int,Int)]])
    testGetIso(element[List[(Int,Unit)]], element[List[Int]])

    testGetIso(element[List[Segment]], element[List[Segment]])
    testGetIso(element[List[Interval]], element[List[(Int,Int)]])

    // ArrayBuffer
    testGetIso(element[ArrayBuffer[Int]], element[ArrayBuffer[Int]])
    testGetIso(element[ArrayBuffer[(Int,Int)]], element[ArrayBuffer[(Int,Int)]])
    testGetIso(element[ArrayBuffer[(Int,Unit)]], element[ArrayBuffer[Int]])

    testGetIso(element[ArrayBuffer[Segment]], element[ArrayBuffer[Segment]])
    testGetIso(element[ArrayBuffer[Interval]], element[ArrayBuffer[(Int,Int)]])

    // Thunk
    testGetIso(element[Thunk[Int]], element[Thunk[Int]])
    testGetIso(element[Thunk[(Int,Int)]], element[Thunk[(Int,Int)]])
    testGetIso(element[Thunk[(Int,Unit)]], element[Thunk[Int]])

    testGetIso(element[Thunk[Segment]], element[Thunk[Segment]])
    testGetIso(element[Thunk[Interval]], element[Thunk[(Int,Int)]])
  }

  test("getIsoByElem for structs") {
    val ctx = new CtxForStructs
    import ctx._

    val seIntInt = tuple2StructElement[Int, Int]
    val seIntUnit = tuple2StructElement[Int, Unit]
    val seIntIntInt = tuple3StructElement[Int, Int, Int]
    testGetIso(element[(Int,Int)], seIntInt)
    testGetIso(element[(Int,Unit)], element[Int])  //NOTE: Unit absorbtion is stronger than converstion to structs

    testGetIsoWithEmit("t1", element[Interval], seIntInt)
    testGetIsoWithEmit("t2", element[(Int, (Int, Int))], seIntIntInt)
    testGetIsoWithEmit("t2.1", element[(Int, (Int, Unit))], seIntInt) //NOTE: first absorb then transfrom to struct
    testGetIsoWithEmit("t3", element[(Int, Interval)], seIntIntInt)
    testGetIsoWithEmit("t3.1", element[(Unit, Interval)], seIntInt)
    testGetIsoWithEmit("t4", element[(Interval, Interval)], tupleStructElement(element[Int], element[Int], element[Int], element[Int]))
    testGetIsoWithEmit("t4.1", element[(Unit,(Interval, Interval))], tupleStructElement(element[Int], element[Int], element[Int], element[Int]))
    testGetIsoWithEmit("t5", element[(Int, Array[(Int, Int)])], tupleStructElement(element[Int], arrayElement(seIntInt)))
    testGetIsoWithEmit("t5.1", element[(Int, Array[(Int, (Int,Unit))])], tupleStructElement(element[Int], arrayElement(seIntInt)))
    testGetIsoWithEmit("t6", element[(Array[(Int, Int)], Int)], tupleStructElement(arrayElement(seIntInt), element[Int]))

    testGetIsoWithEmit("a1", element[Array[(Int,Int)]], arrayElement(seIntInt))
    testGetIsoWithEmit("a2", element[Array[Interval]], arrayElement(seIntInt))
    testGetIsoWithEmit("a2.1", element[Array[(Interval,Unit)]], arrayElement(seIntInt))
    testGetIsoWithEmit("a3", element[Array[(Int, (Int, Int))]], arrayElement(tupleStructElement(element[Int], element[Int], element[Int])))
    testGetIsoWithEmit("a4", element[Array[(Int, Interval)]], arrayElement(tupleStructElement(element[Int], element[Int], element[Int])))
    testGetIsoWithEmit("a4.1", element[Array[((Unit,Int), Interval)]], arrayElement(tupleStructElement(element[Int], element[Int], element[Int])))
    testGetIsoWithEmit("a5", element[Array[(Interval, Interval)]], arrayElement(tupleStructElement(element[Int], element[Int], element[Int], element[Int])))
  }
}
