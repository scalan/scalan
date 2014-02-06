package tests.scalan.arrays

import scalan.{ScalanCtxStaged, ScalanCtxSeq}
import org.scalatest.{Matchers, FlatSpec}
import scalan.arrays.{PArraysDslExp, PArraysDslSeq}
import tests.GraphVizExport

class PArrayExamplesSuite extends FlatSpec with Matchers {

  "when mixing trait" should "be constructed in Seq context" in {
      val ctx = new ScalanCtxSeq with PArraysDslSeq with PArrayExamples {}
  }
  it should "be constructed in Staged context" in {
    val ctx = new ScalanCtxStaged with PArraysDslExp with PArrayExamples {}
  }

  "in seq context" should "execute functions" in {
    val ctx = new ScalanCtxSeq with PArraysDslSeq with PArrayExamples {}
    val in = Array((1,2f), (3,4f), (5,6f))
    val res = ctx.fromAndTo(in)
    res should be(in)
  }

  val prefix = "test-out/scalan/arrays/"
  "in staged context" should "stage functions" in {
   
    val ctx = new ScalanCtxStaged with PArraysDslExp with PArrayExamples with GraphVizExport {}
    import ctx._
    
    var f: Exp[_] = fromArray
    emitDepGraph(f, prefix + "fromArray.dot", false)

    f = ctx.fromArrayOfPairs
    emitDepGraph(f, prefix + "fromArrayOfPairs.dot", false)

    f = ctx.fromAndTo
    emitDepGraph(f, prefix + "fromAndTo.dot", false)

    f = ctx.mapped
    emitDepGraph(f, prefix + "mapped.dot", false)

    f = ctx.zippedMap
    emitDepGraph(f, prefix + "zippedMap.dot", false)

    f = ctx.mapped2
    emitDepGraph(f, prefix + "mapped2.dot", false)

    f = ctx.splitMap
    emitDepGraph(f, prefix + "splitMap.dot", false)

    f = ctx.splitMap2
    emitDepGraph(f, prefix + "splitMap2.dot", false)

    f = ctx.mapInc3Times
    emitDepGraph(f, prefix + "mapInc3Times.dot", false)

    f = ctx.splitMap3
    emitDepGraph(f, prefix + "splitMap3.dot", false)

    f = ctx.splitMapMap
    emitDepGraph(f, prefix + "splitMapMap.dot", false)

    f = ctx.mapScalar
    emitDepGraph(f, prefix + "mapScalar.dot", false)

//
//  lazy val fromAndTo = fun { xs: Arr[(Int,Float)] => PArray(xs).arr }
//
//  lazy val mapped = fun {(xs: PA[Int]) => xs.mapBy(inc) }
//  lazy val zippedMap = fun {(xs: PA[Int]) => (xs zip xs).mapBy(tupled) }
//  lazy val mapped2 = fun {(xs: PA[Int]) => xs.mapBy(inc2) }
//
//  lazy val splitMap = fun {(xs: PA[Int]) => Pair(xs.mapBy(inc), xs.mapBy(inc2)) }
//  lazy val splitMap2 = fun {(xs: PA[Int]) => Pair(xs.mapBy(inc_times), xs.mapBy(inc2)) }
//  lazy val mapInc3Times = fun {(xs: PA[Int]) => Pair(xs.mapBy(inc), Pair(xs.mapBy(inc), xs.mapBy(inc))) }
//  lazy val splitMap3 = fun {(xs: PA[Int]) => Pair(xs.mapBy(inc), Pair(xs.mapBy(inc2), xs.mapBy(inc_times))) }
//  lazy val splitMapMap = fun {(xs: PA[Int]) => Pair(xs.mapBy(inc), xs.mapBy(inc2).mapBy(inc_times)) }
//
//  lazy val mapScalar = fun {(xs: PA[Int]) => xs.mapBy(scalar) }
    // val f2 = ctx.fromAndTo
    // ctx.emitDepGraph(f2, prefix + "fromAndTo.dot", false)
    //val f2 = ctx.inc
  }
 }
