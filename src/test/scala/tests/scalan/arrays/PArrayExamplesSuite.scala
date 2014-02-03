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
    val f1 = ctx.fromAndTo
    ctx.emitDepGraph(f1, prefix + "fromAndTo.dot", false)
    //val f2 = ctx.inc
  }
 }
