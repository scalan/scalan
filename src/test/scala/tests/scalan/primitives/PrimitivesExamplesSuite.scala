/**
 * User: s00747473
 * Date: 11/16/13
 */
package tests.scalan.primitives

import scalan.{ScalanCtxStaged, ScalanCtxSeq}
import org.scalatest.FlatSpec
import tests.GraphVizExport
import scalan.staged.ProgramGraphs

class PrimitivesExamplesSuite extends FlatSpec {

  "when mixing trait" should "be constructed in Seq context" in {
      val ctx = new ScalanCtxSeq with PrimitiveExamples {}
  }
  it should "be constructed in Staged context" in {
    val ctx = new ScalanCtxStaged with PrimitiveExamples {}
  }
  
  val prefix = "test-out/scalan/primitives/"
  
    "in staged context" should "stage functions" in {
    val ctx = new ScalanCtxStaged with PrimitiveExamples with GraphVizExport with ProgramGraphs {}
    import ctx._
//    val f1 = ctx.id
//    ctx.emitDepGraph(f1, prefix + "id.dot", false)

    val f2 = inc
    val g = new PGraph(List(f2))
    g.scheduleAll.foreach(tp => println(s"${tp.sym} -> ${tp.rhs}"))
    
    emitDepGraph(g.roots, prefix + "inc.dot", false)
  }
 }
