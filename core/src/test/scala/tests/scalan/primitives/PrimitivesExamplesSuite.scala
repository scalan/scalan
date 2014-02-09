package tests.scalan.primitives

import scalan.{ScalanCtxStaged, ScalanCtxSeq}
import org.scalatest.FlatSpec
import tests.GraphVizExport
import scalan.staged.ProgramGraphs
import tests.BaseShouldTests
import scalan.arrays.PArraysDslExp
import tests.scalan.arrays.PArrayExamples

class PrimitivesExamplesSuite extends BaseShouldTests {

  "Examples trait" should "be mixable in Seq context" in {
      val ctx = new ScalanCtxSeq with PrimitiveExamples {}
  }
  it should "be mixable in Staged context" in {
    val ctx = new ScalanCtxStaged with PrimitiveExamples {}
  }
  
  val prefix = "test-out/scalan/primitives/"

  def testMethod(name: String) = {
    val ctx = new ScalanCtxStaged with PrimitiveExamples with GraphVizExport {
      this.invokeEnabled = true //HACK: invoke all domain methods if possible //TODO this is not how it should be specified
    }
    val f = ctx.getStagedFunc(name)
    ctx.emitDepGraph(f, s"$prefix$name.dot", false)
  }

  val whenStaged = "when staged"
  whenStaged should "id" beArgFor { testMethod(_) }
  whenStaged should "inc" beArgFor { testMethod(_) }
  whenStaged should "curred" beArgFor { testMethod(_) }
  whenStaged should "tupled" beArgFor { testMethod(_) }
  whenStaged should "highOrder" beArgFor { testMethod(_) }
  whenStaged should "inc2" beArgFor { testMethod(_) }
  whenStaged should "inc_times" beArgFor { testMethod(_) }
  whenStaged should "scalar" beArgFor { testMethod(_) }
  
}
