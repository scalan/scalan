package scalan.primitives

import scalan.{ScalanCtxExp, ScalanCtxSeq}
import scalan.BaseShouldTests
import scalan.codegen.GraphVizExport

class PrimitivesExamplesSuite extends BaseShouldTests {

  def seq = new ScalanCtxSeq with PrimitiveExamples {}
  def staged = new ScalanCtxExp with PrimitiveExamples {}
  "Examples trait" should "be mixable in Seq context" in {
      val ctx = seq
  }
  it should "be mixable in Staged context" in {
    val ctx = staged
  }
  
  val prefix = "test-out/scalan/primitives/"

  def testMethod(name: String) = {
    val ctx = new ScalanCtxExp with PrimitiveExamples with GraphVizExport {
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
  whenStaged should "ifsWithCommonCond" beArgFor { testMethod(_) }

  "If with common condition" should "have correct branches" in {
    val ctx = staged
    import ctx._
    val lam = ifsWithCommonCond.getLambda
    lam.branches.ifBranches.foreach { println(_) }
    lam.schedule.foreach { tp => println(s"${tp.sym} -> ${tp.rhs}") }
  }
}
