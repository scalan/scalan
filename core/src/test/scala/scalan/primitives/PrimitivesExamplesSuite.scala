package scalan.primitives

import java.io.File
import java.lang.reflect.Method

import scalan.{ScalanDslExp, ScalanDslSeq}
import scalan.BaseShouldTests
import scalan.compilation.GraphVizExport

class PrimitivesExamplesSuite extends BaseShouldTests {

  def seq = new ScalanDslSeq with PrimitiveExamples {}
  def staged = new ScalanDslExp with PrimitiveExamples {}
  "Examples trait" should "be mixable in Seq context" in {
      val ctx = seq
  }
  it should "be mixable in Staged context" in {
    val ctx = staged
  }
  
  def testMethod(name: String) = {
    val ctx = new ScalanDslExp with PrimitiveExamples with GraphVizExport {
      override def isInvokeEnabled(d: Def[_], m: Method) = true //HACK: invoke all domain methods if possible //TODO this is not how it should be specified
    }
    import ctx._
    val f = getStagedFunc(name)
    emitDepGraph(f, new File(prefix, s"$name.dot"))(ctx.defaultGraphVizConfig)
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
