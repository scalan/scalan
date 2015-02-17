package scalan.graphs

import java.io.File
import java.lang.reflect.Method

import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.{BaseShouldTests, ScalanCtxExp, ScalanCtxSeq}

/**
 * Created by afilippov on 2/17/15.
 */
class GraphExamplesSuite extends BaseShouldTests{
  "when mixing trait" should "be constructed in Seq context" in {
    val ctx = new ScalanCtxSeq with GraphsDslSeq with GraphExamples {}
  }

  it should "be constructed in Staged context" in {
    val ctx = new ScalanCtxExp with GraphsDslExp with GraphExamples {}
  }

  "in seq context" should "execute functions" in {
    val ctx = new ScalanCtxSeq with GraphsDslSeq with GraphExamples {}
    val in1 = ctx.fromJuggedArray(Array(Array(1,2), Array(3,4), Array(5,6)))(ctx.IntElement)
    val in2 = ctx.fromJuggedArray(Array(Array(1.0,2.0), Array(3.0,4.0), Array(5.0,6.0)))(ctx.DoubleElement)
    val res = ctx.fromAndTo((in1,in2))
    println(res)
    res should be(in1.length)
  }

  def testMethod(name: String) = {
    val ctx = new ScalanCtxExp with GraphsDslExp with GraphExamples with GraphVizExport {
      override def isInvokeEnabled(d: Def[_], m: Method) = true //HACK: invoke all domain methods if possible //TODO this is not how it should be specified
    }
    val f = ctx.getStagedFunc(name)
    ctx.emitDepGraph(f, new File(prefix, s"$name.dot"))(GraphVizConfig.default)
  }

  //val whenStaged = "when staged"
  //whenStaged should "fromArray" beArgFor { testMethod(_) }
  //whenStaged should "fromArrayOfPairs" beArgFor { testMethod(_) }
  //whenStaged should "fromAndTo" beArgFor { testMethod(_) }

}
