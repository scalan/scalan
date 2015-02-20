package scalan.graphs

import java.io.File
import java.lang.reflect.Method

import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.{BaseShouldTests, ScalanCtxExp, ScalanCtxSeq}

/**
 * Created by afilippov on 2/17/15.
 */
class GraphExamplesSuite extends BaseShouldTests{
  val graph = Array(
    Array(1, 8),
    Array(0, 2, 8),
    Array(1, 3, 5, 7),
    Array(2, 4, 5),
    Array(3, 5),
    Array(2, 3, 4, 6),
    Array(5, 7, 8),
    Array(2, 6, 8),
    Array(0, 1, 6, 7),
    Array(10,11),
    Array(9,11),
    Array(9,10)
  )

  val graphValues = Array(
    Array(4.0, 8.0),
    Array(4.0, 8.0, 11.0),
    Array(8.0, 7.0, 4.0, 2.0),
    Array(7.0, 9.0, 14.0),
    Array(9.0, 10.0),
    Array(4.0, 14.0, 10.0, 2.0),
    Array(2.0, 6.0, 1.0),
    Array(2.0, 6.0, 7.0),
    Array(8.0, 11.0, 1.0, 7.0),
    Array(1.0, 2.0),
    Array(1.0, 3.0),
    Array(2.0, 3.0)
  )


  /*"when mixing trait" should "be constructed in Seq context" in {
    val ctx = new ScalanCtxSeq with GraphsDslSeq with GraphExamples {}
  }

  it should "be constructed in Staged context" in {
    val ctx = new ScalanCtxExp with GraphsDslExp with GraphExamples {}
  }
  */
  "in seq context1" should "execute functions" in {
    val ctx = new ScalanCtxSeq with GraphsDslSeq with GraphExamples {}
    val in1 = ctx.fromJuggedArray(Array(Array(1,2), Array(3,4), Array(5,6)))(ctx.IntElement)
    val in2 = ctx.fromJuggedArray(Array(Array(1.0,2.0), Array(3.0,4.0), Array(5.0,6.0)))(ctx.DoubleElement)
    val res = ctx.fromAndTo((in1,in2))
    println(res)
    res should be(in1.length)
  }

  "in seq context2" should "execute functions" in {
    val ctx = new ScalanCtxSeq with GraphsDslSeq with GraphExamples {}

    val in1 = ctx.fromJuggedArray(graph)(ctx.IntElement)
    val in2 = ctx.fromJuggedArray(graphValues)(ctx.DoubleElement)
    val res = ctx.mstFun((in1,in2))
    println(res)
    //res should be(in1.length)
  }

  "in seq context3" should "execute functions" in {
    val ctx = new ScalanCtxSeq with GraphsDslSeq with GraphExamples {}

    val in1 = ctx.fromJuggedArray(graph)(ctx.IntElement)
    val in2 = ctx.fromJuggedArray(graphValues)(ctx.DoubleElement)

    val res = ctx.mstFun1((in1.values.arr,(in2.values.arr, (in1.segOffsets.arr, in1.segLens.arr))) )
    println(res.mkString(","))
    //res should be(in1.length)
  }
  def testMethod(name: String) = {
    val ctx = new ScalanCtxExp with GraphsDslExp with GraphExamples with GraphVizExport {
      override def isInvokeEnabled(d: Def[_], m: Method) = true //HACK: invoke all domain methods if possible //TODO this is not how it should be specified
    }
    val f = ctx.getStagedFunc(name)
    ctx.emitDepGraph(f, new File(prefix, s"$name.dot"))(GraphVizConfig.default)
  }

  val whenStaged = "when staged"
  //whenStaged should "fromArray" beArgFor { testMethod(_) }
  whenStaged should "fromAndTo" beArgFor { testMethod(_) }
  whenStaged should "mstFun" beArgFor { testMethod(_) }
  whenStaged should "mstFun1" beArgFor { testMethod(_) }

}
