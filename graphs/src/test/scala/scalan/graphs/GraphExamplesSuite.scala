package scalan.graphs

import java.io.File
import java.lang.reflect.Method

import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.{BaseShouldTests, ScalanDslExp, ScalanDslStd}

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
    val ctx = new ScalanDslStd with GraphsDslStd with GraphExamples {}
  }

  it should "be constructed in Staged context" in {
    val ctx = new ScalanDslExp with GraphsDslExp with GraphExamples {}
  }
  */

  "in seq context1" should "execute functions" in {
    val ctx = new ScalanDslStd with GraphsDslStd with GraphExamples {}
    import ctx._
    val in1 = NestedCollectionFlat(Array(Array(1,2), Array(3,4), Array(5,6)))
    val in2 = NestedCollectionFlat(Array(Array(1.0,2.0), Array(3.0,4.0), Array(5.0,6.0)))
    val res = fromAndToAdj((in1,in2))
    println(res)
    res should be(in1.length)
  }
  "in seq context2" should "execute functions" in {
    val ctx = new ScalanDslStd with GraphsDslStd with GraphExamples {}
    val in1 = ctx.Collection.fromArray(Array(1.0,2.0, 3.0,4.0, 5.0,6.0))(ctx.DoubleElement)
    val in2 = 2
    val res = ctx.fromAndToInc((in1,in2))
    println(res)
    res should be(in2)
  }

  "in seq context3" should "execute functions" in {
    val ctx = new ScalanDslStd with GraphsDslStd with GraphExamples {}
    import ctx._
    val in1 = NestedCollectionFlat(graph)
    val in2 = NestedCollectionFlat(graphValues)
    val res = mstFunAdj((in1,in2))
    println(res)
  }

  "in seq context4" should "execute functions" in {
    val ctx = new ScalanDslStd with GraphsDslStd with GraphExamples {}

    val vertexNum = graph.length
    val incMatrix = (graph zip graphValues).flatMap({ in =>
      val row = in._1
      val vals = in._2
      val zero = scala.Array.fill(vertexNum)(0.0)
      for (i <- 0 to row.length-1) { zero(row(i)) = vals(i) }
      zero
    })

    val inM = ctx.Collection.fromArray(incMatrix)(ctx.DoubleElement)
    val res = ctx.mstFunInc((inM,vertexNum))
    println(res)
    //res should be(in1.length)
  }

  "in seq context5" should "execute functions" in {
    val ctx = new ScalanDslStd with GraphsDslStd with GraphExamples {}
    import ctx._
    val in1 = NestedCollectionFlat(graph)
    val in2 = NestedCollectionFlat(graphValues)

    val res = mstFun1Adj((in1.values.arr,(in2.values.arr, (in1.segOffsets.arr, in1.segLens.arr))) )
    println(res.mkString(","))
    //res should be(in1.length)
  }

  "in seq context6" should "execute functions" in {
    val ctx = new ScalanDslStd with GraphsDslStd with GraphExamples {}

    val vertexNum = graph.length
    val incMatrix = (graph zip graphValues).flatMap({ in =>
      val row = in._1
      val vals = in._2
      val zero = scala.Array.fill(vertexNum)(0.0)
      for (i <- 0 to row.length-1) { zero(row(i)) = vals(i) }
      zero
    })

    val res = ctx.mstFun1Inc((incMatrix, vertexNum))
    println(res.mkString(","))
    //res should be(in1.length)
  }
  "in seq context7" should "execute functions" in {
    val ctx = new ScalanDslStd with GraphsDslStd with GraphExamples {}
    import ctx._
    val in1 = NestedCollectionFlat(graph)
    val in2 = NestedCollectionFlat(graphValues)
    val res = mstFunAdjMap((in1,in2))
    println(res)
    //res should be(in1.length)
  }
  "in seq context8" should "execute functions" in {
    val ctx = new ScalanDslStd with GraphsDslStd with GraphExamples {}

    val vertexNum = graph.length
    val incMatrix = (graph zip graphValues).flatMap({ in =>
      val row = in._1
      val vals = in._2
      val zero = scala.Array.fill(vertexNum)(0.0)
      for (i <- 0 to row.length-1) { zero(row(i)) = vals(i) }
      zero
    })

    val inM = ctx.Collection.fromArray(incMatrix)(ctx.DoubleElement)
    val res = ctx.mstFunIncMap((inM,vertexNum))
    println(res)
    //res should be(in1.length)
  }

  "in seq context9" should "execute functions" in {
    val ctx = new ScalanDslStd with GraphsDslStd with GraphExamples {}
    import ctx._
    val in1 = NestedCollectionFlat(graph)
    val in2 = NestedCollectionFlat(graphValues)

    val res = mstFun1AdjMap((in1.values.arr,(in2.values.arr, (in1.segOffsets.arr, in1.segLens.arr))) )
    println(res.mkString(","))
    //res should be(in1.length)
  }

  "in seq context10" should "execute functions" in {
    val ctx = new ScalanDslStd with GraphsDslStd with GraphExamples {}

    val vertexNum = graph.length
    val incMatrix = (graph zip graphValues).flatMap({ in =>
      val row = in._1
      val vals = in._2
      val zero = scala.Array.fill(vertexNum)(0.0)
      for (i <- 0 to row.length-1) { zero(row(i)) = vals(i) }
      zero
    })

    val res = ctx.mstFun1IncMap((incMatrix, vertexNum))
    println(res.mkString(","))
    //res should be(in1.length)
  }


  def testMethod(name: String) = {

    val ctx = new ScalanDslExp with GraphsDslExp with GraphExamples with GraphVizExport {
      override def isInvokeEnabled(d: Def[_], m: Method) = true //HACK: invoke all domain methods if possible //TODO this is not how it should be specified
    }
    val f = ctx.getStagedFunc(name)
    ctx.emitDepGraph(f, prefix, name)(GraphVizConfig.default)
  }

  val whenStaged = "when staged"
  whenStaged should "fromAndToAdj" beArgFor { testMethod(_) }
  whenStaged should "fromAndToInc" beArgFor { testMethod(_) }
  whenStaged should "mstFunAdj" beArgFor { testMethod(_) }
  whenStaged should "mstFunInc" beArgFor { testMethod(_) }
  whenStaged should "mstFun1Adj" beArgFor { testMethod(_) }
  whenStaged should "mstFun1Inc" beArgFor { testMethod(_) }
  whenStaged should "mstFunAdjMap" beArgFor { testMethod(_) }
  whenStaged should "mstFunIncMap" beArgFor { testMethod(_) }
  whenStaged should "mstFun1AdjMap" beArgFor { testMethod(_) }
  whenStaged should "mstFun1IncMap" beArgFor { testMethod(_) }
}
