package scalan.it.lms

import scalan.community.ScalanCommunityDslExp
import scalan.{ScalanCtxSeq, ScalanCtxExp}
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.LmsCompilerScala
import scalan.graphs.MST_example
import scalan.it.BaseItTests


abstract class LmsMstItTests extends BaseItTests {
  class ProgExp extends MST_example with ScalanCommunityDslExp with LmsCompilerScala { self =>
    def makeBridge[A, B] = new CommunityBridge[A, B] {
      val scalan = self
      val lms = new CommunityLmsBackend
    }
  }
  class ProgSeq extends MST_example with ScalanCtxSeq

  val progStaged = new ProgExp
  val progSeq = new ProgSeq

  def sparseVectorData(arr: Array[Double]) = (0.until(arr.length).toArray, (arr, arr.length))
}

class LmsMstPrimeItTests extends LmsMstItTests {
  import progSeq._
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

  test("MST_adjList") {

    val links = graph.flatMap( i=> i)
    val edgeVals = graphValues.flatMap(i => i)
    val lens = graph.map(i => i.length)
    val offs = Array(0,2,5,9,12,14,18,21,24,28,30,32) //(Array(0) :+ lens.scan.slice(lens.length-1)
    val input = (links, (edgeVals, (offs, lens)))
    val res = progSeq.MST_adjlist(input)
    //compareOutputWithSequential(progStaged)(progSeq.MST, progStaged.MST, "MST", input)
    println(res.mkString(" , "))
  }

  test("MST_adjMatrix") {
    val vertexNum = graph.length
    val incMatrix = (graph zip graphValues).flatMap({ in =>
      val row = in._1
      val vals = in._2
      val zero = scala.Array.fill(vertexNum)(0.0)
      for (i <- 0 to row.length-1) { zero(row(i)) = vals(i) }
      zero
    })
    val input = (incMatrix, vertexNum)
    val res = progSeq.MST_adjmatrix(input)
    //compareOutputWithSequential(progStaged)(progSeq.MST, progStaged.MST, "MST", input)
    println(res.mkString(" , "))
  }

}
