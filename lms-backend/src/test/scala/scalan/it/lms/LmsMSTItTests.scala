package scalan.it.lms

import scalan.compilation.lms.cxx.{CoreCXXLmsBackend, LmsCompilerCXX}
import scalan.{ScalanCommunityDslExp, ScalanCtxSeq}
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.LmsCompilerScala
import scalan.graphs.MST_example
import scalan.it.BaseItTests


abstract class LmsMstItTests extends BaseItTests {
  class ProgExp extends MST_example with ScalanCommunityDslExp with LmsCompilerScala { self =>
    val lms = new CommunityLmsBackend
  }
  class ProgExpCXX extends MST_example with ScalanCommunityDslExp with CoreBridge with LmsCompilerCXX { self =>
    val lms = new CoreCXXLmsBackend
  }
  class ProgSeq extends MST_example with ScalanCtxSeq

  val progStaged = new ProgExp
  val progStagedCXX = new ProgExpCXX
  val progSeq = new ProgSeq

  def sparseVectorData(arr: Array[Double]) = (0.until(arr.length).toArray, (arr, arr.length))
}

class LmsMstPrimeItTests extends LmsMstItTests {

  test("MST") {
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
    val links = graph.flatMap( i=> i)
    val edgeVals = graphValues.flatMap(i => i)
    val lens = graph.map(i => i.length)
    val offs = Array(0,2,5,9,12,14,18,21,24,28,30,32) //(Array(0) :+ lens.scan.slice(lens.length-1)
    val input = (links, (edgeVals, (offs, lens)))
    val right = Array(-1 , 0 , 1 , 2 , 3 , 2 , 5 , 2 , 6 , -2 , -2 , -2) //epends on the order of operations and algorithm behaviour
    val res = progSeq.MST(input)
    assert(res.sameElements(right), "res.sameElements(right)")
//    compareOutputWithSequential(progStaged)(progSeq.MST, progStaged.MST, "MST", input)
    getStagedOutput(progStagedCXX)(progStagedCXX.MST, "MST", input)
    println(res.mkString(" , "))
  }

}
