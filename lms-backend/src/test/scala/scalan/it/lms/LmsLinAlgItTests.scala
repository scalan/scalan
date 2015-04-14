package scalan.it.lms

import scalan.ScalanCtxExp
import scalan.util.FileUtil._
import scalan.{ScalanCommunityDslExp, ScalanCommunityDslSeq}
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.it.BaseItTests
import scalan.linalgebra.{LinearAlgebraExamples, MatricesDslSeq}

abstract class LmsLinAlgItTests extends BaseItTests {
  class ProgExp extends LinearAlgebraExamples with ScalanCommunityDslExp with ScalanCtxExp with CommunityLmsCompilerScala with CommunityBridge {
    val lms = new CommunityLmsBackend
  }
  class ProgSeq extends LinearAlgebraExamples with MatricesDslSeq with ScalanCommunityDslSeq
  
  val progStaged = new ProgExp
  val progSeq = new ProgSeq
  
  def sparseVectorData(arr: Array[Double]) = (0.until(arr.length).toArray, (arr, arr.length))
}

class LmsMvmItTests extends LmsLinAlgItTests {
  import progSeq._

  test("ddmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.ddmvm, progStaged.ddmvm, "ddmvm", in)
  }

  test("ddmvmList") {
    val inM = List(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.ddmvmList, progStaged.ddmvmList, "ddmvmList", in)
  }

  test("dsmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.dsmvm, progStaged.dsmvm, "dsmvm", in)
  }

  test("sdmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.sdmvm, progStaged.sdmvm, "sdmvm", in)
  }

  test("ssmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.ssmvm, progStaged.ssmvm, "ssmvm", in)
  }

  test("fdmvm") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.fdmvm, progStaged.fdmvm, "fdmvm", in)
  }

  test("fsmvm") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.fsmvm, progStaged.fsmvm, "fsmvm", in)
  }

  test("ddmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.ddmvm0, progStaged.ddmvm0, "ddmvm0", in)
  }

  test("dsmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.dsmvm0, progStaged.dsmvm0, "dsmvm0", in)
  }

  test("sdmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.sdmvm0, progStaged.sdmvm0, "sdmvm0", in)
  }

  test("ssmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.ssmvm0, progStaged.ssmvm0, "ssmvm0", in)
  }

  test("fdmvm0") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.fdmvm0, progStaged.fdmvm0, "fdmvm0", in)
  }

  test("fsmvm0") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.fsmvm0, progStaged.fsmvm0, "fsmvm0", in)
  }  
}

class LmsMmmItTests extends LmsLinAlgItTests {
  import progSeq._

  test("ddmmm") {
    val inM1 = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inM2 = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val in = Pair(inM1, inM2)
    val out = Array(Array(1.0, 2.0), Array(0.0, 1.0))
    compareOutputWithSequential(progStaged)(progSeq.ddmmm, progStaged.ddmmm, "ddmmm", in)
  }

  test("ssmmm") {
    //pending
    val inM1 = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inM2 = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val in = Pair(inM1, inM2)
    val out = Array(Array(1.0, 2.0), Array(0.0, 1.0))
    compareOutputWithSequential(progStaged)(progSeq.ssmmm, progStaged.ssmmm, "ssmmm", in)
    compareOutputWithExpected(progStaged)(out, progStaged.ssmmm, "ssmmm_out", in)
  }

  test("ffmmm") {
    val inM1 = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inM2 = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val in = Pair(inM1, inM2)
    val out = Array(Array(1.0, 2.0), Array(0.0, 1.0))
    compareOutputWithSequential(progStaged)(progSeq.ffmmm, progStaged.ffmmm, "ffmmm", in)
  }
}

class AbstractElemItTests extends LmsLinAlgItTests {
  import progSeq._

  lazy val jArrTrain2x2 = Array(Array((0, 5.0), (1, 3.0)), Array((1, 4.0)))
  lazy val jArrTest2x2 = Array(Array((0, 5.0), (1, 3.0)), Array((0, 3.0), (1, 4.0)))

  def getNArrayWithSegmentsFromJaggedArray(jaggedArray: Array[Array[(Int, Double)]]) = {
    val arr = jaggedArray.flatMap(v => v)
    val lens = jaggedArray.map(i => i.length)
    val offs = lens.scanLeft(0)((x, y) => x + y).take(lens.length)
    (arr, offs zip lens)
  }

  test("pattern matching vectors with abstract elem works") {
    val arrTrain = Array((0, 5.0), (1, 3.0), (1, 4.0))
    lazy val width = 5

    val in = Tuple(width, arrTrain)
    getStagedOutput(progStaged)(progStaged.dotWithAbstractElem, "patternMatchAbstract", in)
  }

  test("elems divergence in if_then_else branches") {
    // Different branches of if_then_else operator unexpectedly produce different elems.
    // This causes Sums and SumViews to appear.
    // Produced code could not be executed. The test only build final graph.
    // Bad behavior remains (with a different graph) if DenseVector.+^ is changed to return DenseVector
    pending
    val matrix = Array(Array(0, 5), Array(1, 3), Array(1, 4))
    val vector = Array(1,2)

    val progStaged = new ProgExp
    val in = Tuple(matrix, vector)
    progStaged.buildGraph(file(prefix, "simpleSum"), "simpleSum", progStaged.funSimpleSum, graphVizConfig)(progStaged.defaultCompilerConfig)
  }

}

class VectorMethodsItTests extends LmsLinAlgItTests {

  import progSeq._

  lazy val vector1 = Array(Pair(0, 1.0), Pair(1, 2.0), Pair(2, 3.0), Pair(3, 4.0), Pair(4, 5.0))
  lazy val vector2 = Array(Pair(0, 1.0), Pair(7, 3.0), Pair(12, 5.0))

  test("applySparseVector1") {

    val progStaged = new ProgExp

    lazy val len = 5
    val i = 2
    val in = Tuple(vector1, len, i)
    val out = 3
    compareOutputWithSequential(progStaged)(progSeq.applySparseVector, progStaged.applySparseVector, "applySparseVector1", in)
    compareOutputWithExpected(progStaged)(out, progStaged.applySparseVector, "applySparseVector1e", in)
  }

  test("applySparseVector2") {

    val progStaged = new ProgExp

    lazy val len = 12
    val i = 12
    val in = Tuple(vector2, len, i)
    val out = 5
    compareOutputWithSequential(progStaged)(progSeq.applySparseVector, progStaged.applySparseVector, "applySparseVector2", in)
    compareOutputWithExpected(progStaged)(out, progStaged.applySparseVector, "applySparseVector2e", in)
  }
}