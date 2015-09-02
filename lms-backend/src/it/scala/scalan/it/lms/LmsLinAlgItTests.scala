package scalan.it.lms

import scalan._
import scalan.compilation.lms.uni._
import scalan.util.FileUtil._
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.it.BaseItTests
import scalan.linalgebra.{LinearAlgebraExamples, MatricesDslSeq}

abstract class LmsLinAlgItTests extends BaseItTests {
  class ProgExp extends LinearAlgebraExamples with ScalanCommunityDslExp with JNIExtractorOpsExp

  class ProgSeq extends LinearAlgebraExamples with MatricesDslSeq with ScalanCommunityDslSeq

  val progStaged = new CommunityLmsCompilerScala(new ProgExp) with CommunityBridge

  val progStagedU = new LmsCompilerUni(new ProgExp) with CommunityBridge with CommunityMethodMappingDSL

  val progSeq = new ProgSeq

  val compilerConfigU = progStagedU.defaultCompilerConfig.copy(scalaVersion = Some("2.11.2"))
  
  def sparseVectorData(arr: Array[Double]) = (0.until(arr.length).toArray, (arr, arr.length))
}

class LmsMvmItTests extends LmsLinAlgItTests {
  import progSeq._

  test("ddmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged, progSeq)(_.ddmvm, "ddmvm", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.ddmvm, "ddmvm", in, compilerConfigU)
  }

  test("ddmvmList(scala)") {
    val inM = List(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged, progSeq)(_.ddmvmList, "ddmvmList", in)
  }

  test("dsmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged, progSeq)(_.dsmvm, "dsmvm", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.dsmvm, "dsmvm", in)
  }

  test("sdmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged, progSeq)(_.sdmvm, "sdmvm", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.sdmvm, "sdmvm", in)
  }

  test("ssmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged, progSeq)(_.ssmvm, "ssmvm", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.ssmvm, "ssmvm", in)
  }

  test("fdmvm") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged, progSeq)(_.fdmvm, "fdmvm", in)
  }

  test("fsmvm") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged, progSeq)(_.fsmvm, "fsmvm", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.fsmvm, "fsmvm", in)
  }

  test("cdmvm") {
    val inM = (3.0, (2, 2))
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(15.0, 15.0)
    compareOutputWithExpected(progStaged)(out, _.cdmvm, "cdmvm_out", in)
    compareOutputWithSequential(progStaged, progSeq)(_.cdmvm, "cdmvm", in)
  }

  test("csmvm") {
    val inM = (3.0, (2, 2))
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(15.0, 15.0)
    compareOutputWithExpected(progStaged)(out, _.csmvm, "csmvm_out", in)
    compareOutputWithSequential(progStaged, progSeq)(_.csmvm, "csmvm", in)
  }

  test("dcmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = (2.0, 2)
    val in = Pair(inM, inV)
    val out = Array(4.0, 2.0)
    compareOutputWithExpected(progStaged)(out, _.dcmvm, "dcmvm_out", in)
    compareOutputWithSequential(progStaged, progSeq)(_.dcmvm, "dcmvm", in)
  }

  test("ccmvm") {
    val inM = (3.0, (2, 2))
    val inV = (2.0, 2)
    val in = Pair(inM, inV)
    val out = Array(12.0, 12.0)
    compareOutputWithExpected(progStaged)(out, _.ccmvm, "ccmvm_out", in)
    compareOutputWithSequential(progStaged, progSeq)(_.ccmvm, "ccmvm", in)
  }

  test("dgdmvm") {
    val inM = Array(1.0, 2.0, 3.0)
    val inV = Array(2.0, 4.0, 6.0)
    val in = Pair(inM, inV)
    val out = Array(2.0, 8.0, 18.0)
    compareOutputWithExpected(progStaged)(out, _.dgdmvm, "dgdmvm_out", in)
    compareOutputWithSequential(progStaged, progSeq)(_.dgdmvm, "dgdmvm", in)
  }

  test("dgsmvm") {
    val inM = Array(1.0, 2.0, 3.0)
    val inV = sparseVectorData(Array(2.0, 4.0, 6.0))
    val in = Pair(inM, inV)
    val out = Array(2.0, 8.0, 18.0)
    compareOutputWithExpected(progStaged)(out, _.dgsmvm, "dgsmvm_out", in)
    compareOutputWithSequential(progStaged, progSeq)(_.dgsmvm, "dgsmvm", in)
  }

  test("dgcmvm") {
    val inM = Array(1.0, 2.0, 3.0)
    val inV = (2.0, 3)
    val in = Pair(inM, inV)
    val out = Array(2.0, 4.0, 6.0)
    compareOutputWithExpected(progStaged)(out, _.dgcmvm, "dgcmvm_out", in)
    compareOutputWithSequential(progStaged, progSeq)(_.dgcmvm, "dgcmvm", in)
  }

  test("ddmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged, progSeq)(_.ddmvm0, "ddmvm0", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.ddmvm0, "ddmvm0", in)
  }

  test("dsmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged, progSeq)(_.dsmvm0, "dsmvm0", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.dsmvm0, "dsmvm0", in)
  }

  test("sdmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged, progSeq)(_.sdmvm0, "sdmvm0", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.sdmvm0, "sdmvm0", in)
  }

  test("ssmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged, progSeq)(_.ssmvm0, "ssmvm0", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.ssmvm0, "ssmvm0", in)
  }

  test("fdmvm0") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged, progSeq)(_.fdmvm0, "fdmvm0", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.fdmvm0, "fdmvm0", in)
  }

  test("fsmvm0") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged, progSeq)(_.fsmvm0, "fsmvm0", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.fsmvm0, "fsmvm0", in)
  }
}

class LmsMmmItTests extends LmsLinAlgItTests {
  import progSeq._

  test("ddmmm") {
    val inM1 = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inM2 = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val in = Pair(inM1, inM2)
    val out = Array(Array(1.0, 2.0), Array(0.0, 1.0))
    compareOutputWithSequential(progStaged, progSeq)(_.ddmmm, "ddmmm", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.ddmmm, "ddmmm", in)
  }

  test("ssmmm") {
    //pending
    val inM1 = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inM2 = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val in = Pair(inM1, inM2)
    val out = Array(Array(1.0, 2.0), Array(0.0, 1.0))
    compareOutputWithSequential(progStaged, progSeq)(_.ssmmm, "ssmmm", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.ssmmm, "ssmmm", in)
    compareOutputWithExpected(progStaged)(out, _.ssmmm, "ssmmm_out", in)
    compareOutputWithExpected(progStagedU)(out, _.ssmmm, "ssmmm_out", in)
  }

  test("ffmmm") {
    val inM1 = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inM2 = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val in = Pair(inM1, inM2)
    val out = Array(Array(1.0, 2.0), Array(0.0, 1.0))
    compareOutputWithSequential(progStaged, progSeq)(_.ffmmm, "ffmmm", in)
    compareOutputWithSequential(progStagedU, progSeq)(_.ffmmm, "ffmmm", in)
  }

  test("ccmmm") {
    val inM1 = (3.0, (2, 2))
    val inM2 = (2.0, (2, 2))
    val in = Pair(inM1, inM2)
    val out = Array(Array(12.0, 12.0), Array(12.0, 12.0))
    compareOutputWithExpected(progStaged)(out, _.ccmmm, "ccmmm", in)
    compareOutputWithSequential(progStaged, progSeq)(_.ccmmm, "ccmmm", in)
  }

  test("cfmmm") {
    val inM1 = (3.0, (2, 2))
    val inM2 = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val in = Pair(inM1, inM2)
    val out = Array(Array(3.0, 6.0), Array(3.0, 6.0))
    compareOutputWithExpected(progStaged)(out, _.cfmmm, "cfmmm", in)
    compareOutputWithSequential(progStaged, progSeq)(_.cfmmm, "cfmmm", in)
  }

  test("dgfmmm") {
    val inM1 = Array(2.0, 3.0)
    val inM2 = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val in = Pair(inM1, inM2)
    val out = Array(Array(2.0, 2.0), Array(0.0, 3.0))
    compareOutputWithExpected(progStaged)(out, _.dgfmmm, "dgfmmm", in)
    compareOutputWithSequential(progStaged, progSeq)(_.dgfmmm, "dgfmmm", in)
  }

  test("dgdgmmm") {
    val inM1 = Array(2.0, 3.0)
    val inM2 = Array(2.0, 3.0)
    val in = Pair(inM1, inM2)
    val out = Array(Array(4.0, 0.0), Array(0.0, 9.0))
    compareOutputWithExpected(progStaged)(out, _.dgdgmmm, "dgdgmmm", in)
    compareOutputWithSequential(progStaged, progSeq)(_.dgdgmmm, "dgdgmmm", in)
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
    getStagedOutput(progStaged)(_.dotWithAbstractElem, "patternMatchAbstract", in)
  }

  test("elems divergence in if_then_else branches") {
    // Different branches of if_then_else operator produce different elems.
    // This causes Sums and SumViews to appear.
    // The test verifies iso lifting in this case (see IfThenElse.rewriteDef)
    val matrix = Array(Array(0, 5), Array(1, 3), Array(1, 4))
    val vector = Array(1,2)

    val in = Tuple(matrix, vector)
    progStaged.buildGraph(file(prefix, "simpleSum"), "simpleSum", progStaged.scalan.funSimpleSum, graphVizConfig)(progStaged.defaultCompilerConfig)
  }

}

class VectorMethodsItTests extends LmsLinAlgItTests {

  import progSeq._

  lazy val vector1 = Array(Pair(0, 1.0), Pair(1, 2.0), Pair(2, 3.0), Pair(3, 4.0), Pair(4, 5.0))
  lazy val vector2 = Array(Pair(0, 1.0), Pair(7, 3.0), Pair(12, 5.0))

  test("applySparseVector1") {

    //val progStaged = new ProgExp

    lazy val len = 5
    val i = 2
    val in = Tuple(vector1, len, i)
    val out = 3
    compareOutputWithSequential(progStaged, progSeq)(_.applySparseVector, "applySparseVector1", in)
    compareOutputWithExpected(progStaged)(out, _.applySparseVector, "applySparseVector1e", in)
  }

  test("applySparseVector2") {

    //val progStaged = new ProgExp

    lazy val len = 12
    val i = 12
    val in = Tuple(vector2, len, i)
    val out = 5
    compareOutputWithSequential(progStaged, progSeq)(_.applySparseVector, "applySparseVector2", in)
    compareOutputWithExpected(progStaged)(out, _.applySparseVector, "applySparseVector2e", in)
  }

  test("transpose") {
    val nItems = 2
    val (arrTrain, segsTrain) = progSeq.getNArrayWithSegmentsFromJaggedArray(progSeq.jArrTrain2x2)
    val in = progSeq.Tuple(arrTrain, segsTrain, nItems)

    compareOutputWithSequential(progStaged, progSeq)(_.transpose, "transpose", in)
  }

  // the below two tests are ignored because they can fail due to randomness
  // we could also just decrease the chance of this significantly
  ignore("random") {
    val c = 1.0
    val in = c

    compareOutputWithSequential(progStaged, progSeq)(_.funRandom.asRep[Any => Any], "random", in)
  }

  ignore("randomArray") {
    val c = 1000
    val in = c

    compareOutputWithSequential(progStaged, progSeq)(_.funRandomArray, "randomArray", in)
  }

  test("ZipMapViewBoth") {
    val c = 10
    val in = c

    compareOutputWithSequential(progStaged, progSeq)(_.funZipMapViewBoth, "ZipMapViewBoth", in)
  }

  test("ZipMapViewLeft") {
    val c = 10
    val in = c

    compareOutputWithSequential(progStaged, progSeq)(_.funZipMapViewLeft, "funZipMapViewLeft", in)
  }

  test("ZipMapViewRight") {
    val c = 10
    val in = c

    compareOutputWithSequential(progStaged, progSeq)(_.funZipMapViewRight, "funZipMapViewRight", in)
  }

  test("collReplicateFilter") {
    val in = Array(1, 2, 4)
    compareOutputWithSequential(progStaged, progSeq)(_.collReplicateFilter, "collReplicateFilter", in)
  }

}
