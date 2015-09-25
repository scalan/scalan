package scalan.it.lms

import scalan._
import scalan.util.FileUtil._
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.{LmsCompilerScalaConfig, CommunityLmsCompilerScala}
import scalan.compilation.lms.source2bin.SbtConfig
import scalan.compilation.lms.uni._
import scalan.it.BaseItTests
import scalan.linalgebra.{LinearAlgebraExamples, MatricesDslSeq}

abstract class LmsLinAlgItTests extends BaseItTests {
  trait Prog extends ScalanCommunityDsl with LinearAlgebraExamples

  class ProgExp extends ScalanCommunityDslExp with JNIExtractorOpsExp with Prog

  class ProgSeq extends ScalanCommunityDslSeq with Prog

  val progStaged = new CommunityLmsCompilerScala(new ProgExp) with CommunityBridge

  val progStagedU = new LmsCompilerUni(new ProgExp) with CommunityBridge with CommunityMethodMappingDSL

  val progSeq = new ProgSeq

  val compilerConfigU = LmsCompilerScalaConfig().withSbtConfig(SbtConfig(scalaVersion = "2.11.2"))
  
  def sparseVectorData(arr: Array[Double]) = (0.until(arr.length).toArray, (arr, arr.length))

  val defaultCompilers = compilers(progStaged, cwc(progStagedU)(compilerConfigU))
  val progStagedOnly = compilers(progStaged)
}

class LmsMvmItTests extends LmsLinAlgItTests {
  import progSeq._

  test("ddmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(_.ddmvm, "ddmvm")(in)
  }

  test("ddmvmList(scala)") {
    val inM = List(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(_.ddmvmList, "ddmvmList", progStagedOnly)(in)
  }

  test("dsmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(_.dsmvm, "dsmvm")(in)
  }

  test("sdmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(_.sdmvm, "sdmvm")(in)
  }

  test("ssmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(_.ssmvm, "ssmvm")(in)
  }

  test("fdmvm") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(_.fdmvm, "fdmvm", progStagedOnly)(in)
  }

  test("fsmvm") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(_.fsmvm, "fsmvm")(in)
  }

  test("cdmvm") {
    val inM = (3.0, (2, 2))
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(15.0, 15.0)
    compareOutputWithSequential(_.cdmvm, "cdmvm", progStagedOnly)(in)
  }

  test("csmvm") {
    val inM = (3.0, (2, 2))
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(15.0, 15.0)
    compareOutputWithSequential(_.csmvm, "csmvm", progStagedOnly)(in)
  }

  test("dcmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = (2.0, 2)
    val in = Pair(inM, inV)
    val out = Array(4.0, 2.0)
    compareOutputWithSequential(_.dcmvm, "dcmvm", progStagedOnly)(in)
  }

  test("ccmvm") {
    val inM = (3.0, (2, 2))
    val inV = (2.0, 2)
    val in = Pair(inM, inV)
    val out = Array(12.0, 12.0)
    compareOutputWithSequential(_.ccmvm, "ccmvm", progStagedOnly)(in)
  }

  test("dgdmvm") {
    val inM = Array(1.0, 2.0, 3.0)
    val inV = Array(2.0, 4.0, 6.0)
    val in = Pair(inM, inV)
    val out = Array(2.0, 8.0, 18.0)
    compareOutputWithSequential(_.dgdmvm, "dgdmvm", progStagedOnly)(in)
  }

  test("dgsmvm") {
    val inM = Array(1.0, 2.0, 3.0)
    val inV = sparseVectorData(Array(2.0, 4.0, 6.0))
    val in = Pair(inM, inV)
    val out = Array(2.0, 8.0, 18.0)
    compareOutputWithSequential(_.dgsmvm, "dgsmvm", progStagedOnly)(in)
  }

  test("dgcmvm") {
    val inM = Array(1.0, 2.0, 3.0)
    val inV = (2.0, 3)
    val in = Pair(inM, inV)
    val out = Array(2.0, 4.0, 6.0)
    compareOutputWithSequential(_.dgcmvm, "dgcmvm", progStagedOnly)(in)
  }

  test("ddmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(_.ddmvm0, "ddmvm0")(in)
  }

  test("dsmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(_.dsmvm0, "dsmvm0")(in)
  }

  test("sdmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(_.sdmvm0, "sdmvm0")(in)
  }

  test("ssmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(_.ssmvm0, "ssmvm0")(in)
  }

  test("fdmvm0") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(_.fdmvm0, "fdmvm0")(in)
  }

  test("fsmvm0") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(_.fsmvm0, "fsmvm0")(in)
  }
}

class LmsMmmItTests extends LmsLinAlgItTests {
  import progSeq._

  test("ddmmm") {
    val inM1 = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inM2 = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val in = Pair(inM1, inM2)
    val out = Array(Array(1.0, 2.0), Array(0.0, 1.0))
    compareOutputWithSequential(_.ddmmm, "ddmmm")(in)
  }

  test("ssmmm") {
    //pending
    val inM1 = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inM2 = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val in = Pair(inM1, inM2)
    val out = Array(Array(1.0, 2.0), Array(0.0, 1.0))
    compareOutputWithSequential(_.ssmmm, "ssmmm")(in)
  }

  test("ffmmm") {
    val inM1 = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inM2 = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val in = Pair(inM1, inM2)
    val out = Array(Array(1.0, 2.0), Array(0.0, 1.0))
    compareOutputWithSequential(_.ffmmm, "ffmmm")(in)
  }

  test("ccmmm") {
    val inM1 = (3.0, (2, 2))
    val inM2 = (2.0, (2, 2))
    val in = Pair(inM1, inM2)
    val out = Array(Array(12.0, 12.0), Array(12.0, 12.0))
    compareOutputWithSequential(_.ccmmm, "ccmmm", progStagedOnly)(in)
  }

  test("cfmmm") {
    val inM1 = (3.0, (2, 2))
    val inM2 = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val in = Pair(inM1, inM2)
    val out = Array(Array(3.0, 6.0), Array(3.0, 6.0))
    compareOutputWithSequential(_.cfmmm, "cfmmm", progStagedOnly)(in)
  }

  test("dgfmmm") {
    val inM1 = Array(2.0, 3.0)
    val inM2 = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val in = Pair(inM1, inM2)
    val out = Array(Array(2.0, 2.0), Array(0.0, 3.0))
    compareOutputWithSequential(_.dgfmmm, "dgfmmm", progStagedOnly)(in)
  }

  test("dgdgmmm") {
    val inM1 = Array(2.0, 3.0)
    val inM2 = Array(2.0, 3.0)
    val in = Pair(inM1, inM2)
    val out = Array(Array(4.0, 0.0), Array(0.0, 9.0))
    compareOutputWithSequential(_.dgdgmmm, "dgdgmmm", progStagedOnly)(in)
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
    getStagedOutput(_.dotWithAbstractElem, "patternMatchAbstract", progStagedOnly)(in)
  }

  test("elems divergence in if_then_else branches") {
    // Different branches of if_then_else operator produce different elems.
    // This causes Sums and SumViews to appear.
    // The test verifies iso lifting in this case (see IfThenElse.rewriteDef)
    val matrix = Array(Array(0, 5), Array(1, 3), Array(1, 4))
    val vector = Array(1,2)

    val in = Tuple(matrix, vector)
    progStaged.buildGraph(file(prefix, "simpleSum"), "simpleSum", progStaged.scalan.funSimpleSum, defaultGraphVizConfig)(progStaged.defaultCompilerConfig)
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
    compareOutputWithSequential(_.applySparseVector, "applySparseVector1e", progStagedOnly)(in)
  }

  test("applySparseVector2") {

    //val progStaged = new ProgExp

    lazy val len = 12
    val i = 12
    val in = Tuple(vector2, len, i)
    val out = 5
    compareOutputWithSequential(_.applySparseVector, "applySparseVector2", progStagedOnly)(in)
  }

  test("transpose") {
    val nItems = 2
    val (arrTrain, segsTrain) = progSeq.getNArrayWithSegmentsFromJaggedArray(progSeq.jArrTrain2x2)
    val in = progSeq.Tuple(arrTrain, segsTrain, nItems)

    compareOutputWithSequential(_.transpose, "transpose", progStagedOnly)(in)
  }

  // the below two tests are ignored because they can fail due to randomness
  // we could also just decrease the chance of this significantly
  ignore("random") {
    val c = 1.0
    val in = c

    compareOutputWithSequential(_.funRandom.asRep[Any => Any], "random", progStagedOnly)(in)
  }

  ignore("randomArray") {
    val c = 1000
    val in = c

    compareOutputWithSequential(_.funRandomArray, "randomArray", progStagedOnly)(in)
  }

  test("ZipMapViewBoth") {
    val c = 10
    val in = c

    compareOutputWithSequential(_.funZipMapViewBoth, "ZipMapViewBoth", progStagedOnly)(in)
  }

  test("ZipMapViewLeft") {
    val c = 10
    val in = c

    compareOutputWithSequential(_.funZipMapViewLeft, "funZipMapViewLeft", progStagedOnly)(in)
  }

  test("ZipMapViewRight") {
    val c = 10
    val in = c

    compareOutputWithSequential(_.funZipMapViewRight, "funZipMapViewRight", progStagedOnly)(in)
  }

  test("collReplicateFilter") {
    val in = Array(1, 2, 4)
    compareOutputWithSequential(_.collReplicateFilter, "collReplicateFilter", progStagedOnly)(in)
  }

}
