package scalan.linalgebra

import scalan._
import scalan.compilation.lms.linalgebra.{LinAlgLmsCompilerUni, LinAlgLmsCompilerScala}
import scalan.compilation.lms.scalac.LmsCompilerScalaConfig
import scalan.compilation.lms.source2bin.SbtConfig
import scalan.compilation.lms.uni._
import scalan.it.BaseItTests

abstract class LmsLinAlgItTests extends BaseItTests[LinearAlgebraExamples](new MatricesDslStd with LinearAlgebraExamples) {
  class ProgExp extends MatricesDslExp with JNIExtractorOpsExp with LinearAlgebraExamples

  val progStaged = new LinAlgLmsCompilerScala(new ProgExp)

  val progStagedU = new LinAlgLmsCompilerUni(new ProgExp)

  val compilerConfigU = LmsCompilerScalaConfig().withSbtConfig(SbtConfig(scalaVersion = "2.11.2"))
  
  def sparseVectorData(arr: Array[Double]) = (arr.indices.toArray, (arr, arr.length))

  val defaultCompilers = compilers(progStaged, cwc(progStagedU)(compilerConfigU))
  val progStagedOnly = compilers(progStaged)
}

class LmsMvmItTests extends LmsLinAlgItTests {
  import progStd._

  test("ddmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    compareOutputWithStd(_.ddmvm)(Pair(inM, inV))
  }

  test("ddmvmList") {
    val inM = List(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    // TODO List support in LmsCompilerUni
    compareOutputWithStd(_.ddmvmList, progStagedOnly)(Pair(inM, inV))
  }

  test("dsmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = sparseVectorData(Array(2.0, 3.0))
    compareOutputWithStd(_.dsmvm)(Pair(inM, inV))
  }

  test("sdmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = Array(2.0, 3.0)
    compareOutputWithStd(_.sdmvm)(Pair(inM, inV))
  }

  test("ssmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = sparseVectorData(Array(2.0, 3.0))
    compareOutputWithStd(_.ssmvm)(Pair(inM, inV))
  }

  test("fdmvm") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = Array(2.0, 3.0)
    compareOutputWithStd(_.fdmvm, progStagedOnly)(Pair(inM, inV))
  }

  test("fsmvm") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = sparseVectorData(Array(2.0, 3.0))
    compareOutputWithStd(_.fsmvm)(Pair(inM, inV))
  }

  test("cdmvm") {
    val inM = (3.0, (2, 2))
    val inV = Array(2.0, 3.0)
    compareOutputWithStd(_.cdmvm, progStagedOnly)(Pair(inM, inV))
  }

  test("csmvm") {
    val inM = (3.0, (2, 2))
    val inV = sparseVectorData(Array(2.0, 3.0))
    compareOutputWithStd(_.csmvm, progStagedOnly)(Pair(inM, inV))
  }

  test("dcmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = (2.0, 2)
    compareOutputWithStd(_.dcmvm, progStagedOnly)(Pair(inM, inV))
  }

  test("ccmvm") {
    val inM = (3.0, (2, 2))
    val inV = (2.0, 2)
    compareOutputWithStd(_.ccmvm, progStagedOnly)(Pair(inM, inV))
  }

  test("dgdmvm") {
    val inM = Array(1.0, 2.0, 3.0)
    val inV = Array(2.0, 4.0, 6.0)
    compareOutputWithStd(_.dgdmvm, progStagedOnly)(Pair(inM, inV))
  }

  test("dgsmvm") {
    val inM = Array(1.0, 2.0, 3.0)
    val inV = sparseVectorData(Array(2.0, 4.0, 6.0))
    compareOutputWithStd(_.dgsmvm, progStagedOnly)(Pair(inM, inV))
  }

  test("dgcmvm") {
    val inM = Array(1.0, 2.0, 3.0)
    val inV = (2.0, 3)
    compareOutputWithStd(_.dgcmvm, progStagedOnly)(Pair(inM, inV))
  }

  test("ddmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    compareOutputWithStd(_.ddmvm0)(Pair(inM, inV))
  }

  test("dsmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = sparseVectorData(Array(2.0, 3.0))
    compareOutputWithStd(_.dsmvm0)(Pair(inM, inV))
  }

  test("sdmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = Array(2.0, 3.0)
    compareOutputWithStd(_.sdmvm0)(Pair(inM, inV))
  }

  test("ssmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = sparseVectorData(Array(2.0, 3.0))
    compareOutputWithStd(_.ssmvm0)(Pair(inM, inV))
  }

  test("fdmvm0") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = Array(2.0, 3.0)
    compareOutputWithStd(_.fdmvm0)(Pair(inM, inV))
  }

  test("fsmvm0") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = sparseVectorData(Array(2.0, 3.0))
    compareOutputWithStd(_.fsmvm0)(Pair(inM, inV))
  }
}

class LmsMmmItTests extends LmsLinAlgItTests {
  import progStd._

  test("ddmmm") {
    val inM1 = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inM2 = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    compareOutputWithStd(_.ddmmm)(Pair(inM1, inM2))
  }

  test("ssmmm") {
    //pending
    val inM1 = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inM2 = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    compareOutputWithStd(_.ssmmm)(Pair(inM1, inM2))
  }

  test("ffmmm") {
    val inM1 = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inM2 = (Array(1.0, 1.0, 0.0, 1.0), 2)
    compareOutputWithStd(_.ffmmm)(Pair(inM1, inM2))
  }

  test("ccmmm") {
    val inM1 = (3.0, (2, 2))
    val inM2 = (2.0, (2, 2))
    compareOutputWithStd(_.ccmmm, progStagedOnly)(Pair(inM1, inM2))
  }

  test("cfmmm") {
    val inM1 = (3.0, (2, 2))
    val inM2 = (Array(1.0, 1.0, 0.0, 1.0), 2)
    compareOutputWithStd(_.cfmmm, progStagedOnly)(Pair(inM1, inM2))
  }

  test("dgfmmm") {
    val inM1 = Array(2.0, 3.0)
    val inM2 = (Array(1.0, 1.0, 0.0, 1.0), 2)
    compareOutputWithStd(_.dgfmmm, progStagedOnly)(Pair(inM1, inM2))
  }

  test("dgdgmmm") {
    val inM1 = Array(2.0, 3.0)
    val inM2 = Array(2.0, 3.0)
    compareOutputWithStd(_.dgdgmmm, progStagedOnly)(Pair(inM1, inM2))
  }

}

class AbstractElemItTests extends LmsLinAlgItTests {
  import progStd._

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

    getStagedOutput(_.dotWithAbstractElem, progStagedOnly)(Tuple(width, arrTrain))
  }

  test("elems divergence in if_then_else branches") {
    // Different branches of if_then_else operator produce different elems.
    // This causes Sums and SumViews to appear.
    // The test verifies iso lifting in this case (see IfThenElse.rewriteDef)
    buildGraphs(_.funSimpleSum, progStagedOnly)
  }

}

class VectorMethodsItTests extends LmsLinAlgItTests {

  import progStd._

  lazy val vector1 = Array(Pair(0, 1.0), Pair(1, 2.0), Pair(2, 3.0), Pair(3, 4.0), Pair(4, 5.0))
  lazy val vector2 = Array(Pair(0, 1.0), Pair(7, 3.0), Pair(12, 5.0))

  test("applySparseVector1") {
    val len = 5
    val i = 2
    compareOutputWithStd(_.applySparseVector, progStagedOnly)(Tuple(vector1, len, i))
  }

  test("applySparseVector2") {
    val len = 12
    val i = 12
    compareOutputWithStd(_.applySparseVector, progStagedOnly)(Tuple(vector2, len, i))
  }

  test("transpose") {
    val nItems = 2
    val (arrTrain, segsTrain) = progStd.getNArrayWithSegmentsFromJaggedArray(progStd.jArrTrain2x2)

    compareOutputWithStd(_.transpose, progStagedOnly)(Tuple(arrTrain, segsTrain, nItems))
  }

  // the below two tests are ignored because they can fail due to randomness
  // we could also just decrease the chance of this significantly
  ignore("random") {
    compareOutputWithStd(_.funRandom.asRep[Any => Any], progStagedOnly)(1.0)
  }

  ignore("randomArray") {
    compareOutputWithStd(_.funRandomArray, progStagedOnly)(1000)
  }

  test("ZipMapViewBoth") {
    compareOutputWithStd(_.funZipMapViewBoth, progStagedOnly)(10)
  }

  test("ZipMapViewLeft") {
    compareOutputWithStd(_.funZipMapViewLeft, progStagedOnly)(10)
  }

  test("ZipMapViewRight") {
    compareOutputWithStd(_.funZipMapViewRight, progStagedOnly)(10)
  }

  test("collReplicateFilter") {
    compareOutputWithStd(_.collReplicateFilter, progStagedOnly)(Array(1, 2, 4))
  }

  test("sumConvertSum") {
    compareOutputWithStd(_.sumConvertSum, progStagedOnly)(Array(1.0, 2.0, 4.0))
  }
}

class MatrixMethodsItTests extends LmsLinAlgItTests {
  import progStd._

  val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
  val inV = Array(2.0, 3.0)

  test("dmUpdate") {
    val in = (inM, (inV, true))
    compareOutputWithSequential(_.dmUpdate, progStagedOnly)(in)
  }
}
