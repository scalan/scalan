package scalan.compilation.lms.cxx.sharedptr

import scalan.compilation.GraphVizExport
import scalan.compilation.lms.CoreBridge
import scalan.compilation.lms.cxx.LmsCompilerCxx
import scalan.it.BaseItTests
import scalan.it.lms.ItTestsUtilLmsCxx
import scalan.linalgebra.{LinearAlgebraExamples, MatricesDslSeq}
import scalan.{ScalanCommunityDslExp, ScalanCommunityDslSeq, ScalanCommunityExp}

class CxxShptrLmsLinAlgItTests extends BaseItTests with ItTestsUtilLmsCxx {
  class ProgExp extends LinearAlgebraExamples with ScalanCommunityDslExp with ScalanCommunityExp with LmsCompilerCxx with CoreBridge with GraphVizExport { self =>
    val lms = new CoreCxxShptrLmsBackend
  }
  class ProgSeq extends LinearAlgebraExamples with MatricesDslSeq with ScalanCommunityDslSeq
  
  val progStaged = new ProgExp
  val progSeq = new ProgSeq
  
  def sparseVectorData(arr: Array[Double]) = (0.until(arr.length).toArray, (arr, arr.length))

  implicit val compilerConfig = progStaged.defaultCompilerConfig
}

class CxxShptrLmsMvmItTests extends CxxShptrLmsLinAlgItTests {
  import progSeq._

  test("ddmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    generate(progStaged)(progStaged.ddmvm, "ddmvm")
//    compareOutputWithSequential(progStaged)(progSeq.ddmvm, progStaged.ddmvm, "ddmvm", in)
  }

  test("dsmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    generate(progStaged)(progStaged.dsmvm, "dsmvm")
//    compareOutputWithSequential(progStaged)(progSeq.dsmvm, progStaged.dsmvm, "dsmvm", in)
  }

  test("sdmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    generate(progStaged)(progStaged.sdmvm, "sdmvm")
//    compareOutputWithSequential(progStaged)(progSeq.sdmvm, progStaged.sdmvm, "sdmvm", in)
  }

  test("ssmvm") {
    pending
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    generate(progStaged)(progStaged.ssmvm, "ssmvm")
//    compareOutputWithSequential(progStaged)(progSeq.ssmvm, progStaged.ssmvm, "ssmvm", in)
  }

  test("fdmvm") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    generate(progStaged)(progStaged.fdmvm, "fdmvm")
//    compareOutputWithSequential(progStaged)(progSeq.fdmvm, progStaged.fdmvm, "fdmvm", in)
  }

  test("fsmvm") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    generate(progStaged)(progStaged.fsmvm, "fsmvm")
//    compareOutputWithSequential(progStaged)(progSeq.fsmvm, progStaged.fsmvm, "fsmvm", in)
  }

  test("ddmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    generate(progStaged)(progStaged.ddmvm0, "ddmvm0")
//    compareOutputWithSequential(progStaged)(progSeq.ddmvm0, progStaged.ddmvm0, "ddmvm0", in)
  }

  test("dsmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    generate(progStaged)(progStaged.dsmvm0, "dsmvm0")
//    compareOutputWithSequential(progStaged)(progSeq.dsmvm0, progStaged.dsmvm0, "dsmvm0", in)
  }

  test("sdmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    generate(progStaged)(progStaged.sdmvm0, "sdmvm0")
//    compareOutputWithSequential(progStaged)(progSeq.sdmvm0, progStaged.sdmvm0, "sdmvm0", in)
  }

  test("ssmvm0") {
    pending
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    generate(progStaged)(progStaged.ssmvm0, "ssmvm0")
//    compareOutputWithSequential(progStaged)(progSeq.ssmvm0, progStaged.ssmvm0, "ssmvm0", in)
  }

  test("fdmvm0") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    generate(progStaged)(progStaged.fdmvm0, "fdmvm0")
//    compareOutputWithSequential(progStaged)(progSeq.fdmvm0, progStaged.fdmvm0, "fdmvm0", in)
  }

  test("fsmvm0") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    generate(progStaged)(progStaged.fsmvm0, "fsmvm0")
//    compareOutputWithSequential(progStaged)(progSeq.fsmvm0, progStaged.fsmvm0, "fsmvm0", in)
  }
}

class CxxShptrLmsMmmItTests extends CxxShptrLmsLinAlgItTests {
  import progSeq._

  test("ddmmm") {
    val inM1 = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inM2 = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val in = Pair(inM1, inM2)
    val out = Array(Array(1.0, 2.0), Array(0.0, 1.0))
    generate(progStaged)(progStaged.ddmmm, "ddmmm")
//    compareOutputWithSequential(progStaged)(progSeq.ddmmm, progStaged.ddmmm, "ddmmm", in)
  }

  test("ssmmm") {
    pending
    val inM1 = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inM2 = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val in = Pair(inM1, inM2)
    val out = Array(Array(1.0, 2.0), Array(0.0, 1.0))
    generate(progStaged)(progStaged.ssmmm, "ssmmm")
//    compareOutputWithSequential(progStaged)(progSeq.ssmmm, progStaged.ssmmm, "ssmmm", in)
  }

  test("ffmmm") {
    val inM1 = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inM2 = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val in = Pair(inM1, inM2)
    val out = Array(Array(1.0, 2.0), Array(0.0, 1.0))
    generate(progStaged)(progStaged.ffmmm, "ffmmm")
//    compareOutputWithSequential(progStaged)(progSeq.ffmmm, progStaged.ffmmm, "ffmmm", in)
  }
}
