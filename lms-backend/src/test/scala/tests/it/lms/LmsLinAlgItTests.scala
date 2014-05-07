package tests.it.lms

import _root_.tests.it.ItTests
import _root_.tests.scalan.linalgebra.LinearAlgebraExamples
import scalan.codegen.lms.LmsBackend
import scalan.ScalanCtxStaged
import scalan.codegen.GraphVizExport
import scalan.linalgebra.MatricesDslExp
import scalan.ScalanCtxSeq
import scalan.linalgebra.MatricesDslSeq

class LmsLinAlgItTests extends ItTests {
  class ProgStaged extends LinearAlgebraExamples with MatricesDslExp with ScalanCtxStaged with GraphVizExport with LmsBackend
  class ProgSeq extends LinearAlgebraExamples with MatricesDslSeq with ScalanCtxSeq
  
  val progStaged = new ProgStaged() {
    this.invokeEnabled = true
  }
  val progSeq = new ProgSeq
  
  import progSeq._

  test("ddmvm") {
    val inM = Array(Array(1, 1), Array(0, 1))
    val inV = Array(2, 3)
    val in = Pair(inM, inV)
    val out = Array(5, 3)
    progSeq.ddmvm(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.ddmvm, progStaged.ddmvm)("ddmvm", in)
  }

  test("dsmvm") {
    val inM = Array(Array(1, 1), Array(0, 1))
    val inV = Array(2, 3)
    val in = Pair(inM, inV)
    val out = Array(5, 3)
    progSeq.dsmvm(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.dsmvm, progStaged.dsmvm)("dsmvm", in)
  }

  test("sdmvm") {
    val inM = Array(Array(1, 1), Array(0, 1))
    val inV = Array(2, 3)
    val in = Pair(inM, inV)
    val out = Array(5, 3)
    progSeq.sdmvm(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.sdmvm, progStaged.sdmvm)("sdmvm", in)
  }

  test("ssmvm") {
    val inM = Array(Array(1, 1), Array(0, 1))
    val inV = Array(2, 3)
    val in = Pair(inM, inV)
    val out = Array(5, 3)
    progSeq.ssmvm(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.ssmvm, progStaged.ssmvm)("ssmvm", in)
  }
}