package tests.it.lms

import _root_.tests.it.ItTests
import _root_.tests.scalan.linalgebra.LinearAlgebraExamples
import scalan.codegen.lms.LmsBackend
import scalan.ScalanCtxStaged
import scalan.codegen.GraphVizExport
import scalan.linalgebra.MatricesDslExp
import scalan.ScalanCtxSeq
import scalan.linalgebra.MatricesDslSeq
import scalan.codegen.lms.MyBridge

class LmsLinAlgItTests extends ItTests {
  class ProgStaged extends LinearAlgebraExamples with MatricesDslExp with ScalanCtxStaged with GraphVizExport with LmsBackend { self =>
//    override def makeBridge[A, B] = new MyBridge[A, B] {
//      override val scalan = self
//      
//      override def 
//    }
  }
  
  class ProgSeq extends LinearAlgebraExamples with MatricesDslSeq with ScalanCtxSeq
  
  val progStaged = new ProgStaged() {
    this.invokeEnabled = true
  }
  val progSeq = new ProgSeq
  
  def sparseVectorData(arr: Array[Double]) = ((0.until(arr.length)).toArray, (arr, arr.length))
}

class LmsMvmItTests extends LmsLinAlgItTests {
  import progSeq._
  import scala.Array

  test("ddmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    progSeq.ddmvm(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.ddmvm, progStaged.ddmvm)("ddmvm", in)
  }

  test("dsmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    progSeq.dsmvm(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.dsmvm, progStaged.dsmvm)("dsmvm", in)
  }

  test("sdmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    progSeq.sdmvm(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.sdmvm, progStaged.sdmvm)("sdmvm", in)
  }

  test("ssmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    progSeq.ssmvm(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.ssmvm, progStaged.ssmvm)("ssmvm", in)
  }

  test("fdmvm") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    progSeq.fdmvm(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.fdmvm, progStaged.fdmvm)("fdmvm", in)
  }

  test("fsmvm") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    progSeq.fsmvm(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.fsmvm, progStaged.fsmvm)("fsmvm", in)
  }

  test("ddmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    progSeq.ddmvm0(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.ddmvm0, progStaged.ddmvm0)("ddmvm0", in)
  }

  test("dsmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    progSeq.dsmvm0(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.dsmvm0, progStaged.dsmvm0)("dsmvm0", in)
  }

  test("sdmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    progSeq.sdmvm0(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.sdmvm0, progStaged.sdmvm0)("sdmvm0", in)
  }

  test("ssmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    progSeq.ssmvm0(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.ssmvm0, progStaged.ssmvm0)("ssmvm0", in)
  }

  test("fdmvm0") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    progSeq.fdmvm0(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.fdmvm0, progStaged.fdmvm0)("fdmvm0", in)
  }

  test("fsmvm0") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    progSeq.fsmvm0(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.fsmvm0, progStaged.fsmvm0)("fsmvm0", in)
  }  
}

class LmsMmmItTests extends LmsLinAlgItTests {
  import progSeq._
  import scala.Array

  test("ddmmm") {
    pending
    val inM1 = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inM2 = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val in = Pair(inM1, inM2)
    val out = Array(Array(1.0, 2.0), Array(0.0, 1.0))
    progSeq.ddmmm(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.ddmmm, progStaged.ddmmm)("ddmmm", in)
  }

  test("ssmmm") {
    pending
    val inM1 = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inM2 = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val in = Pair(inM1, inM2)
    val out = Array(Array(1.0, 2.0), Array(0.0, 1.0))
    progSeq.ssmmm(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.ssmmm, progStaged.ssmmm)("ssmmm", in)
  }

  test("ffmmm") {
    val inM1 = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inM2 = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val in = Pair(inM1, inM2)
    val out = Array(Array(1.0, 2.0), Array(0.0, 1.0))
    // FIXME progSeq.ffmmm(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.ffmmm, progStaged.ffmmm)("ffmmm", in)
  }
}