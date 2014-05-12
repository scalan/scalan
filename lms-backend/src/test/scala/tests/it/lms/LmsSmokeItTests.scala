package tests.it.lms

import _root_.tests.it.smoke.SmokeItTests
import scalan.codegen.lms.LmsBackend
import scalan.arrays.PArraysDslExp
import scalan.ScalanCtxStaged
import scalan.codegen.GraphVizExport
import scalan.linalgebra.VectorsDslExp

class LmsSmokeItTests extends SmokeItTests {
  // TODO remove VectorsDslExp!
  class ProgStaged extends Prog with PArraysDslExp with ScalanCtxStaged with GraphVizExport with LmsBackend with VectorsDslExp
  
  val progStaged = new ProgStaged() {
    this.invokeEnabled = true
  }
  
  import progSeq._

  test("test0simpleArith") {
    val (in, out) = 2 -> 6
    progSeq.simpleArith(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.simpleArith, progStaged.simpleArith)("simpleArith", in)
  }
  test("test1simpleArrGet") {
    val (in, out) = (Array(2,3),1) -> 3
    progSeq.simpleArrGet(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.simpleArrGet, progStaged.simpleArrGet)("simpleArrGet", in)
  }
  test("test2simpleMap") {
    val (in, out) = Array(2,3) -> Array(3,4)
    progSeq.simpleMap(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.simpleMap, progStaged.simpleMap)("simpleMap", in)
  }
  test("test3simpleZip") {
    val (in, out) = Array(2,3) -> Array((4,2), (5,3))
    progSeq.simpleZip(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.simpleZip, progStaged.simpleZip)("simpleZip", in)
  }
  test("test4simpleZipWith") {
    val (in, out) = Array(2,3) -> Array(10,18)
    progSeq.simpleZipWith(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.simpleZipWith, progStaged.simpleZipWith)("simpleZipWith", in)
  }
  test("test5simpleReduce") {
    val (in, out) = Array(2,3) -> 5
    progSeq.simpleReduce(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.simpleReduce, progStaged.simpleReduce)("simpleReduce", in)
  }
  test("test6mvMul") {
    val (in, out) = (Array(Array(2,3), Array(4,5)), Array(6,7))  -> Array(33,59)
    progSeq.mvMul(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.mvMul, progStaged.mvMul)("mvMul", in)
  }
  test("test7expBaseArrays") {
    val (in, out) = Array(Array(2,3), Array(4,5)) ->  Array(Array(2,3), Array(4,5))
    progSeq.expBaseArrays(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.expBaseArrays, progStaged.expBaseArrays)("expBaseArrays", in)
  }
}