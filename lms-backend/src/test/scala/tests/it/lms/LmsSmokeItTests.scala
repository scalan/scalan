package tests.it.lms

import scalan.codegen.lms.LmsBackend
import scalan.arrays.PArraysDslExp
import scalan.codegen.GraphVizExport
import scalan.linalgebra.VectorsDslExp
import scalan.community.{ScalanCommunitySeq, ScalanCommunityExp}
import tests.scalan.CommunitySmokeItTests

class LmsSmokeItTests extends CommunitySmokeItTests {
  import scala.Array

  class ProgExp extends ProgCommunity with PArraysDslExp with ScalanCommunityExp with GraphVizExport with LmsBackend with VectorsDslExp
  
  override val progStaged = new ProgExp() {
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
  test("test3simpleMapNested") {
    val (in, out) = (Array(Array(2.0,3.0), Array(3.0,4.0)), 1) -> Array(3.0,4.0)
    progSeq.simpleMapNested(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.simpleMapNested, progStaged.simpleMapNested)("simpleMapNested", in)
  }
  test("test4simpleZip") {
    val (in, out) = Array(2,3) -> Array((4,2), (5,3))
    progSeq.simpleZip(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.simpleZip, progStaged.simpleZip)("simpleZip", in)
  }
  test("test5simpleZipWith") {
    val (in, out) = Array(2,3) -> Array(10,18)
    progSeq.simpleZipWith(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.simpleZipWith, progStaged.simpleZipWith)("simpleZipWith", in)
  }
  test("test6simpleReduce") {
    val (in, out) = Array(2,3) -> 5
    progSeq.simpleReduce(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.simpleReduce, progStaged.simpleReduce)("simpleReduce", in)
  }
  test("test7mvMul") {
    val (in, out) = (Array(Array(2,3), Array(4,5)), Array(6,7))  -> Array(33,59)
    progSeq.mvMul(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.mvMul, progStaged.mvMul)("mvMul", in)
  }
  test("test8expBaseArrays") {
    val (in, out) = Array(Array(2,3), Array(4,5)) ->  Array(Array(2,3), Array(4,5))
    progSeq.expBaseArrays(in) should be(out)
    lmsTestRun(progSeq, progStaged)(progSeq.expBaseArrays, progStaged.expBaseArrays)("expBaseArrays", in)
  }
}