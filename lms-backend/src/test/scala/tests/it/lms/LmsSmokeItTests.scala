package tests.it.lms

import tests.it.smoke.SmokeItTests
import scalan.codegen.lms.LmsBackend
import scalan.arrays.PArraysDslExp
import scalan.ScalanCtxStaged

class LmsSmokeItTests extends SmokeItTests {
  class ProgStaged extends Prog with PArraysDslExp with ScalanCtxStaged with LmsBackend
  
  val progStaged = new ProgStaged()
  
  import progSeq._
  
  test("test00simpleConst") {
    val (in, out) = Array(0) -> Array(1)
    progSeq.simpleConst(progSeq.PArray.fromArray(in)).arr should be(out)
    checkRun(progSeq, progStaged)(progSeq.simpleConst, progStaged.simpleConst)("00simpleConst", progSeq.PArray.fromArray(in), progSeq.PArray.fromArray(out))
  }
}