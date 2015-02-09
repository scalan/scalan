package scalan.it.lms

import java.io.File

import scalan.ScalanCtxExp
import scalan.community.ScalanCommunityExp
import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.compilation.lms.CoreBridge
import scalan.compilation.lms.cxx.{LmsCompilerCXX, CoreCXXLmsBackend}
import scalan.it.smoke.SmokeItTests

class CXXLmsSmokeItTests extends SmokeItTests with ItTestsUtilLmsCxx {
  class ProgExp extends Prog with ScalanCtxExp with ScalanCommunityExp with GraphVizExport with LmsCompilerCXX { self =>
    def makeBridge[A, B] = new CoreBridge[A, B] {
      val scalan = self
      val lms = new CoreCXXLmsBackend
    }
  }
  
  override val progStaged = new ProgExp
  import progStaged.defaultCompilerConfig

  test("test0simpleArith") {
    val in = 2
    generate(progStaged)(progStaged.simpleArith, "simpleArith")
//    compareOutputWithSequential(progStaged)(progSeq.simpleArith, progStaged.simpleArith, "simpleArith", in)
  }
  test("test1simpleArrGet") {
    val in = (Array(2,3), 1)
    generate(progStaged)(progStaged.simpleArrGet, "simpleArrGet" )
//    compareOutputWithSequential(progStaged)(progSeq.simpleArrGet, progStaged.simpleArrGet, "simpleArrGet", in)
  }
  test("test2simpleMap") {
    val in = Array(2,3)
    generate(progStaged)(progStaged.simpleMap, "simpleMap")
//    compareOutputWithSequential(progStaged)(progSeq.simpleMap, progStaged.simpleMap, "simpleMap", in)
  }
  test("test3simpleMapNested") {
    val in = (Array(Array(2.0,3.0), Array(3.0,4.0)), 1)
    generate(progStaged)(progStaged.simpleMapNested, "simpleMapNested")
//    compareOutputWithSequential(progStaged)(progSeq.simpleMapNested, progStaged.simpleMapNested, "simpleMapNested", in)
  }
  test("test4simpleZip") {
    val in = Array(2,3)
    generate(progStaged)(progStaged.simpleZip, "simpleZip")
//    compareOutputWithSequential(progStaged)(progSeq.simpleZip, progStaged.simpleZip, "simpleZip", in)
  }
  test("test5simpleZipWith") {
    val in = Array(2,3)
    generate(progStaged)(progStaged.simpleZipWith, "simpleZipWith")
//    compareOutputWithSequential(progStaged)(progSeq.simpleZipWith, progStaged.simpleZipWith, "simpleZipWith", in)
  }
  test("test6simpleReduce") {
    val in = Array(2,3)
    generate(progStaged)(progStaged.simpleReduce, "simpleArith")
//    compareOutputWithSequential(progStaged)(progSeq.simpleReduce, progStaged.simpleReduce, "simpleArith", in)
  }
  test("test7mvMul") {
    val in = (Array(Array(2,3), Array(4,5)), Array(6,7))
    generate(progStaged)(progStaged.mvMul, "mvMul")
//    compareOutputWithSequential(progStaged)(progSeq.mvMul, progStaged.mvMul, "mvMul", in)
  }
  test("test9simpleIf") {
    val in = (Array(2.0,3.0), 4.0)
    generate(progStaged)(progStaged.simpleIf, "simpleIf")
//    compareOutputWithSequential(progStaged)(progSeq.simpleIf, progStaged.simpleIf, "simpleIf", in)
  }
  test("test10simpleSum") { //TODO: CLikeGen: remap(m) : Type scala.util.Either[Int, Unit] cannot be remapped.
    pending
    val in = 7
    generate(progStaged)(progStaged.simpleSum, "simpleSum")
//    compareOutputWithSequential(progStaged)(progSeq.simpleSum, progStaged.simpleSum, "simpleSum", in)
  }
  test("test11optionOps") {
    val in = 7
    generate(progStaged)(progStaged.optionOps, "optionOps")
//    compareOutputWithSequential(progStaged)(progSeq.optionOps, progStaged.optionOps, "simpleOptionOps", in)
  }
  test("lambdaApply") {
    val x = 7
    val f = (_: Int) * 2
    generate(progStaged)(progStaged.lambdaApply, "lambdaApply")
//    compareOutputWithSequential(progStaged)(progSeq.lambdaApply, progStaged.lambdaApply, "lambdaApply", (x, f))
  }
  test("lambdaConst") {
    pending
    val in = 7
    generate(progStaged)(progStaged.lambdaConst, "lambdaConst")
//    getStagedOutput(progStaged)(progStaged.lambdaConst, "lambdaConst", in).isInstanceOf[Right[_, _]]
  }

}
