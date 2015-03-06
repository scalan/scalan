package scalan
package compilation
package lms
package cxx
package sharedptr

import java.io.File

import scalan.it.smoke.SmokeItTests

class CxxShptrLmsSmokeItTests extends SmokeItTests {
  class ProgExp extends Prog with ScalanCtxExp with ScalanCommunityExp with GraphVizExport with LmsCompilerCXX with CoreBridge {
    val lms = new CoreCxxShptrLmsBackend
  }

  override val progStaged = new ProgExp
  import progStaged.defaultCompilerConfig

  test("test0simpleArith") {
    val in = 2
    val functionName = "simpleArith"
    val dir = new File(prefix, functionName)
    progStaged.buildExecutable(dir, dir, functionName, progStaged.simpleArith, GraphVizConfig.default)
    //    compareOutputWithSequential(progStaged)(progSeq.simpleArith, progStaged.simpleArith, "simpleArith", in)
  }
  test("test1simpleArrGet") {
    val in = (Array(2,3), 1)
    val functionName = "simpleArrGet"
    val dir = new File(prefix, functionName)
    progStaged.buildExecutable(dir, dir, functionName, progStaged.simpleArrGet, GraphVizConfig.default)
    //    compareOutputWithSequential(progStaged)(progSeq.simpleArrGet, progStaged.simpleArrGet, "simpleArrGet", in)
  }
  test("test2simpleMap") {
    val in = Array(2,3)
    val functionName = "simpleMap"
    val dir = new File(prefix, functionName)
    progStaged.buildExecutable(dir, dir, functionName, progStaged.simpleMap, GraphVizConfig.default)
    //    compareOutputWithSequential(progStaged)(progSeq.simpleMap, progStaged.simpleMap, "simpleMap", in)
  }
  test("test3simpleMapNested") {
    val in = (Array(Array(2.0,3.0), Array(3.0,4.0)), 1)
    val functionName = "simpleMapNested"
    val dir = new File(prefix, functionName)
    progStaged.buildExecutable(dir, dir, functionName, progStaged.simpleMapNested, GraphVizConfig.default)
    //    compareOutputWithSequential(progStaged)(progSeq.simpleMapNested, progStaged.simpleMapNested, "simpleMapNested", in)
  }
  test("test4simpleZip") {
    val in = Array(2,3)
    val functionName = "simpleZip"
    val dir = new File(prefix, functionName)
    progStaged.buildExecutable(dir, dir, functionName, progStaged.simpleZip, GraphVizConfig.default)
    //    compareOutputWithSequential(progStaged)(progSeq.simpleZip, progStaged.simpleZip, "simpleZip", in)
  }
  test("test5simpleZipWith") {
    val in = Array(2,3)
    val functionName = "simpleZipWith"
    val dir = new File(prefix, functionName)
    progStaged.buildExecutable(dir, dir, functionName, progStaged.simpleZipWith, GraphVizConfig.default)
    //    compareOutputWithSequential(progStaged)(progSeq.simpleZipWith, progStaged.simpleZipWith, "simpleZipWith", in)
  }
  test("test6simpleReduce") {
    val in = Array(2,3)
    val functionName = "simpleReduce"
    val dir = new File(prefix, functionName)
    progStaged.buildExecutable(dir, dir, functionName, progStaged.simpleReduce, GraphVizConfig.default)
    //    compareOutputWithSequential(progStaged)(progSeq.simpleReduce, progStaged.simpleReduce, "simpleArith", in)
  }
  test("test7mvMul") {
    val in = (Array(Array(2,3), Array(4,5)), Array(6,7))
    val functionName = "mvMul"
    val dir = new File(prefix, functionName)
    progStaged.buildExecutable(dir, dir, functionName, progStaged.mvMul, GraphVizConfig.default)
    //    compareOutputWithSequential(progStaged)(progSeq.mvMul, progStaged.mvMul, "mvMul", in)
  }
  test("test9simpleIf") {
    val in = (Array(2.0,3.0), 4.0)
    val functionName = "simpleIf"
    val dir = new File(prefix, functionName)
    progStaged.buildExecutable(dir, dir, functionName, progStaged.simpleIf, GraphVizConfig.default)
    //    compareOutputWithSequential(progStaged)(progSeq.simpleIf, progStaged.simpleIf, "simpleIf", in)
  }
  test("test10simpleSum") { //TODO: CLikeGen: remap(m) : Type scala.util.Either[Int, Unit] cannot be remapped.
    pending
    val in = 7
    val functionName = "simpleSum"
    val dir = new File(prefix, functionName)
    progStaged.buildExecutable(dir, dir, functionName, progStaged.simpleSum, GraphVizConfig.default)
    //    compareOutputWithSequential(progStaged)(progSeq.simpleSum, progStaged.simpleSum, "simpleSum", in)
  }
  test("test11optionOps") {
    val in = 7
    val functionName = "optionOps"
    val dir = new File(prefix, functionName)
    progStaged.buildExecutable(dir, dir, functionName, progStaged.optionOps, GraphVizConfig.default)
    //    compareOutputWithSequential(progStaged)(progSeq.simpleOptionOps, progStaged.simpleOptionOps, "simpleOptionOps", in)
  }
  test("lambdaApply") {
    val x = 7
    val f = (_: Int) * 2
    val functionName = "lambdaApply"
    val dir = new File(prefix, functionName)
    progStaged.buildExecutable(dir, dir, functionName, progStaged.lambdaApply, GraphVizConfig.default)
    //    compareOutputWithSequential(progStaged)(progSeq.lambdaApply, progStaged.lambdaApply, "lambdaApply", (x, f))
  }
  test("lambdaConst") {
    pending
    val in = 7
    val functionName = "lambdaConst"
    val dir = new File(prefix, functionName)
    progStaged.buildExecutable(dir, dir, functionName, progStaged.lambdaConst, GraphVizConfig.default)
    //    getStagedOutput(progStaged)(progStaged.lambdaConst, "lambdaConst", in).isInstanceOf[Right[_, _]]
  }

  test("arrayUpdate") {
    val in = Array(0, 0)
    val functionName = "arrayUpdate"
    val dir = new File(prefix, functionName)
    progStaged.buildExecutable(dir, dir, functionName, progStaged.arrayUpdate, GraphVizConfig.default)
    //    compareOutputWithSequential(progStaged)(progSeq.arrayUpdate, progStaged.arrayUpdate, "arrayUpdate", in)
  }
}
