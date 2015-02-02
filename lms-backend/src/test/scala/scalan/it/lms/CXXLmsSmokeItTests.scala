package scalan.it.lms

import java.io.File

import scalan.ScalanCtxExp
import scalan.community.ScalanCommunityExp
import scalan.compilation.GraphVizExport
import scalan.compilation.lms.CoreBridge
import scalan.compilation.lms.cxx.{LmsCompilerCXX, CoreCXXLmsBackend}
import scalan.it.smoke.SmokeItTests

class CXXLmsSmokeItTests extends SmokeItTests {
  class ProgExp extends Prog with ScalanCtxExp with ScalanCommunityExp with GraphVizExport with LmsCompilerCXX { self =>
    def makeBridge[A, B] = new CoreBridge[A, B] {
      val scalan = self
      val lms = new CoreCXXLmsBackend
    }
  }
  
  override val progStaged = new ProgExp

  test("test0simpleArith") {
    val in = 2
    val functionName = "simpleArith"
    val dir = new File(prefix, functionName)
    progStaged.generate(dir, dir, functionName, progStaged.simpleArith, true)(progStaged.defaultConfig)
//    compareOutputWithSequential(progStaged)(progSeq.simpleArith, progStaged.simpleArith, "simpleArith", in)
  }
  test("test1simpleArrGet") {
    val in = (Array(2,3), 1)
    val functionName = "simpleArrGet"
    val dir = new File(prefix, functionName)
    progStaged.generate(dir, dir, functionName, progStaged.simpleArrGet, true)(progStaged.defaultConfig)
//    compareOutputWithSequential(progStaged)(progSeq.simpleArrGet, progStaged.simpleArrGet, "simpleArrGet", in)
  }
  test("test2simpleMap") {
    val in = Array(2,3)
    val functionName = "simpleMap"
    val dir = new File(prefix, functionName)
    progStaged.generate(dir, dir, functionName, progStaged.simpleMap, true)(progStaged.defaultConfig)
//    compareOutputWithSequential(progStaged)(progSeq.simpleMap, progStaged.simpleMap, "simpleMap", in)
  }
  test("test3simpleMapNested") {
    val in = (Array(Array(2.0,3.0), Array(3.0,4.0)), 1)
    val functionName = "simpleMapNested"
    val dir = new File(prefix, functionName)
    progStaged.generate(dir, dir, functionName, progStaged.simpleMapNested, true)(progStaged.defaultConfig)
//    compareOutputWithSequential(progStaged)(progSeq.simpleMapNested, progStaged.simpleMapNested, "simpleMapNested", in)
  }
  test("test4simpleZip") {
    val in = Array(2,3)
    val functionName = "simpleZip"
    val dir = new File(prefix, functionName)
    progStaged.generate(dir, dir, functionName, progStaged.simpleZip, true)(progStaged.defaultConfig)
//    compareOutputWithSequential(progStaged)(progSeq.simpleZip, progStaged.simpleZip, "simpleZip", in)
  }
  test("test5simpleZipWith") {
    val in = Array(2,3)
    val functionName = "simpleZipWith"
    val dir = new File(prefix, functionName)
    progStaged.generate(dir, dir, functionName, progStaged.simpleZipWith, true)(progStaged.defaultConfig)
//    compareOutputWithSequential(progStaged)(progSeq.simpleZipWith, progStaged.simpleZipWith, "simpleZipWith", in)
  }
  test("test6simpleReduce") {
    val in = Array(2,3)
    val functionName = "simpleReduce"
    val dir = new File(prefix, functionName)
    progStaged.generate(dir, dir, functionName, progStaged.simpleReduce, true)(progStaged.defaultConfig)
//    compareOutputWithSequential(progStaged)(progSeq.simpleReduce, progStaged.simpleReduce, "simpleArith", in)
  }
  test("test7mvMul") {
    val in = (Array(Array(2,3), Array(4,5)), Array(6,7))
    val functionName = "mvMul"
    val dir = new File(prefix, functionName)
    progStaged.generate(dir, dir, functionName, progStaged.mvMul, true)(progStaged.defaultConfig)
//    compareOutputWithSequential(progStaged)(progSeq.mvMul, progStaged.mvMul, "mvMul", in)
  }
  ignore("test9simpleIf") { //TODO: implement fat if C codegen
    val in = (Array(2.0,3.0), 4.0)
    val functionName = "simpleIf"
    val dir = new File(prefix, functionName)
      progStaged.generate(dir, dir, functionName, progStaged.simpleIf, true)(progStaged.defaultConfig)
//    compareOutputWithSequential(progStaged)(progSeq.simpleIf, progStaged.simpleIf, "simpleIf", in)
  }
  ignore("test10simpleSum") { //TODO: CLikeGen: remap(m) : Type scala.util.Either[Int, Unit] cannot be remapped.
    val in = 7
    val functionName = "simpleSum"
    val dir = new File(prefix, functionName)
    progStaged.generate(dir, dir, functionName, progStaged.simpleSum, true)(progStaged.defaultConfig)
//    compareOutputWithSequential(progStaged)(progSeq.simpleSum, progStaged.simpleSum, "simpleSum", in)
  }
  test("test11simpleOptionOps") {
    val in = 7
    val functionName = "optionOps"
    val dir = new File(prefix, functionName)
    progStaged.generate(dir, dir, functionName, progStaged.optionOps, true)(progStaged.defaultConfig)
//    compareOutputWithSequential(progStaged)(progSeq.simpleOptionOps, progStaged.simpleOptionOps, "simpleOptionOps", in)
  }

}
