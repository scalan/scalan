package scalan
package compilation
package lms
package cxx
package sharedptr

import java.io.File

import scalan.it.smoke.SmokeItTests

class CxxShptrLmsSmokeItTests extends SmokeItTests {
  trait Prog extends super.Prog {
    lazy val arrayForeach = fun {arr:Rep[Array[Double]] =>
      arr.fold(arr, {p:Rep[(Array[Double],Double)] => p._1.update(0, p._1(0) + p._2 + 1.0)})
    }

    lazy val testList = fun {in:Rep[List[Array[Double]]] =>
      in.map{a => a.map({v => v*2.0})}
    }

    lazy val testStringDuplicate = fun {str:Rep[String] => (str + str + "duplicate").startsWith("hello")}
  }
  class ProgExp extends Prog with ScalanCommunityExp

  val progSeq = new Prog with ScalanCommunitySeq
  val progStaged = new LmsCompilerCxx(new ProgExp) with CoreBridge
  val defaultCompilers = compilers(progStaged)

  import progStaged.scalan.{|, SOption}

  test("testStringDuplicate") {
    val in = "word_"
    compileSource(_.testStringDuplicate, "testStringDuplicate")
  }

  test("testList") {
    val in = 2
    compileSource(_.testList, "testList")
  }

  test("arrayForeach") {
    val in = 2
    compileSource(_.arrayForeach, "arrayForeach")
    //    compareOutputWithSequential(progStaged)(progSeq.simpleArith, progStaged.simpleArith, "simpleArith", in)
  }

  test("test10simpleSum") {
    val in = 7
    compileSource[Int, (Int | Unit, Unit | Int)](_.simpleSum, "simpleSum")
    //    compareOutputWithSequential(progStaged)(progSeq.simpleSum, progStaged.simpleSum, "simpleSum", in)
  }
  test("test11optionOps") {
    val in = 7
    compileSource(_.optionOps, "optionOps")
    //    compareOutputWithSequential(progStaged)(progSeq.simpleOptionOps, progStaged.simpleOptionOps, "simpleOptionOps", in)
  }
  test("lambdaApply") {
    val x = 7
    val f = (_: Int) * 2
    compileSource(_.lambdaApply, "lambdaApply")
    //    compareOutputWithSequential(progStaged)(progSeq.lambdaApply, progStaged.lambdaApply, "lambdaApply", (x, f))
  }
  test("lambdaConst") {
    val in = 7
    compileSource[Int, SOption[Int => Boolean]](_.lambdaConst, "lambdaConst")
    //    getStagedOutput(progStaged)(progStaged.lambdaConst, "lambdaConst", in).isInstanceOf[Right[_, _]]
  }

  test("arrayUpdate") {
    val in = Array(0, 0)
    compileSource(_.arrayUpdate, "arrayUpdate")
    //    compareOutputWithSequential(progStaged)(progSeq.arrayUpdate, progStaged.arrayUpdate, "arrayUpdate", in)
  }
}
