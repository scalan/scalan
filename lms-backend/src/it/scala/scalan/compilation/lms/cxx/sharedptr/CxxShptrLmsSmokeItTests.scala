package scalan
package compilation
package lms
package cxx
package sharedptr

import scalan.it.BaseItTests
import scalan.it.smoke.SmokeProg

trait CxxShptrTestProg extends SmokeProg {
  lazy val arrayForeach = fun {arr:Rep[Array[Double]] =>
    arr.fold(arr, {p:Rep[(Array[Double],Double)] => p._1.update(0, p._1(0) + p._2 + 1.0)})
  }

  lazy val testList = fun {in:Rep[List[Array[Double]]] =>
    in.map{a => a.map({v => v*2.0})}
  }

  lazy val testStringDuplicate = fun {str:Rep[String] => (str + str + "duplicate").startsWith("hello")}
}

class CxxShptrLmsSmokeItTests extends BaseItTests[CxxShptrTestProg](new ScalanDslSeq with CxxShptrTestProg) {
  class ProgExp extends ScalanDslExp with CxxShptrTestProg

  val progStaged = new LmsCompilerCxx(new ProgExp)
  val defaultCompilers = compilers(progStaged)

  import progStaged.scalan.{SOption, |}

  test("testStringDuplicate") {
    val in = "word_"
    compileSource(_.testStringDuplicate)
  }

  test("testList") {
    val in = 2
    compileSource(_.testList)
  }

  test("arrayForeach") {
    val in = 2
    compileSource(_.arrayForeach)
    //    compareOutputWithSequential(progStaged)(progSeq.simpleArith, progStaged.simpleArith, "simpleArith", in)
  }

  test("test10simpleSum") {
    val in = 7
    compileSource[Int, (Int | Unit, Unit | Int)](_.simpleSum)
    //    compareOutputWithSequential(progStaged)(progSeq.simpleSum, progStaged.simpleSum, "simpleSum", in)
  }
  test("test11optionOps") {
    val in = 7
    compileSource(_.optionOps)
    //    compareOutputWithSequential(progStaged)(progSeq.simpleOptionOps, progStaged.simpleOptionOps, "simpleOptionOps", in)
  }
  test("lambdaApply") {
    val x = 7
    val f = (_: Int) * 2
    compileSource(_.lambdaApply)
    //    compareOutputWithSequential(progStaged)(progSeq.lambdaApply, progStaged.lambdaApply, "lambdaApply", (x, f))
  }
  test("lambdaConst") {
    val in = 7
    compileSource[Int, SOption[Int => Boolean]](_.lambdaConst)
    //    getStagedOutput(progStaged)(progStaged.lambdaConst, "lambdaConst", in).isInstanceOf[Right[_, _]]
  }

  test("arrayUpdate") {
    val in = Array(0, 0)
    compileSource(_.arrayUpdate)
    //    compareOutputWithSequential(progStaged)(progSeq.arrayUpdate, progStaged.arrayUpdate, "arrayUpdate", in)
  }
}
