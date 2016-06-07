package scalan.pointers

import java.io.File

import scalan._
import scalan.compilation.GraphVizConfig
import scalan.compilation.lms.cxx.LmsCompilerCxx
import scalan.compilation.lms.cxx.sharedptr.{CxxCoreCodegen, CoreCxxShptrLmsBackend}
import scalan.compilation.lms.pointers.{CxxShptrGenPointer, PointerLmsOpsExp, PointerLmsBridge}
import scalan.it.BaseItTests

trait PointerProg extends ScalanDsl with PointerOps {
  lazy val intPtr = fun { i: Rep[Int] =>
    val iPtr = scalarPtr(i)
    iPtr
  }

  lazy val valuePtr = fun { i: Rep[Int] =>  // input not used
    val yPtr = scalarPtr(2)   // note: in cpp we cannot take address of const: &2, here we use scalar pointer
    yPtr
  }

  lazy val pairPtrIntDoubleSame = fun { i: Rep[Int] => // not used
    val int0Ptr = scalarPtr[Int](0)
    val double0Ptr = scalarPtr[Double](0.0)
    (int0Ptr, double0Ptr)
  }

  lazy val intIfPtr = fun { i: Rep[Int] =>
    val iPtr = intPtr(i)
    val yPtr = valuePtr(i)
    val int0: Rep[Int] = 0
    val ifPtr = ifThenElse(i === int0, iPtr, yPtr)
    ifPtr
  }

  lazy val xsArrayPtr = fun { xs: Rep[Array[Int]] =>
    arrayPtr(xs)
  }
}

class PointerItTests extends BaseItTests[PointerProg](???) {

  class ProgExp extends ScalanDslExp with PointerOpsExp with PointerProg

  val progExp = new LmsCompilerCxx(new ProgExp) with PointerLmsBridge {
    override val lms = new CoreCxxShptrLmsBackend with PointerLmsOpsExp { self =>
      override val codegen = new CxxCoreCodegen[self.type](self) with CxxShptrGenPointer
    }
  }
  val defaultCompilers = compilers(progExp)
  implicit val cfg = progExp.defaultCompilerConfig

  def commonTestScenario[A, B](functionName: String, func: => progExp.Exp[A => B]): Unit = {
    val dir = new File(prefix, functionName)
    progExp.buildExecutable(dir, dir, functionName, func, GraphVizConfig.default)
  }

  /**
   * test now generate cxx code only, to execute and check please use:
   * .../scalan-lite-public/lms-backend/src/test/cpp/pointerItTestsCheck$ . check
   */

  import progExp.scalan.Pointer

  test("intPtr") { commonTestScenario[Int, Pointer[Int]]("intPtr", progExp.scalan.intPtr) }

  test("valuePtr") { commonTestScenario[Int, Pointer[Int]]("valuePtr", progExp.scalan.valuePtr) }

  test("pairPtrIntDoubleSame") {
    commonTestScenario[Int, (Pointer[Int], Pointer[Double])]("pairPtrIntDoubleSame", progExp.scalan.pairPtrIntDoubleSame)
  }

  test("intIfPtr") { commonTestScenario[Int, Pointer[Int]]("intIfPtr", progExp.scalan.intIfPtr) }

  test("xsArrayPtr") { commonTestScenario[Array[Int], Pointer[Int]]("xsArrayPtr", progExp.scalan.xsArrayPtr) }
}
