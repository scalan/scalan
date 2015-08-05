package scalan.it.lms

import java.io.File

import scalan.{PointerOpsExp, CommunityMethodMappingDSL, ScalanCommunityDslExp, ScalanCommunityExp}
import scalan.compilation.lms.PointerBridge
import scalan.compilation.lms.cxx.LmsCompilerCxx
import scalan.compilation.lms.cxx.sharedptr.CoreCxxShptrLmsBackend
import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.it.BaseItTests

class PointerItTests extends BaseItTests {

  class ProgExp extends ScalanCommunityDslExp with PointerOpsExp {
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
      val ifPtr = __ifThenElse(i === int0, iPtr, yPtr)
      ifPtr
    }

    lazy val xsArrayPtr = fun { xs: Rep[Array[Int]] =>
      arrayPtr(xs)
    }
  }

  val progExp = new LmsCompilerCxx(new ProgExp) with PointerBridge with CommunityMethodMappingDSL
  implicit val cfg = progExp.defaultCompilerConfig

  def commonTestScenario[A, B](functionName: String, func: progExp.Exp[A => B]): Unit = {
    val dir = new File(prefix, functionName)
    progExp.buildExecutable(dir, dir, functionName, func, GraphVizConfig.default)
    println("cxx file generated")
  }

  /**
   * test now generate cxx code only, to execute and check please use:
   * .../scalan-lite-public/lms-backend/src/test/cpp/pointerItTestsCheck$ . check
   */

  type Pointer[T] = progExp.scalan.Pointer[T]

  test("intPtr") { commonTestScenario[Int, Pointer[Int]]("intPtr", progExp.scalan.intPtr) }

  test("valuePtr") { commonTestScenario[Int, Pointer[Int]]("valuePtr", progExp.scalan.valuePtr) }

  test("pairPtrIntDoubleSame") {
    commonTestScenario[Int, (Pointer[Int], Pointer[Double])]("pairPtrIntDoubleSame", progExp.scalan.pairPtrIntDoubleSame)
  }

  test("intIfPtr") { commonTestScenario[Int, Pointer[Int]]("intIfPtr", progExp.scalan.intIfPtr) }

  test("xsArrayPtr") { commonTestScenario[Array[Int], Pointer[Int]]("xsArrayPtr", progExp.scalan.xsArrayPtr) }
}
