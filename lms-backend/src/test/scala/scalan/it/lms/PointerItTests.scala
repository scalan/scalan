package scalan.it.lms

import java.io.File

import scalan.{CommunityMethodMappingDSL, ScalanCommunityDslExp, ScalanCommunityExp}
import scalan.compilation.lms.PointerBridge
import scalan.compilation.lms.cxx.LmsCompilerCxx
import scalan.compilation.lms.cxx.sharedptr.CoreCxxShptrLmsBackend
import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.it.BaseItTests

class PointerItTests extends BaseItTests {

  class ProgExp
    extends ScalanCommunityDslExp
    with LmsCompilerCxx
    with PointerBridge
    with ScalanCommunityExp
    with GraphVizExport
    with CommunityMethodMappingDSL { self =>

    val lms = new CoreCxxShptrLmsBackend

    lazy val pairScalarIntDoubleSame = fun { i: Rep[Int] => // not used
      val iDef = createScalar[Int](0)
      val dDef = createScalar[Double](0.0)
      (iDef, dDef)
    }

    lazy val intPtr = fun { i: Rep[Int] =>
      var iScalar = createScalar(i)
      val iPtr = scalarPtr(iScalar)
      iPtr
    }

    lazy val valuePtr = fun { i: Rep[Int] =>  // input not used
      var yScalar = createScalar(2)    // note: in cpp we cannot take address of const: &2, here we use scalar type
      val yPtr = scalarPtr(yScalar)
      yPtr
    }

    lazy val xsArrayPtr = fun { xs: Rep[Array[Int]] =>
      arrayPtr(xs)
    }
  }

  val progExp = new ProgExp
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

  test("pairScalarIntDoubleSame") {
    commonTestScenario[Int, (progExp.Scalar[Int], progExp.Scalar[Double])]("pairScalarIntDoubleSame", progExp.pairScalarIntDoubleSame)
  }

  test("intPtr") {
    commonTestScenario[Int, progExp.Pointer[Int]]("intPtr", progExp.intPtr)
  }

  test("valuePtr") {
    commonTestScenario[Int, progExp.Pointer[Int]]("valuePtr", progExp.valuePtr)
  }

  test("xsArrayPtr") {
    commonTestScenario[Array[Int], progExp.Pointer[Int]]("xsArrayPtr", progExp.xsArrayPtr)
  }
}
