package scalan.it.lms

import java.io.File
//
//import scalan.compilation.lms.mpi.CoreCxxShptrLmsBackend
//import scalan.compilation.lms.mpi.LmsCompilerCxx
//import scalan.it.BaseItTests
//import scalan.compilation.{GraphVizExport, GraphVizConfig}
//import scalan.compilation.lms.MpiBridge
//import scalan.compilation.lms.{CoreCxxShptrLmsBackend, LmsCompilerCxx}
//import org.scalatest.Tag

//import scala.language.reflectiveCalls
//import scalan._
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
  }

  val progExp = new ProgExp
  implicit val cfg = progExp.defaultCompilerConfig

  def commonTestScenario[A, B](functionName: String, func: progExp.Exp[A => B]): Unit = {
    val dir = new File(prefix, functionName)
    progExp.buildExecutable(dir, dir, functionName, func, GraphVizConfig.default)
    println("cxx file generated")
  }

  test("pairScalarIntDoubleSame") {
    commonTestScenario[Int, (progExp.Scalar[Int], progExp.Scalar[Double])]("pairScalarIntDoubleSame", progExp.pairScalarIntDoubleSame)
  }
}
