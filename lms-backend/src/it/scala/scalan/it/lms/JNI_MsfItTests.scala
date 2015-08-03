package scalan.it.lms

import java.io.File

import scala.language.reflectiveCalls
import scalan._
import scalan.compilation.lms.JNIBridge
import scalan.compilation.lms.cxx.LmsCompilerCxx
import scalan.compilation.lms.cxx.sharedptr.CoreCxxShptrLmsBackend
import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.graphs.GraphsDslExp
import scalan.linalgebra.{MatricesDslExp, VectorsDslExp}

class JNI_MsfItTests extends LmsMsfItTests {
  trait ProgExp extends ScalanCommunityDslExp with JNIExtractorOpsExp with GraphsDslExp with MsfFuncs {

    lazy val MSF_JNI_adjlist = fun {in:Rep[JNIType[(Array[Int], (Array[Double], (Array[Int], Array[Int])))]] =>
      val data = JNI_Extract(in)
      val res = msfFunAdjBase(data)
      JNI_Pack(res)
    }

    lazy val MSF_JNI_adjmatrix = fun {in:Rep[JNIType[(Array[Double], Int)]] =>
      val data = JNI_Extract(in)
      val res = msfFunIncBase(data)
      JNI_Pack(res)
    }
  }

  class Ctx extends TestCompilerContext("MSF_JNI-cxx") {
    val compiler = new LmsCompilerCxx with JNIBridge {
      val scalan = new ProgExp {}
      val lms = new CoreCxxShptrLmsBackend
    }
  }

  test("MSF_JNI") {
    val ctx1 = new Ctx

    val ctx2 = new Ctx

    ctx1.test("MSF_JNI_adjlist", ctx1.compiler.scalan.MSF_JNI_adjlist)
    ctx2.test("MSF_JNI_adjmatrix", ctx2.compiler.scalan.MSF_JNI_adjmatrix)
  }
}
