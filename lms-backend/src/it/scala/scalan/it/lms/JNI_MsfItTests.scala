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
  trait ProgExp extends GraphsDslExp with MsfFuncs with ScalanCommunityExp with ScalanCommunityDslExp with GraphVizExport with LmsCompilerCxx with JNIBridge with VectorsDslExp with MatricesDslExp { self =>
    val lms = new CoreCxxShptrLmsBackend

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

  class Ctx extends ScalanCtxExp with ProgExp {
    def generate[A,B](name: String, f: Exp[A => B]): Unit = {
      val dir = new File(prefix, "MSF_JNI-cxx")
      buildExecutable(dir, dir, name, f, GraphVizConfig.default)
    }
  }

  test("MSF_JNI") {
    val ctx1 = new Ctx

    val ctx2 = new Ctx

    ctx1.generate("MSF_JNI_adjlist", ctx1.MSF_JNI_adjlist)
    ctx2.generate("MSF_JNI_adjmatrix", ctx2 .MSF_JNI_adjmatrix)
  }
}
