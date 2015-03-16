package scalan.it.lms

import java.io.File
import scala.language.reflectiveCalls
import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.compilation.lms.JNIBridge
import scalan.compilation.lms.cxx.{CommunityCXXLmsBackend, LmsCompilerCXX}
import scalan.linalgebra.{MatricesDslExp, VectorsDslExp}
import scalan.performance.MVMs
import scalan._
import scalan.graphs.{GraphsDslExp}
import scalan.it.BaseItTests

class JNI_MsfItTests extends LmsMsfItTests {
  trait ProgExp extends GraphsDslExp with MsfFuncs with ScalanCommunityExp with ScalanCommunityDslExp with GraphVizExport with LmsCompilerCXX with JNIBridge with VectorsDslExp with MatricesDslExp { self =>
    val lms = new CommunityCXXLmsBackend

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

  test("MSF_JNI") {
    val ctx1 = new ScalanCtxExp with ProgExp with FirstProg {
      override def subfolder: String = "MSF_JNI-cxx"
      def test() = {

      }

      def generate[A,B](name: String, f: Exp[A => B]): Unit = {
        val dir = new File(prefix, subfolder)
        buildExecutable(dir, dir, name, f, GraphVizConfig.default)
      }
    }

    val ctx2 = new ScalanCtxExp with ProgExp with FirstProg {
      override def subfolder: String = "MSF_JNI-cxx"
      def test() = {

      }

      def generate[A,B](name: String, f: Exp[A => B]): Unit = {
        val dir = new File(prefix, subfolder)
        buildExecutable(dir, dir, name, f, GraphVizConfig.default)
      }
    }

    ctx1.generate("MSF_JNI_adjlist", ctx1.MSF_JNI_adjlist)
    ctx2.generate("MSF_JNI_adjmatrix", ctx2 .MSF_JNI_adjmatrix)
  }
}
