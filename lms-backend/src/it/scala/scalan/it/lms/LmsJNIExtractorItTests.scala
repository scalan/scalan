package scalan.it.lms

import java.io.File

import scala.language.reflectiveCalls
import scalan._
import scalan.compilation.lms.JNIBridge
import scalan.compilation.lms.cxx.LmsCompilerCxx
import scalan.compilation.lms.cxx.sharedptr.CoreCxxShptrLmsBackend
import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.graphs.MST_example
import scalan.it.BaseItTests
import scalan.linalgebra.{MatricesDslExp, VectorsDslExp}

class LmsJNIExtractorItTests extends BaseItTests {
  trait ProgExp extends MST_example with ScalanCommunityExp with ScalanCommunityDslExp with GraphVizExport with LmsCompilerCxx with JNIBridge with VectorsDslExp with MatricesDslExp { self =>
    val lms = new CoreCxxShptrLmsBackend
    
    lazy val MST_JNI_adjlist = fun {in:Rep[JNIType[(Array[Int], (Array[Double], (Array[Int], Array[Int])))]] =>
      val data = JNI_Extract(in)
      val res = MST_adjlist(data)
      JNI_Pack(res)
    }

    lazy val MST_JNI_adjmatrix = fun {in:Rep[JNIType[(Array[Double], Int)]] =>
      val data = JNI_Extract(in)
      val res = MST_adjmatrix(data)
      JNI_Pack(res)
    }

    lazy val MSF_JNI_adjlist = fun {in:Rep[JNIType[(Array[Int], (Array[Double], (Array[Int], Array[Int])))]] =>
      val data = JNI_Extract(in)
      val res = MSF_adjlist(data)
      JNI_Pack(res)
    }

    lazy val MSF_JNI_adjmatrix = fun {in:Rep[JNIType[(Array[Double], Int)]] =>
      val data = JNI_Extract(in)
      val res = MSF_adjmatrix(data)
      JNI_Pack(res)
    }

    lazy val MSF_JNI_adjlistList = fun {in:Rep[JNIType[(Array[Int], (Array[Double], (Array[Int], Array[Int])))]] =>
      val data = JNI_Extract(in)
      val res = MSF_adjlistList(data)
      JNI_Pack(res)
    }

    lazy val MSF_JNI_adjmatrixList = fun {in:Rep[JNIType[(Array[Double], Int)]] =>
      val data = JNI_Extract(in)
      val res = MSF_adjmatrixList(data)
      JNI_Pack(res)
    }
  }

  class Ctx(subfolder: String) extends ScalanCtxExp with ProgExp {
    def generate[A,B](name: String, f: Exp[A => B]): Unit = {
      val dir = new File(prefix, subfolder)
      buildExecutable(dir, dir, name, f, GraphVizConfig.default)
    }
  }

  test("MST_JNI") {
    val ctx = new Ctx("firstprog-cxx") with FirstProg

//    ctx.generate("MST_JNI_adjlist", ctx.MST_JNI_adjlist)
//    ctx.generate("MST_JNI_adjmatrix", ctx.MST_JNI_adjmatrix)
    ctx.generate("MSF_JNI_adjlist", ctx.MSF_JNI_adjlist)
    ctx.generate("MSF_JNI_adjmatrix", ctx.MSF_JNI_adjmatrix)
    ctx.generate("MSF_JNI_adjlistList", ctx.MSF_JNI_adjlistList)
    ctx.generate("MSF_JNI_adjmatrixList", ctx.MSF_JNI_adjmatrixList)
  }

  test("simpleGenCxx") {
    val ctx = new Ctx("firstprog-cxx") with FirstProg

    ctx.generate("extractDouble", ctx.extractDouble)
    ctx.generate("extractArray", ctx.extractArray)
    ctx.generate("extractDM", ctx.extractDM)
    ctx.generate("extractPair", ctx.extractPair)
    ctx.generate("extractPairUse1", ctx.extractPairUse1)
    ctx.generate("extractPairUse2", ctx.extractPairUse2)
    ctx.generate("extractSM", ctx.extractSM)
    ctx.generate("ddmvm", ctx.ddmvm)
    ctx.generate("sdmvm", ctx.sdmvm)
  }

  test("simplePackGenCxx") {
    val ctx = new Ctx("packprog-cxx") with PackProg

//    ctx.generate("opyt", ctx.opyt)
//    ctx.generate("packArray", ctx.packArray)
    ctx.generate("extractAndPack", ctx.extractAndPack)
    ctx.generate("packPair", ctx.packPair)
  }

  test("mvm") {
    val ctx = new Ctx("packprog-cxx") with PackProg

//    ctx.generate("opyt", ctx.opyt)
    ctx.generate("ddmvm", ctx.ddmvm)
    ctx.generate("sdmvm", ctx.sdmvm)
//    ctx.generate("packPair", ctx.packPair)
  }
}
