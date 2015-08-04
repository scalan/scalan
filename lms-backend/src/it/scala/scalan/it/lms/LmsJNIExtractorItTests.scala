package scalan.it.lms

import java.io.File

import scala.language.reflectiveCalls
import scalan._
import scalan.compilation.lms.JNIBridge
import scalan.compilation.lms.cxx.LmsCompilerCxx
import scalan.compilation.lms.cxx.sharedptr.CoreCxxShptrLmsBackend
import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.graphs.MST_example
import scalan.it.BaseCtxItTests
import scalan.linalgebra.{MatricesDslExp, VectorsDslExp}

class LmsJNIExtractorItTests extends BaseCtxItTests {
  trait ProgExp extends MST_example with ScalanCommunityDslExp with JNIExtractorOpsExp {
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

  class Ctx[T <: ProgExp](val _scalan: T) extends TestCompilerContext {
    val compiler = new LmsCompilerCxx with JNIBridge {
      val scalan: _scalan.type = _scalan
    }
  }

  test("MST_JNI") {
    val ctx = new Ctx(new ProgExp with FirstProg)

//    ctx.test("MST_JNI_adjlist", ctx.MST_JNI_adjlist)
//    ctx.test("MST_JNI_adjmatrix", ctx.MST_JNI_adjmatrix)
    ctx.test("MSF_JNI_adjlist", ctx._scalan.MSF_JNI_adjlist)
    ctx.test("MSF_JNI_adjmatrix", ctx._scalan.MSF_JNI_adjmatrix)
    ctx.test("MSF_JNI_adjlistList", ctx._scalan.MSF_JNI_adjlistList)
    ctx.test("MSF_JNI_adjmatrixList", ctx._scalan.MSF_JNI_adjmatrixList)
  }

  test("simpleGenCxx") {
    val ctx = new Ctx(new ProgExp with FirstProg)

    ctx.test("extractDouble", ctx._scalan.extractDouble)
    ctx.test("extractArray", ctx._scalan.extractArray)
    ctx.test("extractDM", ctx._scalan.extractDM)
    ctx.test("extractPair", ctx._scalan.extractPair)
    ctx.test("extractPairUse1", ctx._scalan.extractPairUse1)
    ctx.test("extractPairUse2", ctx._scalan.extractPairUse2)
    ctx.test("extractSM", ctx._scalan.extractSM)
    ctx.test("ddmvm", ctx._scalan.ddmvm)
    ctx.test("sdmvm", ctx._scalan.sdmvm)
  }

  test("simplePackGenCxx") {
    val ctx = new Ctx(new ProgExp with PackProg)

//    ctx.test("opyt", ctx.opyt)
//    ctx.test("packArray", ctx.packArray)
    ctx.test("extractAndPack", ctx._scalan.extractAndPack)
    ctx.test("packPair", ctx._scalan.packPair)
  }

  test("mvm") {
    val ctx = new Ctx(new ProgExp with PackProg)

//    ctx.test("opyt", ctx.opyt)
    ctx.test("ddmvm", ctx._scalan.ddmvm)
    ctx.test("sdmvm", ctx._scalan.sdmvm)
//    ctx.test("packPair", ctx.packPair)
  }
}
