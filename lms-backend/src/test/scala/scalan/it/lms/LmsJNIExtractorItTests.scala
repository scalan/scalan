package scalan.it.lms

import java.io.File

import scala.language.reflectiveCalls
import scalan._
import scalan.compilation.lms.JNIBridge
import scalan.compilation.lms.cxx.{CommunityCXXLmsBackend, LmsCompilerCXX}
import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.graphs.MST_example
import scalan.it.BaseItTests
import scalan.linalgebra.{MatricesDslExp, VectorsDslExp}
import scalan.parrays.PArraysDslExp
import scalan.performance.MVMs

/**
 * Created by zotov on 1/19/15.
 */
class LmsJNIExtractorItTests extends BaseItTests {
  trait ProgExp extends MST_example with ScalanCommunityExp with ScalanCommunityDslExp with GraphVizExport with LmsCompilerCXX with JNIBridge with VectorsDslExp with MatricesDslExp { self =>
    val lms = new CommunityCXXLmsBackend

    lazy val MST_JNI_adjlist = fun {in:Rep[JNIType[(Array[Int], (Array[Double], (Array[Int], Array[Int])))]] =>
      val data = JNI_Extract(in)
      val res = MST_adjlist(data)
      JNI_Pack(res)
    }
  }

  test("MST_JNI") {
    val ctx = new ScalanCtxExp with ProgExp with FirstProg {
      override def subfolder: String = "MST_JNI-cxx"
      def test() = {

      }

      def generate[A,B](name: String, f: Exp[A => B]): Unit = {
        val dir = new File(prefix, subfolder)
        buildExecutable(dir, dir, name, f, GraphVizConfig.default)
      }
    }

    ctx.generate("MST_JNI_adjlist", ctx.MST_JNI_adjlist)
  }

  test("simpleGenCxx") {
    val ctx = new ScalanCtxExp with ProgExp with FirstProg {
      override def subfolder: String = super.subfolder + "-cxx"
      def test() = {

      }

      def generate[A,B](name: String, f: Exp[A => B]): Unit = {
        val dir = new File(prefix, subfolder)
        buildExecutable(dir, dir, name, f, GraphVizConfig.default)
      }
    }

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
    val ctx = new ScalanCtxExp with ProgExp with PackProg {
      override def subfolder: String = super.subfolder + "-cxx"
      def test() = {

      }

      def generate[A,B](name: String, f: Exp[A => B]): Unit = {
        val dir = new File(prefix, subfolder)
        buildExecutable(dir, dir, name, f, GraphVizConfig.default)
      }
    }

//    ctx.generate("opyt", ctx.opyt)
//    ctx.generate("packArray", ctx.packArray)
    ctx.generate("extractAndPack", ctx.extractAndPack)
    ctx.generate("packPair", ctx.packPair)
  }

  test("mvmCxxGen") {
    val ctx = new ScalanCtxExp with ProgExp with PackProg {
      override def subfolder: String = super.subfolder + "-cxx"
      def test() = {

      }

      def generate[A,B](name: String, f: Exp[A => B]): Unit = {
        val dir = new File(prefix, subfolder)
        buildExecutable(dir, dir, name, f, GraphVizConfig.default)
      }
    }

//    ctx.generate("opyt", ctx.opyt)
    ctx.generate("ddmvm", ctx.ddmvm)
    ctx.generate("sdmvm", ctx.sdmvm)
//    ctx.generate("packPair", ctx.packPair)
  }

  test("simpleBuildExecutable") { //TODO: automate executable building
    pending
    val ctx = new ScalanCtxExp with ProgExp with FirstProg {
      override def subfolder: String = super.subfolder + "-build"

      def test() = {
      }

      def buildExecutable[A,B](name: String, f: Exp[A => B]): Unit = {
        val dir = new File(prefix, subfolder)
        buildExecutable(dir, dir, name, f, GraphVizConfig.default)
      }
    }

    ctx.buildExecutable("extractDouble", ctx.extractDouble)
  }

  test("simpleExecute") { //TODO: automate execution
    pending
    val ctx = new ScalanCtxExp with ProgExp with FirstProg {
      override def subfolder: String = "release"

      def test() = {
      }

      def execute(): Unit = {
        val dir = new File(prefix, subfolder)
//        System.setProperty("java.library.path", dir.getAbsolutePath) //TODO: is there a way to set path dynamically?
        println(s"java.library.path=${System.getProperty("java.library.path")}")
        System.loadLibrary("jniExtractor")

//        val m = scala.Array(scala.Array(34.0, 89.0),scala.Array(12.0, 43.0))
        val m = scala.Array( (scala.Array(0,1), (scala.Array(34.0, 88.0), 2))
                           , (scala.Array(0,1), (scala.Array(12.0, 43.0), 2)) )
        val v = scala.Array(77.0, 12.0)

        val res = (new MVMs).extractorTest( (m,v) )
        println(res.mkString("[",", ","]"))
        //assert(res sameElements (v map {a => k*a}) )
      }
    }

    ctx.execute
  }
}
