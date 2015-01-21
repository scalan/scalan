package scalan.it.lms

import java.io.File

import scala.language.reflectiveCalls
import scalan.performance.MVMs
import scalan.{ScalanCtxExp, FirstProg, BaseTests}
import scalan.community.ScalanCommunityExp
import scalan.compilation.GraphVizExport
import scalan.compilation.lms._
import scalan.linalgebra.{MatricesDslExp, VectorsDslExp}
import scalan.parrays.PArraysDslExp

/**
 * Created by zotov on 1/19/15.
 */
class LmsJNIExtractorItTests extends BaseTests {
  val prefix = new File("it-out/LmsJNI/")

  trait ProgExp extends PArraysDslExp with ScalanCommunityExp with GraphVizExport with LmsCompilerCXX with VectorsDslExp with MatricesDslExp { self =>
    def makeBridge[A, B] = new LmsBridge[A, B] with CommunityBridge[A, B] with LinalgBridge[A, B] with JNIBridge[A,B] {
      val scalan = self
    }
  }

  test("simpleGenCxx") {
    val ctx = new ScalanCtxExp with ProgExp with FirstProg {
      override def subfolder: String = super.subfolder + "-cxx"
      def test() = {

      }

      def generate[A,B](name: String, f: Exp[A => B]): Unit = {
        val dir = new File(prefix, subfolder)
        generate(dir, dir, name, f, false)
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
  }

  test("simpleBuildExecutable") {
    val ctx = new ScalanCtxExp with ProgExp with FirstProg {
      override def subfolder: String = super.subfolder + "-build"

      def test() = {
      }

      def buildExecutable[A,B](name: String, f: Exp[A => B]): Unit = {
        val dir = new File(prefix, subfolder)
        buildExecutable(dir, dir, name, f, false)
      }
    }

    ctx.buildExecutable("extractDouble", ctx.extractDouble)
  }

  test("simpleExecute") {
    val ctx = new ScalanCtxExp with ProgExp with FirstProg {
      override def subfolder: String = "release"

      def test() = {
      }

      def execute(): Unit = {
        val dir = new File(prefix, subfolder)
//        System.setProperty("java.library.path", dir.getAbsolutePath) //TODO: is there a way to set path dynamically?
        println(s"java.library.path=${System.getProperty("java.library.path")}")
        System.loadLibrary("jniExtractor")

        val m = scala.Array(scala.Array(34.0, 89.0),scala.Array(12.0, 43.0))
        val v = scala.Array(77.0, 12.0)

        val res = (new MVMs).extractorTest( (m,v) )
        println(res.mkString("[",", ","]"))
        //assert(res sameElements (v map {a => k*a}) )
      }
    }

    ctx.execute
  }
}
