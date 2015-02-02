package scalan

import java.io.File
import java.lang.reflect.Method
import scala.language.reflectiveCalls

import scalan.arrays.ArrayOps
import scalan.util.FileUtil

/**
 * Created by zotov on 1/17/15.
 */
trait FirstProg { self: Scalan with JNIExtractorOps with ArrayOps =>

  def subfolder = "/firstprog"

  lazy val extractDouble = fun { (in: Rep[JNIType[Double]]) =>
    JNI_Extract(in)
  }

  lazy val extractArray = fun { (in: Rep[JNIType[Array[Double]]]) =>
    JNI_Extract(in)
  }

  lazy val extractDM = fun { (in: Rep[JNIType[Array[Array[Double]]]]) =>
    JNI_Extract(in)
  }

  lazy val extractSM = fun { (in: Rep[JNIType[( Array[( Array[Int], (Array[Double],Int))], Array[Int])]]) =>
    JNI_Extract(in)
  }

  lazy val extractPair = fun { (in: Rep[JNIType[(Array[Double],Double)]]) =>
    JNI_Extract(in)
  }

  lazy val extractPairUse1 = fun { (in: Rep[JNIType[(Array[Double],Double)]]) =>
    val Pair(v,_) = JNI_Extract(in)
    v
  }

  lazy val extractPairUse2 = fun { (in: Rep[JNIType[(Array[Double],Double)]]) =>
    val Pair(_,k) = JNI_Extract(in)
    k
  }

  lazy val ddmvm = fun { (in: Rep[JNIType[(Array[Array[Double]],Array[Double])]]) =>
    val Pair(m,v) = JNI_Extract(in)
    ((m map {row => row zip v }) map { row2 => row2 map { p => p._1 * p._2} }) map { r => r.reduce(numericPlusMonoid[Double]) }
  }

  lazy val sdmvm = fun { (in: Rep[JNIType[( Array[( Array[Int], (Array[Double],Int))], Array[Double])]]) =>
    val pin = JNI_Extract(in)
    val sm: Rep[Array[(Array[Int],(Array[Double],Int))]] = pin._1
    val v: Rep[Array[Double]] = pin._2
    val srs:Rep[Array[Array[(Int,Double)]]] = (sm map {srow => (srow._1).zip((srow._2))})
    (srs map {zsrow => zsrow map {p => v(p._1) * p._2} } ).map {r => r.reduce(numericPlusMonoid[Double])}
  }

}

class JNIExtractorTests extends BaseTests {

  trait TestContext extends ScalanCtxExp with JNIExtractorOpsExp {
    override def isInvokeEnabled(d: Def[_], m: Method) = true
    override def shouldUnpack(e: ViewElem[_, _]) = true
    def subfolder: String
    def emit(name: String, ss: Exp[_]*) =
      emitDepGraph(ss.toList, FileUtil.file(prefix, subfolder, s"/$name.dot") )
  }

  test("simple") {
    val ctx = new TestContext with FirstProg {
      def test() = {

      }
    }

    ctx.test
    ctx.emit("extractDouble", ctx.extractDouble)
    ctx.emit("extractArray", ctx.extractArray)
    ctx.emit("extractDM", ctx.extractDM)
    ctx.emit("extractPair", ctx.extractPair)
    ctx.emit("extractPairUse1", ctx.extractPairUse1)
    ctx.emit("extractPairUse2", ctx.extractPairUse2)
    ctx.emit("extractSM", ctx.extractSM)
    ctx.emit("ddmvm", ctx.ddmvm)
    ctx.emit("sdmvm", ctx.sdmvm)
  }
}
