package scalan

import java.io.File
import java.lang.reflect.Method
import scala.language.reflectiveCalls

import scalan.arrays.ArrayOps
import scalan.compilation.GraphVizConfig
import scalan.util.FileUtil

/**
 * Created by zotov on 1/17/15.
 */
trait FirstProg { self: Scalan with JNIExtractorOps with ArrayOps =>

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
    val res = ((m map {row => row zip v }) map { row2 => row2 map { p => p._1 * p._2} }) map { r => r.reduce(numericPlusMonoid[Double]) }
    JNI_Pack(res)
  }

  lazy val sdmvm = fun { (in: Rep[JNIType[( Array[( Array[Int], (Array[Double],Int))], Array[Double])]]) =>
    val pin = JNI_Extract(in)
    val sm: Rep[Array[(Array[Int],(Array[Double],Int))]] = pin._1
    val v: Rep[Array[Double]] = pin._2
    val srs:Rep[Array[Array[(Int,Double)]]] = (sm map {srow => (srow._1).zip(srow._2)})
    val res = (srs map {zsrow => zsrow map {p => v(p._1) * p._2} } ).map {r => r.reduce(numericPlusMonoid[Double])}
    JNI_Pack(res)
  }

}

trait PackProg extends FirstProg {
  self: Scalan with JNIExtractorOps with ArrayOps =>

//  lazy val packDouble = fun { (in: Rep[JNIType[Double]]) =>
//    JNI_Extract(in)
//  }

  lazy val packArray = fun { (in: Rep[JNIType[Array[Double]]]) =>
    val v = JNI_Extract(in)
    val v1 = v map {a => a + 1.0}
    val v2 = v1 map {a => a / 3.14}
    val v3 = v1 zip v2
    val v4 = v3 map {p:Rep[(Double,Double)] => p._1*p._2}
    JNI_Pack(v4)
  }

  lazy val packPair = fun {in: Rep[(Int,Double)] =>
    JNI_Pack(in)
  }

//  lazy val extractAndPack = fun {in: Rep[JNIType[(Int,Array[Int])]] =>
//  lazy val extractAndPack = fun {in: Rep[JNIType[(Byte,(Int,Double))]] =>
//  lazy val extractAndPack = fun {in: Rep[JNIType[Array[Double]]] =>
//  lazy val extractAndPack = fun {in: Rep[JNIType[(Int,Double)]] =>
  lazy val extractAndPack = fun {in: Rep[JNIType[( Array[( Array[Int], (Array[Double],Int))], Array[Double])]] =>
    JNI_Pack( JNI_Extract(in) )
  }

  lazy val opyt = fun {(in: Rep[Array[Int]]) =>
    val v1 = in map {a => a + 1}
    //v1 map {a => a}
    SArray.rangeFrom0(v1.length).map{i => v1(i)}
  }
}

class JNIExtractorTests extends BaseCtxTests {

  class Ctx(testName: String) extends TestContext(testName) with JNIExtractorOpsExp

  test("simple") {
    val ctx = new Ctx("firstprog") with FirstProg

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

  test("simplePack") {
    val ctx = new Ctx("testprog") with PackProg

    ctx.emit("packArray", ctx.packArray)
  }
}
