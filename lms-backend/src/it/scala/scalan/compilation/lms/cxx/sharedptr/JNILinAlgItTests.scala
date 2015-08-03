package scalan
package compilation
package lms
package cxx
package sharedptr

import java.io.File

import scalan.it.BaseItTests
import scalan.linalgebra.{LinearAlgebraExamples, MatricesDslExp, VectorsDslExp}

class JNILinAlgItTests extends BaseItTests{
  class ProgExp extends LinearAlgebraExamples with ScalanCommunityDslExp with GraphVizExport with VectorsDslExp with MatricesDslExp with JNIExtractorOpsExp {

    lazy val ddmvm_jni = fun { p: Rep[JNIType[(Array[Array[Double]], Array[Double])]] =>
      JNI_Pack(ddmvm(JNI_Extract(p)))
    }

    lazy val dsmvm_jni = fun { p: Rep[JNIType[(Array[Array[Double]], (Array[Int], (Array[Double], Int)))]] =>
      JNI_Pack(dsmvm(JNI_Extract(p)))
    }

    lazy val sdmvm_jni = fun { p: Rep[JNIType[(Array[(Array[Int], (Array[Double], Int))], Array[Double])]] =>
      JNI_Pack(sdmvm(JNI_Extract(p)))
    }

    lazy val ssmvm_jni = fun { p: Rep[JNIType[(Array[(Array[Int], (Array[Double], Int))], (Array[Int], (Array[Double], Int)))]] =>
      JNI_Pack(ssmvm(JNI_Extract(p)))
    }

    lazy val fdmvm_jni = fun { p: Rep[JNIType[((Array[Double], Int), Array[Double])]] =>
      JNI_Pack(fdmvm(JNI_Extract(p)))
    }

    lazy val fsmvm_jni = fun { p: Rep[JNIType[((Array[Double], Int), (Array[Int], (Array[Double], Int)))]] =>
      JNI_Pack(fsmvm(JNI_Extract(p)))
    }
  }

  val subfolder = "mvm-cxx"
  val prog = new LmsCompilerCxx with JNIBridge with CommunityBridge with CommunityMethodMappingDSL {
    val scalan = new ProgExp
    val lms = new CommunityCxxShptrLmsBackend
  }
  implicit val cfg = prog.defaultCompilerConfig
  val dir = new File(prefix, subfolder)

  test("ddmvm_jni") {
    prog.buildExecutable(dir, dir, "ddmvm", prog.scalan.ddmvm_jni, GraphVizConfig.default)
  }
  test("dsmvm_jni") {
    prog.buildExecutable(dir, dir, "dsmvm", prog.scalan.dsmvm_jni, GraphVizConfig.default)
  }
  test("sdmvm_jni") {
    prog.buildExecutable(dir, dir, "sdmvm", prog.scalan.sdmvm_jni, GraphVizConfig.default)
  }
  test("ssmvm_jni") {
    prog.buildExecutable(dir, dir, "ssmvm", prog.scalan.ssmvm_jni, GraphVizConfig.default)
  }
  test("fdmvm_jni") {
    prog.buildExecutable(dir, dir, "fdmvm", prog.scalan.fdmvm_jni, GraphVizConfig.default)
  }
  test("fsmvm_jni") {
    prog.buildExecutable(dir, dir, "fsmvm", prog.scalan.fsmvm_jni, GraphVizConfig.default)
  }
}
