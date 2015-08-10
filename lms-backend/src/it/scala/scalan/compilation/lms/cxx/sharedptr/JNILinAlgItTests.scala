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

    lazy val ddmvm_jni = JNI_Wrap(ddmvm)

    lazy val dsmvm_jni = JNI_Wrap(dsmvm)

    lazy val sdmvm_jni = JNI_Wrap(sdmvm)

    lazy val ssmvm_jni = JNI_Wrap(ssmvm)

    lazy val fdmvm_jni = JNI_Wrap(fdmvm)

    lazy val fsmvm_jni = JNI_Wrap(fsmvm)
  }

  val subfolder = "mvm-cxx"
  val prog = new LmsCompilerCxx(new ProgExp) with JNIBridge with CommunityBridge with CommunityMethodMappingDSL

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
