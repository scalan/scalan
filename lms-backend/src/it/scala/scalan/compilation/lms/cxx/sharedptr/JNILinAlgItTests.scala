package scalan
package compilation
package lms
package cxx
package sharedptr

import java.io.File

import scalan.it.BaseItTests
import scalan.linalgebra.{LinearAlgebraExamples, MatricesDslExp, VectorsDslExp}

class JNILinAlgItTests extends BaseItTests{
  abstract class Prog extends LinearAlgebraExamples with ScalanCommunityDsl with JNIExtractorOps {
    lazy val ddmvm_jni = JNI_Wrap(ddmvm)

    lazy val dsmvm_jni = JNI_Wrap(dsmvm)

    lazy val sdmvm_jni = JNI_Wrap(sdmvm)

    lazy val ssmvm_jni = JNI_Wrap(ssmvm)

    lazy val fdmvm_jni = JNI_Wrap(fdmvm)

    lazy val fsmvm_jni = JNI_Wrap(fsmvm)
  }

  class ProgExp extends Prog with ScalanCommunityDslExp with JNIExtractorOpsExp

  val prog = new LmsCompilerCxx(new ProgExp) with JNIBridge with CommunityBridge with CommunityMethodMappingDSL
  val defaultCompilers = compilers(prog)
  // unused
  lazy val progSeq = ???

  implicit val cfg = prog.defaultCompilerConfig
  val dir = new File(prefix, "mvm-cxx")

  test("ddmvm_jni") {
    // doesn't compile yet (similar below)
    // compileSource(_.ddmvm_jni, "ddmvm")
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
