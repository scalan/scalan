package scalan.compilation.lms.cxx.sharedptr

import java.io.File

import scalan.compilation.lms.linalgebra.{LinAlgBridge, LinAlgCxxShptrLmsBackend}
import scalan.{JNIExtractorOps, JNIExtractorOpsExp}
import scalan.compilation.GraphVizConfig
import scalan.compilation.lms.JNIBridge
import scalan.compilation.lms.cxx.LmsCompilerCxx
import scalan.it.BaseItTests
import scalan.linalgebra._

trait JNILinAlgProg extends LinearAlgebraExamples with JNIExtractorOps {
  lazy val ddmvm_jni = JNI_Wrap(ddmvm)

  lazy val dsmvm_jni = JNI_Wrap(dsmvm)

  lazy val sdmvm_jni = JNI_Wrap(sdmvm)

  lazy val ssmvm_jni = JNI_Wrap(ssmvm)

  lazy val fdmvm_jni = JNI_Wrap(fdmvm)

  lazy val fsmvm_jni = JNI_Wrap(fsmvm)
}

class JNILinAlgItTests extends BaseItTests[JNILinAlgProg](???) {

  class ProgExp extends MatricesDslExp with JNILinAlgProg with JNIExtractorOpsExp

  val prog = new LmsCompilerCxx(new ProgExp) with JNIBridge with LinAlgBridge {
    override val lms = new LinAlgCxxShptrLmsBackend(this)
  }
  implicit val cfg = prog.defaultCompilerConfig

  val defaultCompilers = compilers(prog)

  val dir = new File(prefix, "mvm-cxx")

  test("ddmvm_jni") {
    // doesn't compile yet (similar below)
    // compileSource(_.ddmvm_jni)
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
