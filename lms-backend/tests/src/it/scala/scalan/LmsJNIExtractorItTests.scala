package scalan

import scalan.compilation.lms.JNILmsBridge
import scalan.compilation.lms.cxx.LmsCompilerCxx
import scalan.graphs.MST_example
import scalan.it.BaseCtxItTests

trait JNIMst extends MST_example with JNIExamples with JNIExtractorOps {
  lazy val MST_JNI_adjlist = JNI_Wrap(MST_adjlist)

  lazy val MST_JNI_adjmatrix = JNI_Wrap(MST_adjmatrix)

  lazy val MSF_JNI_adjlist = JNI_Wrap(MSF_adjlist)

  lazy val MSF_JNI_adjmatrix = JNI_Wrap(MSF_adjmatrix)

  lazy val MSF_JNI_adjlistList = JNI_Wrap(MSF_adjlistList)

  lazy val MSF_JNI_adjmatrixList = JNI_Wrap(MSF_adjmatrixList)
}

class LmsJNIExtractorItTests extends BaseCtxItTests[JNIMst](???) {
  class ProgExp extends ScalanDslExp with JNIExtractorOpsExp with JNIMst

  val compiler = new LmsCompilerCxx(new ProgExp) with JNILmsBridge

  val defaultCompilers = compilers(compiler)

  class Ctx[T <: ProgExp]/*(val _scalan: T)*/ extends TestCompilerContext {
    val compiler = LmsJNIExtractorItTests.this.compiler
    // val compiler = new LmsCompilerCxx[_scalan.type](_scalan) with JNIBridge
  }

  test("MST_JNI") {
    val ctx = new Ctx

    //    ctx.test("MST_JNI_adjlist", ctx.MST_JNI_adjlist)
    //    ctx.test("MST_JNI_adjmatrix", ctx.MST_JNI_adjmatrix)
    ctx.test("MSF_JNI_adjlist", ctx.compiler.scalan.MSF_JNI_adjlist)
    ctx.test("MSF_JNI_adjmatrix", ctx.compiler.scalan.MSF_JNI_adjmatrix)
    ctx.test("MSF_JNI_adjlistList", ctx.compiler.scalan.MSF_JNI_adjlistList)
    ctx.test("MSF_JNI_adjmatrixList", ctx.compiler.scalan.MSF_JNI_adjmatrixList)
  }

  test("simpleGenCxx") {
    val ctx = new Ctx

    ctx.test("extractDouble", ctx.compiler.scalan.extractDouble)
    ctx.test("extractPairUse2", ctx.compiler.scalan.extractPairUse2)
    ctx.test("ddmvm", ctx.compiler.scalan.ddmvm)
    ctx.test("sdmvm", ctx.compiler.scalan.sdmvm)
  }

  test("JNI_Extract failing examples") {
    // incorrect code generated, type mismatches between boost::container::vector<A> and jni_array<A>
    pending
    val ctx = new Ctx
    ctx.test("extractArray", ctx.compiler.scalan.extractArray)
    ctx.test("extractDM", ctx.compiler.scalan.extractDM)
    ctx.test("extractPair", ctx.compiler.scalan.extractPair)
    ctx.test("extractPairUse1", ctx.compiler.scalan.extractPairUse1)
    ctx.test("extractSM", ctx.compiler.scalan.extractSM)
  }

  test("simplePackGenCxx") {
    val ctx = new Ctx

    //    ctx.test("opyt", ctx.opyt)
    //    ctx.test("packArray", ctx.packArray)
    ctx.test("extractAndPack", ctx.compiler.scalan.extractAndPack)
    ctx.test("packPair", ctx.compiler.scalan.packPair)
  }

  test("mvm") {
    val ctx = new Ctx

    //    ctx.test("opyt", ctx.opyt)
    ctx.test("ddmvm", ctx.compiler.scalan.ddmvm)
    ctx.test("sdmvm", ctx.compiler.scalan.sdmvm)
    //    ctx.test("packPair", ctx.packPair)
  }
}
