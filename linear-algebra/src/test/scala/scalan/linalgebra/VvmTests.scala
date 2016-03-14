package scalan.linalgebra

import scala.language.reflectiveCalls
import scalan._
import scalan.common.{SegmentsDslStd, SegmentsDsl, SegmentsDslExp, Lazy}
import scalan.compilation.{StructsCompiler, DummyCompiler}
import scalan.it.BaseItTests

class VvmTests extends BaseViewTests {

  class Ctx extends TestCompilerContext {

    class ScalanCake extends ScalanDslExp with LinearAlgebraExamples with MatricesDslExp {
      override val cacheElems = false
    }

    override val compiler = new DummyCompiler(new ScalanCake)
      with StructsCompiler[ScalanDslExp with LinearAlgebraExamples]
  }

  test("dd_vvm") {
    val ctx = new Ctx
    import ctx.compiler.scalan._
    ctx.test("dd_vvm", dd_vvm)
  }

  test("ds_vvm") {
    val ctx = new Ctx
    import ctx.compiler.scalan._
    ctx.test("ds_vvm", ctx.compiler.scalan.ds_vvm)
  }

  test("sd_vvm") {
    val ctx = new Ctx
    import ctx.compiler.scalan._
    ctx.test("sd_vvm", ctx.compiler.scalan.sd_vvm)
  }

  test("ss_vvm") {
    val ctx = new Ctx
    import ctx.compiler.scalan._
    ctx.test("ss_vvm", ctx.compiler.scalan.ss_vvm)
  }
}
