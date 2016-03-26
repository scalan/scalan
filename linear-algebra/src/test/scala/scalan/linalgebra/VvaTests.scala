package scalan.linalgebra

import scala.language.reflectiveCalls
import scalan._
import scalan.compilation.{DummyCompiler, StructsCompiler}

class VvaTests extends BaseViewTests {

  class Ctx extends TestCompilerContext {

    class ScalanCake extends ScalanDslExp with LinearAlgebraExamples with MatricesDslExp {
      override val cacheElems = false
    }

    override val compiler = new DummyCompiler(new ScalanCake)
      with StructsCompiler[ScalanDslExp with LinearAlgebraExamples]
  }

  test("dd_vva") {
    val ctx = new Ctx
    ctx.test("dd_vva", ctx.compiler.scalan.dd_vva)
  }
  test("ds_vva") {
    val ctx = new Ctx
    ctx.test("ds_vva", ctx.compiler.scalan.ds_vva)
  }
  test("sd_vva") {
    val ctx = new Ctx
    ctx.test("sd_vva", ctx.compiler.scalan.sd_vva)
  }
  test("ss_vva") {
    val ctx = new Ctx
    ctx.test("ss_vva", ctx.compiler.scalan.ss_vva)
  }
}
