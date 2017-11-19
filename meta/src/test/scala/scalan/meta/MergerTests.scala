package scalan.meta

import scalan.meta.ScalanAst._
import scalan.meta.ScalanAstTransformers.{RepTypeRemover, TypeTransformerInAst}

class MergerTests extends ScalanAstTests with Examples {
  val warrays = parseModule(warraysModule)
  val warrays1 = parseModule(warrays1Module)
  val warrays2 = parseModule(warrays2Module)

  describe("Module merging") {
    it("merge wrappers") {
      val merger = new SUnitMerger(warrays1)
      val res = merger.merge(warrays2)
      res shouldBe(warrays)
    }
  }
}
