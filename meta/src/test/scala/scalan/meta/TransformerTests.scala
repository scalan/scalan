package scalan.meta

import scalan.meta.ScalanAst._
import scalan.meta.ScalanAstTransformers.{TypeTransformerInAst, RepTypeRemover}

class TransformerTests extends ScalanAstTests with Examples {
  val m = parseModule(reactiveModule)
  context.addModule(m)

  describe("Rep removing") {
    def test(t: STpeExpr, expected: Option[(String, List[STpeExpr])]): Unit = {
      context.TypeSynonim.unapply(t) should be(expected)
    }
    it("from method result type") {
      val trans = new TypeTransformerInAst(new RepTypeRemover())
      test(STraitCall("Obs", List(TpeInt)), Some(("Observable", List(TpeInt))))
      test(STraitCall("Iso", List(TpeInt, TpeInt)), Some(("IsoUR", List(TpeInt, TpeInt))))
      test(STraitCall("Conv", List(TpeInt, TpeInt)), Some(("Converter", List(TpeInt, TpeInt))))
    }

  }

}
