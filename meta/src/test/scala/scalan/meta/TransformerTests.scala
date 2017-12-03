package scalan.meta

import scalan.meta.ScalanAst._
import scalan.meta.ScalanAstExtensions._
import scalan.meta.ScalanAstTransformers.{RepTypeRemover, TypeTransformerInAst}

class TransformerTests extends ScalanAstTests with Examples {
  val colsVirt = parseModule(colsVirtModule)
  val warrays = parseModule(warraysModule)
  context.updateWrapper("Array", WrapperDescr(warrays, Nil, WrapperConfig.default("Array")))
  val b = new SModuleBuilder

  describe("Rep removing") {
    context.addModule(colsVirt)
    val trans = new TypeTransformerInAst(new RepTypeRemover())
    def test(m: SUnitDef, typeIn: SUnitDef => STpeExpr): Unit = {
      val before = typeIn(m)
      val newCols = trans.moduleTransform(m)
      val after = typeIn(newCols)
      after should be(context.RepTypeOf.unapply(before).get)
    }
    it("from method result type") {
      test(colsVirt, m => getMethod(m, "Collection", "length").tpeRes.get)
      test(colsVirt, m => getMethod(m, "Collection", "apply").tpeRes.get)
    }
    it("from method arg") {
      test(colsVirt, m => getMethod(m, "Collection", "apply").allArgs(0).tpe)
    }
    it("from class arg") {
      test(colsVirt, m => m.getEntity("ColOverArray").args.args(0).tpe)
    }
    it("from class val") {
      test(colsVirt, m => getVal(m, "ColOverArray", "list").tpe.get)
    }
  }

  describe("Unit transforms") {
    it("allEntitiesSorted") {
      val es = colsVirt.allEntitiesSorted.map(e => e.name)
      es shouldBe(List("Collection", "ColOverArray"))
    }

    it("addDefAncestorToAllEntities doesn't change virtualized unit") {
      val newCols = b.addDefAncestorToAllEntities(colsVirt)
      newCols.allEntitiesSorted shouldBe(colsVirt.allEntitiesSorted)
    }

    it("ModuleVirtualizationPipeline") {
      val cols = parseModule(colsModule)
      val p = new ModuleVirtualizationPipeline
      val virt = p(cols)
      val opt = optimizeModuleImplicits(virt)
      val expected = colsVirt.allEntitiesSorted.map(_.name)
      opt.allEntitiesSorted.map(_.name) shouldBe(expected)
// TODO     opt.allEntitiesSorted shouldBe(colsVirt.allEntitiesSorted)
    }

    it("addDefAncestorToAllEntities") {
      val cols = parseModule(colsModule)
      val res = b.addDefAncestorToAllEntities(cols)
      val as = res.allEntitiesSorted.map(e => e.ancestors)
      as shouldBe colsVirt.allEntitiesSorted.map(e => e.ancestors)
      res.allEntities forall { _.isInherit("Def") } shouldBe(true)
    }
  }
}
