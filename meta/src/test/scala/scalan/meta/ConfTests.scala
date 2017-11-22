package scalan.meta

class ConfTests extends ScalanAstTests with Examples {
  val warrays = parseModule(warraysModule)
  val warrays1 = parseModule(warrays1Module)
  val warrays2 = parseModule(warrays2Module)
  describe("Module dependencies") {
    it("collectInputModules") {
      val warrayModule = new SourceModuleConf("warrays")
          .addUnit("WArrays.scala", "scala/WArrays.scala")
      val apiModule = new SourceModuleConf("library-api")
          .addUnit("Cols.scala", "scalan/collection/Cols.scala")
          .dependsOn(warrayModule)
      val implModule = new SourceModuleConf("library-impl")
          .addUnit("ColsOverArrays.scala", "scalan/collection/ColsOverArrays.scala")
          .dependsOn(apiModule, warrayModule)
      implModule.collectInputModules() shouldBe (Set(warrayModule, apiModule))
    }
  }
}
