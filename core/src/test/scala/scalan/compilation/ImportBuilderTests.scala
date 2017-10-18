package scalan.compilation

import scalan.{BaseNestedTests, Scalan}

class ImportBuilderTests extends BaseNestedTests {
  val config = CodegenConfig("core/generated")
  val gen = new MockFileCodegen(new Scalan, config)
  val name = Name("scalan", "Scalan")
  import gen.importBuilder._

  describe("ImportBuilder") {
    it("adds import for new Name") {
      val added = addImport(name)
      added shouldEqual true
      assert(findImportItem(name).isDefined)
    }
    it("avoid duplicates") {
      val sameName = Name("scalan", "Scalan")
      val added = addImport(sameName)
      added shouldEqual true
      importedItems.size shouldEqual(1)
    }
    it("should find conflict") {
      val fromOtherPackage = Name("scalan2", "Scalan")
      val added = addImport(fromOtherPackage)
      added shouldEqual false
      importedItems.size shouldEqual(1)
    }
    it("should combine names for each package") {
      val samePackage = Name("scalan", "Scalan2")
      val added = addImport(samePackage)
      added shouldEqual true
      importedItems.size shouldEqual(1)
      importedItems("scalan").importedNames.size shouldEqual(2)
    }
    it("should import more than one package") {
      val samePackage = Name("scalan2", "Scalan3")
      val added = addImport(samePackage)
      added shouldEqual true
      importedItems.size shouldEqual(2)
    }
  }
}
