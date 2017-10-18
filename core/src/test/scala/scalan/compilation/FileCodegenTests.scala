package scalan.compilation

import java.io.PrintWriter

import scalan.{BaseNestedTests, Scalan}

class FileCodegenTests extends BaseNestedTests {
  val config = CodegenConfig("core/generated")
  val gen = new MockFileCodegen(new Scalan, config)
  val name = Name("scalan", "Scalan")
  val otherPackage = Name("scalan2", "Scalan2")
  import gen.importBuilder._

  describe("translateToSrc") {
    it("short name for first Name") {
       val code = gen.translateToSrc(name)
       code shouldEqual("Scalan")
    }
    it("short name for repeated Name") {
      val code = gen.translateToSrc(name)
      code shouldEqual("Scalan")
    }
    it("short name for new imported Name") {
      val code = gen.translateToSrc(otherPackage)
      code shouldEqual("Scalan2")
    }
    it("long name for confilcting names") {
      val newName = Name("scalan", "Scalan2") // assume Scalan2 already imported from scalan2
      val code = gen.translateToSrc(newName)
      code shouldEqual("scalan.Scalan2")
    }
    it("have to implicitly import all translated names") {
      assert(List(name, otherPackage) forall(n => findImportItem(n).isDefined))
    }
  }
}
