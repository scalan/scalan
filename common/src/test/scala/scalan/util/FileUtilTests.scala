package scalan.util

import scalan.BaseNestedTests

class FileUtilTests extends BaseNestedTests {
  import FileUtil._
  describe("File traversals") {
    val dir = file("common/src/test/scala")
    it("list all files") {
      listFiles(dir) shouldBe Array(file(dir, "scalan"))
    }
  }
  describe("file path methods") {
    it("extractModuleName") {
      extractModuleName("src/main/scala/d") shouldBe("")
      extractModuleName("/src/main/scala/d") shouldBe("")
      extractModuleName("b/src/main/scala/d") shouldBe("b")
      extractModuleName("a/b/src/main/scala/d") shouldBe("b")
    }
  }
}
