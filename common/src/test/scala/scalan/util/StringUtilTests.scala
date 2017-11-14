package scalan.util

import scalan.BaseNestedTests

class StringUtilTests extends BaseNestedTests {
  import StringUtil._

  describe("StringExtension methods") {
    it("lastComponent") {
      "a/b/c".lastComponent('/') shouldBe("c")
      "a/b/".lastComponent('/') shouldBe("")
      "a".lastComponent('/') shouldBe("a")
      "".lastComponent('/') shouldBe("")
    }
    it("prefixBefore") {
      "a/b/c".prefixBefore("/b") shouldBe("a")
      "a/b/c".prefixBefore("/c") shouldBe("a/b")
      "a/b/c".prefixBefore("a/b/c") shouldBe("")
      "a/b/c".prefixBefore("") shouldBe("")
    }
  }
}
