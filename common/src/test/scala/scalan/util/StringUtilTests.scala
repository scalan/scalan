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
    it("replaceSuffix") {
      "a.old".replaceSuffix(null, ".new") shouldBe("a.old")
      (null: String).replaceSuffix(".old", ".new") shouldBe(null)
      "a.old".replaceSuffix("", ".new") shouldBe("a.old")
      "a.".replaceSuffix(".old", ".new") shouldBe("a.")
      "a.old".replaceSuffix(".old", ".new") shouldBe("a.new")
      "a.old".replaceSuffix(".old", "") shouldBe("a")
      "a.old".replaceSuffix(".old", null) shouldBe("a")
    }
  }
}
