package scalan.util

import scalan.BaseTests

class ScalaNameUtilSuite extends BaseTests {
  def +\() = ???

  test("Operator names should be decoded correctly") {
    ScalaNameUtil.cleanScalaName("$plus$bslash$up") shouldEqual("""+\^""")
  }

  test("Method names obtained by reflection should be decoded") {
    val methodNames = classOf[ScalaNameUtilSuite].getDeclaredMethods.map {
      m => ScalaNameUtil.cleanScalaName(m.getName)
    }.toList

    methodNames should equal(List("""+\"""))
  }
}
