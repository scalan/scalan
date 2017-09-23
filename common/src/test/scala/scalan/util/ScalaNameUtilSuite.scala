package scalan.util

import scalan.BaseTests

class ScalaNameUtilSuite extends BaseTests {
  def +\() = ???

  import ScalaNameUtil._

  test("Operator names should be decoded correctly") {
    cleanScalaName("$plus$bslash$up") shouldEqual("""+\^""")
  }

  test("Method names obtained by reflection should be decoded") {
    val methodNames = classOf[ScalaNameUtilSuite].getDeclaredMethods.map {
      m => cleanScalaName(m.getName)
    }.toList

    methodNames should equal(List("""+\"""))
  }

  test("extract package and name") {
    val name = "com.my.Class"
    PackageAndName.unapply(name) should equal(Some((List("com", "my"), "Class")))
  }
}
