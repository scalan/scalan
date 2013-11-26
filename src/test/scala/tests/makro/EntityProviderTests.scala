/**
 * User: s00747473
 * Date: 11/16/13
 */
package tests.makro

import tests.BaseTests

class EntityProviderTests extends BaseTests {
  import makro.ScalanCodegen._

  val graphsPackage = "graphs"

  def testBaseTrait(entityTemplate: String, expected: String) =  {
    val d = entity(graphsPackage, entityTemplate)
    val p = new EntityFileGenerator(d)
    val code = p.getBaseTrait
    code shouldBe expected
  }

  test("test1") {
    testBaseTrait(
      """trait Edge[V, E] {
        |}""".stripMargin,
      """trait Edges extends ScalanDsl {
        |
        |  trait Edge[V,E] {
        |  }
        |
        |}
      """.stripMargin)
  }
}
