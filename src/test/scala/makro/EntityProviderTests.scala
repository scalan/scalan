/**
 * User: s00747473
 * Date: 11/16/13
 */
package tests.makro

import org.junit.Test
import org.junit.Assert._
import org.hamcrest.core.Is._


class EntityProviderTests extends {
  import scalan.makro.ScalanCodegen._

  val graphsPackage = "graphs"

  def testBaseTrait(entityTemplate: String, expected: String) =  {
    val d = entity(graphsPackage, entityTemplate)
    val p = new EntityFileGenerator(d)
    val code = p.getBaseTrait
    assertEquals(expected, code)
  }

  @Test def test1() {
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
