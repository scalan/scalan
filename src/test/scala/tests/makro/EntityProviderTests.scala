/**
 * User: s00747473
 * Date: 11/16/13
 */
package tests.makro

import org.junit.Test
import org.junit.Assert._
import org.hamcrest.core.Is._


class EntityProviderTests extends {
  import makro.ScalanCodegen._

  val graphsPackage = "graphs"

  def testSeqTrait(entityTemplate: String, expected: String) =  {
    val d = parseEntityModule(entityTemplate)
    val p = new EntityFileGenerator(d)
    val code = p.getSeqTrait
    assertEquals(expected, code)
  }

  @Test def testSeq() {
    testSeqTrait(
      """package scalan.rx
        |import scalan._
        |import scalan.common.Common
        |trait Reactive extends ScalanDsl {
        |  type Obs[A] = Rep[Observable[A]]
        |  trait Observable[A] {
        |    implicit def eA: Elem[A]
        |    def value: Rep[A]
        |    def index: Rep[Int]
        |    def completed: Rep[Boolean]
        |    def map[B:Elem](f: Rep[A=>B]): Obs[B]
        |    def zip[B](that: Obs[B]): Obs[(A, B)]
        |  }
        |  class ObservableImpl[A]
        |      (val value: Rep[A], val index: Rep[Int], val completed: Rep[Boolean])(implicit val eA: Elem[A]) extends Observable[A] {
        |  }
        |}
      """.stripMargin,
      """trait Edges extends ScalanDsl {
        |
        |  trait Edge[V,E] {
        |  }
        |
        |}
      """.stripMargin)
  }

}
