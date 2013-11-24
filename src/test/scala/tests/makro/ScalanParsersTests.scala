/**
 * User: Alexander Slesarenko
 * Date: 11/16/13
 */
package tests.makro

import org.junit.Test
import org.junit.Assert._
import org.hamcrest.core.Is._
import makro.ScalanImpl

class ScalanParsersTests extends {
  import ScalanImpl._
  import ScalanImpl.{
    TpeInt => INT, TpeBoolean => BOOL, TraitCall => TC,
    TraitDef => TD, MethodDef => MD, MethodArg => MA, TpeTuple => T}
  import scala.{List => L}

  def test[T](p: Parser[T], prog: String, expected: T) {
    parseAll(p, prog) match {
      case Success(res, _) => assertEquals(expected, res)
      case NoSuccess(msg, input) => fail(s"$msg (pos: ${input.pos})")
    }
  }

  def testTrait(prog: String, expected: TraitDef) {
    test(traitDef, prog, expected)
  }

  def testTpe(prog: String, expected: TpeExpr) {
    test(tpeExpr, prog, expected)
  }
  def testMethod(prog: String, expected: MethodDef) {
    test(methodDef, prog, expected)
  }

  @Test def testTpeExpr() {
    testTpe("Int", INT)
    testTpe("(Int,Boolean)", TpeTuple(L(INT, BOOL)))
    testTpe("Edge", TC("Edge"))
    testTpe("Edge[V,E]", TC("Edge", L(TC("V"), TC("E"))))
  }

  @Test def testMethodDef() {
    testMethod("def f: Int", MD("f", tpeRes = INT))
    testMethod("def f(x: Int): Int", MD("f", Nil, L(MA("x", INT)), INT))
    testMethod(
      "def f[A <: T](x: A): Int",
      MD("f", L(TpeArg("A", Some(TC("T")))), L(MA("x", TC("A"))), INT))
  }

  @Test def testTraitDef() {
    testTrait("trait A", TD("A"))
    testTrait("trait Edge[V,E]", TD("Edge", L(TpeArg("V"), TpeArg("E"))))
    testTrait("trait Edge[V,E]{}", TD("Edge", L(TpeArg("V"), TpeArg("E"))))
    testTrait("trait Edge[V,E]{}", TD("Edge", L(TpeArg("V"), TpeArg("E"))))
    testTrait("trait Edge[V,E]{ def f[A <: T](x: A, y: (A,T)): Int }",
      TD("Edge", L(TpeArg("V"), TpeArg("E")), Nil,
        body = L(MD("f", L(TpeArg("A", Some(TC("T")))),
                      L(MA("x", TC("A")),MA("y", T(L(TC("A"),TC("T"))))), INT))))
    testTrait(
      """trait A {
        |  def f: (Int,A)
        |  def g(x: Boolean): A
        |}""".stripMargin,
      TD("A", Nil, Nil,
          body = L(
            MD("f", Nil, Nil, T(L(INT,TC("A")))),
            MD("g", Nil, L(MA("x", BOOL)), TC("A")))))

  }

}
