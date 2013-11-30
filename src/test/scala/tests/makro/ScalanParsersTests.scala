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
    TpeInt => INT, TpeBoolean => BOOL, TpeFloat => FLOAT, TraitCall => TC,
    TraitDef => TD, MethodDef => MD, MethodArg => MA, TpeTuple => T,
    EntityModuleDef => EMD, ImportStat => IS}
  import scala.{List => L}

  def test[T](p: Parser[T], prog: String, expected: T) {
    parseAll(p, prog) match {
      case Success(res, _) => {
        //println(res)
        assertEquals(expected, res)
      }
      case NoSuccess(msg, input) => fail(s"$msg (pos: ${input.pos})")
    }
  }

  def testModule(prog: String, expected: EntityModuleDef) {
    test(entityModuleDef, prog, expected)
  }

  def testTrait(prog: String, expected: TraitDef) {
    test(traitDef, prog, expected)
  }
  def testClass(prog: String, expected: ClassDef) {
    test(classDef, prog, expected)
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
    testTpe("Int=>Boolean", TpeFunc(L(INT, BOOL)))
    testTpe("Int=>Boolean=>Float", TpeFunc(L(INT, BOOL, FLOAT)))
    testTpe("(Int,Boolean=>Float)", TpeTuple(L(INT, TpeFunc(L(BOOL, FLOAT)))))
    testTpe("(Int,(Boolean=>Float))", TpeTuple(L(INT, TpeFunc(L(BOOL, FLOAT)))))
    testTpe("(Int,Boolean)=>Float", TpeFunc(L(TpeTuple(L(INT,BOOL)), FLOAT)))
    testTpe("Edge", TC("Edge"))
    testTpe("Edge[V,E]", TC("Edge", L(TC("V"), TC("E"))))
    testTpe("Rep[A=>B]", TC("Rep", L(TpeFunc(L(TC("A"), TC("B"))))))
  }

  def f[A <: Int : Numeric : Fractional](x: A): Int = ???

  @Test def testMethodDef() {
    testMethod("def f: Int", MD("f", tpeRes = INT))
    testMethod("implicit def f: Int", MD("f", Nil, Nil, INT, true))
    testMethod("def f(x: Int): Int", MD("f", Nil, L(MA("x", INT)), INT))
    testMethod(
      "def f[A <: T](x: A): Int",
      MD("f", L(TpeArg("A", Some(TC("T")))), L(MA("x", TC("A"))), INT))
    testMethod(
      "def f[A : Numeric](x: A): Int",
      MD("f", L(TpeArg("A", None, L("Numeric"))), L(MA("x", TC("A"))), INT))
    testMethod(
      "def f[A <: Int : Numeric : Fractional](x: A): Int",
      MD("f", L(TpeArg("A", Some(INT), L("Numeric","Fractional"))), L(MA("x", TC("A"))), INT))
  }

  @Test def testTraitDef() {
    testTrait("trait A", TD("A"))
    testTrait("trait A extends B", TD("A", Nil, List(TC("B"))))
    testTrait("trait A extends B with C", TD("A", Nil, List(TC("B"), TC("C"))))
    testTrait("trait Edge[V,E]", TD("Edge", L(TpeArg("V"), TpeArg("E"))))
    testTrait("trait Edge[V,E]{}", TD("Edge", L(TpeArg("V"), TpeArg("E"))))
    testTrait("trait Edge[V,E]{}", TD("Edge", L(TpeArg("V"), TpeArg("E"))))
    testTrait("trait Edge[V,E]{ def f[A <: T](x: A, y: (A,T)): Int }",
      TD("Edge", L(TpeArg("V"), TpeArg("E")), Nil,
        body = L(MD("f", L(TpeArg("A", Some(TC("T")))),
                      L(MA("x", TC("A")),MA("y", T(L(TC("A"),TC("T"))))), INT))))
    testTrait(
      """trait A {
        |  import scalan._
        |  type Rep[A] = A
        |  def f: (Int,A)
        |  def g(x: Boolean): A
        |}""".stripMargin,
      TD("A", Nil, Nil,
          body = L(
            IS(L("scalan","_")),
            TpeDef("Rep", L(TpeArg("A")),TC("A")),
            MD("f", Nil, Nil, T(L(INT,TC("A")))),
            MD("g", Nil, L(MA("x", BOOL)), TC("A")))))

  }

  val reactiveTrait =
    """trait Reactive extends ScalanDsl {
      |  type Obs[A] = Rep[Observable[A]]
      |  trait Observable[A] {
      |    implicit def eA: Elem[A]
      |  }
      |  class ObservableImpl[A](implicit val eA: Elem[A]) extends Observable[A] {
      |  }
      |}
    """.stripMargin

  val reactiveModule =
    """package scalan.rx
      |import scalan._
      |trait Reactive extends ScalanDsl {
      |  type Obs[A] = Rep[Observable[A]]
      |  trait Observable[A] {
      |    implicit def eA: Elem[A]
      |  }
      |  class ObservableImpl1[A](implicit val eA: Elem[A]) extends Observable[A] {
      |  }
      |  class ObservableImpl2[A](implicit val eA: Elem[A]) extends Observable[A] {
      |  }
      |}
    """.stripMargin

  @Test def testClassDef() {
    testClass("class A", ClassDef("A"))
    testClass("class A extends B", ClassDef("A", Nil,Nil,Nil, L(TC("B"))))
    testClass("class A extends B with C", ClassDef("A", Nil,Nil,Nil, L(TC("B"), TC("C"))))
    testClass("class Edge[V,E]", ClassDef("Edge", L(TpeArg("V"), TpeArg("E"))))
    testClass("class Edge[V,E](val x: V){ def f[A <: T](x: A, y: (A,T)): Int }",
      ClassDef("Edge", L(TpeArg("V"), TpeArg("E")), L(ClassArg(false, false, true, "x",TC("V"))),Nil,Nil,
        L(MD("f", L(TpeArg("A", Some(TC("T")))),
          L(MA("x", TC("A")),MA("y", T(L(TC("A"),TC("T"))))), INT))))
  }

  @Test def testEntityModuleDef() {
    testModule(
      reactiveModule,
      EMD("scalan.rx", "Reactive",
        TpeDef("Obs", L(TpeArg("A")), TC("Rep",L(TC("Observable", L(TC("A")))))),
        TD("Observable", L(TpeArg("A")), Nil,
          L(MD("eA",Nil,Nil,TC("Elem",L(TC("A"))),isImplicit = true))),
        L(ClassDef("ObservableImpl1", L(TpeArg("A")), L(ClassArg(true, false, true, "eA",TC("Elem",L(TC("A"))))),Nil,
          L(TC("Observable",L(TC("A")))),Nil ),
          ClassDef("ObservableImpl2", L(TpeArg("A")), L(ClassArg(true, false, true, "eA",TC("Elem",L(TC("A"))))),Nil,
            L(TC("Observable",L(TC("A")))),Nil )),
        Some("ScalanDsl")))
  }

}
