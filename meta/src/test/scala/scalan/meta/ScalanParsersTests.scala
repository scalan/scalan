package scalan.meta

import scalan.BaseNestedTests
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.Global
import scalan.meta.ScalanAst._

class ScalanParsersTests extends ScalanAstTests with Examples {

  import compiler._
  import scalan.meta.ScalanAst.{STraitCall => TC, SModuleDef => EMD, SClassDef => CD, STpeTuple => T, SMethodArg => MA, STraitDef => TD, SMethodDef => MD, SMethodArgs => MAs, SImportStat => IS}
  import scala.{List => L}

  describe("STpeExpr") {
    implicit val ctx = new ParseCtx(true)
    testSTpe("Int", INT)
    testSTpe("(Int,Boolean)", STpeTuple(L(INT, BOOL)))
    testSTpe("Int=>Boolean", STpeFunc(INT, BOOL))
    testSTpe("Int=>Boolean=>Float", STpeFunc(INT, STpeFunc(BOOL, FLOAT)))
    testSTpe("(Int,Boolean=>Float)", STpeTuple(L(INT, STpeFunc(BOOL, FLOAT))))
    testSTpe("(Int,(Boolean=>Float))", STpeTuple(L(INT, STpeFunc(BOOL, FLOAT))))
    testSTpe("(Int,Boolean)=>Float", STpeFunc(STpeTuple(L(INT, BOOL)), FLOAT))
    testSTpe("Edge", TC("Edge", Nil))
    testSTpe("Edge[V,E]", TC("Edge", L(TC("V", Nil), TC("E", Nil))))
    testSTpe("Rep[A=>B]", TC("Rep", L(STpeFunc(TC("A", Nil), TC("B", Nil)))))
  }

  describe("SMethodDef") {
    implicit val ctx = new ParseCtx(true)
    testSMethod("def f: Int", MD("f", Nil, Nil, Some(INT), false, false, None, Nil, None))
    testSMethod("@OverloadId(\"a\") implicit def f: Int", MD("f", Nil, Nil, Some(INT), true, false, Some("a"), L(SMethodAnnotation("OverloadId",List(SConst("a")))), None))
    testSMethod(
      "def f(x: Int): Int",
      MD("f", Nil, L(MAs(List(MA(false, false, "x", INT, None)))), Some(INT), false, false, None, Nil, None))
    testSMethod(
      "def f[A <: T](x: A): Int",
      MD("f", L(STpeArg("A", Some(TC("T", Nil)), Nil)), L(MAs(L(MA(false, false, "x", TC("A", Nil), None)))), Some(INT), false, false, None, Nil, None))
    testSMethod(
      "def f[A : Numeric]: Int",
      MD("f", L(STpeArg("A", None, L("Numeric"))), Nil, Some(INT), false, false, None, Nil, None))
    testSMethod(
      "def f[A <: Int : Numeric : Fractional](x: A)(implicit y: A): Int",
      MD(
        "f",
        L(STpeArg("A", Some(INT), L("Numeric", "Fractional"))),
        L(MAs(L(MA(false, false, "x", TC("A", Nil), None))), MAs(L(MA(true, false, "y", TC("A", Nil), None)))),
        Some(INT), false, false, None, Nil, None))
  }

  describe("TraitDef") {
    implicit val ctx = new ParseCtx(true)
    val traitA = TD("A", Nil, Nil, Nil, None, None)
    val traitEdgeVE = TD("Edge", L(STpeArg("V", None, Nil), STpeArg("E", None, Nil)), Nil, Nil, None, None)

    testTrait("trait A", traitA)
    testTrait("trait A extends B",
      traitA.copy(ancestors = L(TC("B", Nil).toTypeApply)))
    testTrait("trait A extends B with C",
      traitA.copy(ancestors = L(TC("B", Nil).toTypeApply, TC("C", Nil).toTypeApply)))
    testTrait("trait Edge[V,E]", traitEdgeVE)
    testTrait("trait Edge[V,E]{}", traitEdgeVE)
    testTrait("trait Edge[V,E]{ def f[A <: T](x: A, y: (A,T)): Int }",
      traitEdgeVE.copy(
        body = L(MD("f", L(STpeArg("A", Some(TC("T", Nil)), Nil)),
          L(MAs(L(MA(false, false, "x", TC("A", Nil), None), MA(false, false, "y", T(L(TC("A", Nil), TC("T", Nil))), None)))),
          Some(INT), false, false, None, Nil, None))))
    testTrait(
      """trait A {
        |  import scalan._
        |  type Rep[A] = A
        |  def f: (Int,A)
        |  @OverloadId("b")
        |  def g(x: Boolean): A
        |}""".stripMargin,
      TD("A", Nil, Nil, L(
        IS("scalan._"),
        STpeDef("Rep", L(STpeArg("A", None, Nil)), TC("A", Nil)),
        MD("f", Nil, Nil, Some(T(L(INT, TC("A", Nil)))), false, false, None, Nil, None),
        MD("g", Nil, L(MAs(L(MA(false, false, "x", BOOL, None)))), Some(TC("A", Nil)), false, false, Some("b"), L(SMethodAnnotation("OverloadId",List(SConst("b")))), None)), None, None))

  }

  val reactiveTrait =
    """trait Reactive extends Scalan {
      |  type Obs[A] = Rep[Observable[A]]
      |  trait Observable[A] {
      |    implicit def eA: Elem[A]
      |  }
      |  class ObservableImpl[A](implicit val eA: Elem[A]) extends Observable[A] {
      |  }
      |}
    """.stripMargin

  describe("SClassDef") {
    implicit val ctx = new ParseCtx(true)
    val classA =
      CD("A", Nil, SClassArgs(Nil), SClassArgs(Nil), Nil, Nil, None, None, false)
    val classEdgeVE =
      CD("Edge", L(STpeArg("V", None, Nil), STpeArg("E", None, Nil)), SClassArgs(Nil), SClassArgs(Nil), Nil, Nil, None, None, false)
    testSClass("class A", classA)
    testSClass("class A extends B",
      classA.copy(ancestors = L(TC("B", Nil).toTypeApply)))
    testSClass("class A extends B with C",
      classA.copy(ancestors = L(TC("B", Nil).toTypeApply, TC("C", Nil).toTypeApply)))
    testSClass("class Edge[V,E]", classEdgeVE)
    testSClass("class Edge[V,E](val x: V){ def f[A <: T](x: A, y: (A,T)): Int }",
      classEdgeVE.copy(
        args = SClassArgs(L(SClassArg(false, false, true, "x", TC("V", Nil), None))),
        body = L(MD("f", L(STpeArg("A", Some(TC("T", Nil)), Nil)),
          L(MAs(L(MA(false, false, "x", TC("A", Nil), None), MA(false, false, "y", T(L(TC("A", Nil), TC("T", Nil))), None)))),
          Some(INT), false, false, None, Nil, None))))
  }

  describe("SModuleDef") {
    implicit val ctx = new ParseCtx(true)
    val tpeArgA = L(STpeArg("A", None, Nil))
    val ancObsA = L(TC("Observable", L(TC("A", Nil))))
    val argEA = L(SClassArg(true, false, true, "eA", TC("Elem", L(TC("A", Nil))), None, Nil, true))
    val entity = TD("Observable", tpeArgA, Nil, L(SMethodDef("eA",List(),List(),Some(TC("Elem",L(TC("A",Nil)))),true,false, None, Nil, None, true)), None, None)
    val obsImpl1 = CD("ObservableImpl1", tpeArgA, SClassArgs(Nil), SClassArgs(argEA), ancObsA.map(_.toTypeApply), Nil, None, None, false)
    val obsImpl2 = obsImpl1.copy(name = "ObservableImpl2")

    testModule(reactiveModule,
      EMD("scalan.rx", L(SImportStat("scalan._")), reactiveModule.moduleName,
        List(STpeDef("Obs", L(STpeArg("A",None,Nil)) , TC("Rep", ancObsA))),
        List(entity),
        L(obsImpl1, obsImpl2),
        Nil,
        None,
//        stdDslImpls = Some(SDeclaredImplementations(Map())),
//        expDslImpls = Some(SDeclaredImplementations(Map())),
        ancestors = L(STraitCall("Scalan", Nil).toTypeApply), None, true))
  }

  describe("AstContext methods") {
    val m = parseModule(reactiveModule)
    context.addModule(m)
    val cols = parseModule(colsVirtModule)
    context.addModule(cols)
    val warrays = parseModule(warraysModule)
    context.addModule(warrays)
    val itersApi = parseModule(itersApiModule)
    context.addModule(itersApi)
    val itersImpl = parseModule(itersImplModule)
    context.addModule(itersImpl)

    it("recognize type synonym") {
      def test(t: STpeExpr): Unit = {
        context.TypeDef.unapply(t) should matchPattern { case Some(_) => }
      }
      test(STraitCall("Obs", List(TpeInt)))
      test(STraitCall("Col", List(TpeString)))
      test(STraitCall("RepWArray", List(TpeString)))
      context.TypeDef.unapply(STraitCall("RepIter", List(TpeString))) should matchPattern { case None => }
    }

    it("recognize Rep type") {
      def test(t: STpeExpr, expected: Option[STpeExpr]): Unit = {
        context.RepTypeOf.unapply(t) should be(expected)
      }
      test(TpeInt, None)
      test(STraitCall("Elem", List(TpeInt)), None)
      test(STraitCall("Rep", List(TpeInt)), Some(TpeInt))
      test(STraitCall("RFunc", List(TpeInt, TpeString)), Some(STpeFunc(TpeInt, TpeString)))
    }

    it("resolve Entity by name") {
      List("Observable", "Collection", "WArray",
           "Iter", "IterBuilder", "IterOverArray", "IterOverArrayBuilder") foreach { en =>
        en should matchPattern { case m.context.ModuleEntity(_, e) if e.name == en => }
      }
    }

    it("resolve recognize wrapper entity by name") {
      "WArray" should matchPattern { case m.WrapperEntity(e, "Array") if e.name == "WArray" => }
      "Collection" shouldNot matchPattern { case m.WrapperEntity(e, _) => }
    }
  }

  def testPath(module: SModuleDef, tpeString: String, name: String, expected: STpeExpr => Option[STpePath]): Unit = {
    implicit val ctx = new ParseCtx(module.isVirtualized)
    val tpe = parseType(tpeString)

    it(s"find('${tpeString}', '${name}')") {
      assertResult(STpePath.find(tpe, name))(expected(tpe))
    }
  }

  def testStructPath(module: SModuleDef, tpe: STpeExpr, name: String, expected: Option[STpePath]): Unit = {
    it(s"find(${tpe}, '${name}')") {
      assertResult(STpePath.find(tpe, name))(expected)
    }
  }

  describe("find TpePath") {
    val module = parseModule(reactiveModule)
    implicit val ctx = new ParseCtx(module.isVirtualized)
    testPath(module, "Int", "A", _ => None)
    testPath(module, "A", "A", _ => Some(SNilPath))
    testPath(module, "A => Int", "A", t => Some(SDomPath(t, SNilPath)))
    testPath(module, "Int => A", "A", t => Some(SRangePath(t, SNilPath)))
    testPath(module, "B => A", "A", t => Some(SRangePath(t, SNilPath)))

    testPath(module, "(A, Int)", "A", t => Some(STuplePath(t, 0, SNilPath)))
    testPath(module, "(Int, A)", "A", t => Some(STuplePath(t, 1, SNilPath)))
    testPath(module, "(B, A)", "A", t => Some(STuplePath(t, 1, SNilPath)))

    testPath(module, "Thunk[A]", "A", t => Some(SThunkPath(t, SNilPath)))
    testPath(module, "Thunk[B]", "A", _ => None)

    val t1 = STpeStruct(List(("a", parseType("A"))))
    testStructPath(module, t1, "A", Some(SStructPath(t1, "a", SNilPath)))
    val t2 = parseType("(A,Int)")
    val t3 = STpeStruct(List(("a", parseType("Int")), ("b", t2)))
    testStructPath(module, t3, "A",
      Some(SStructPath(t3, "b", STuplePath(t2, 0, SNilPath))))

    val entity = module.getEntity("Observable")
    testPath(module, "Observable[Int]", "A", _ => None)
    testPath(module, "Observable[A]", "A", t => Some(SEntityPath(t, entity, STpeArg("A"), SNilPath)))

    {
      val t1 = parseType("A => Int")
      testPath(module, "Observable[A => Int]", "A", t => Some(SEntityPath(t, entity, STpeArg("A"), SDomPath(t1, SNilPath))))
    }
    {
      val t1 = parseType("Int => A")
      testPath(module, "Observable[Int => A]", "A", t => Some(SEntityPath(t, entity, STpeArg("A"), SRangePath(t1, SNilPath))))
    }
    {
      val t1 = parseType("Observable[A]")
      testPath(module, "Observable[Observable[A]]", "A",
        t => Some(SEntityPath(t, entity, STpeArg("A"), SEntityPath(t1, entity, STpeArg("A"), SNilPath))))
    }
  }

  def makePath(module: SModuleDef, tpeString: String, name: String): STpePath = {
    implicit val ctx = new ParseCtx(module.isVirtualized)
    val tpe = parseType(tpeString)
    STpePath.find(tpe, name).get
  }
  def makePath(module: SModuleDef, tpe: STpeExpr, name: String): STpePath = {
    STpePath.find(tpe, name).get
  }

  import ScalanCodegen._

  def testEmit(module: SModuleDef, inType: String, name: String, path: STpePath, expected: String): Unit = {
    val prefix = "x.elem"
    val code = emitImplicitElemDeclByTpePath(prefix, path)
    it(s"emit($inType, $name)") {
      assertResult(code)(prefix + expected)
    }
  }

  def testEmit(module: SModuleDef, inType: String, name: String, expected: String): Unit = {
    val path = makePath(module, inType, name)
    testEmit(module, inType, name, path, expected)
  }

  def testEmit(module: SModuleDef, inType: STpeExpr, name: String, expected: String): Unit = {
    val path = makePath(module, inType, name)
    testEmit(module, inType.toString, name, path, expected)
  }

  describe("emitImplicitElemDeclByTpePath") {
    val module = parseModule(reactiveModule)
    implicit val ctx = new ParseCtx(module.isVirtualized)
    testEmit(module, "A", "A", "")
    testEmit(module, "A => Int", "A", ".eDom")
    testEmit(module, "Int => A", "A", ".eRange")
    testEmit(module, "(A, Int)", "A", ".eFst")
    testEmit(module, "(Int, A)", "A", ".eSnd")
    testEmit(module, "(Int, A) => Int", "A", ".eDom.eSnd")
    testEmit(module, "Int => (Int, A)", "A", ".eRange.eSnd")

    val tStruct = STpeStruct(List(("a", parseType("Int")), ("b", parseType("(A,Int)"))))
    testEmit(module, tStruct, "A", """.asInstanceOf[StructElem[_]]("b").asInstanceOf[PairElem[_,_]].eFst""")

    testEmit(module, "Observable[A]", "A", """.typeArgs("A")._1.asElem[A]""")
    testEmit(module, "Observable[Observable[A]]", "A",
      """.typeArgs("A")._1.asElem[Observable[A]].typeArgs("A")._1.asElem[A]""")
  }
}
