package scalan.meta

import com.fasterxml.jackson.module.scala.DefaultScalaModule
import scalan.meta.serialization.JacksonSerializer

class ScalanAstSerializerTests extends ScalanAstTests with Examples {
  import ScalanAst._
  import compiler._

  val serde = new JacksonSerializer()

  def test[T: Manifest](t: T): Unit = {
    val str = serde.serialize(t)
    println(str)
    val v = serde.deserialize(str)
    v should be(t)
  }

  def testExpr(source: String): Unit = {
    test(parseExpr(source))
  }

  def testDef(source: String): Unit = {
    test(parseBodyItem(source))
  }

  val TpeObservable = parseType("Observable[Int]")
  val fooDef        = parseBodyItem("def foo()").asInstanceOf[SMethodDef]
  it("serialize STpeExpr") {
    test(STpeAnnotated(TpeInt, "NonNull"))
    test(STpeSingleton(SEmpty(Some(TpeUnit))))
    val tpeStruct = STpeStruct(List("a" -> TpeInt, "b" -> TpeString))
    test(tpeStruct)
    test(STpeSelectFromTT(tpeStruct, "a"))
    test(STpeEmpty())
    test(STpeConst(SConst(1, Some(TpeInt))))
    test(STpeCompound(List(TpeObservable), List(fooDef)))
    STpePrimitives.foreach { case (n, t) => test(t) }
    test(STpeMethod(List("A"), List(TpeInt), TpeString))
    test(STpeBind("a", TpeInt))
    test(STpeThis("a"))
    test(parseType("(Int, Int)"))
    test(parseType("Observable[T] forSome {type T}"))
    test(STpeSingle(TpeInt, "Name"))
    test(parseType("(Int => Int)"))
    test(TpeObservable)
    test(STpeTypeBounds(TpeNothing, TpeAny))
  }
  it("serialize SExpr") {
    testExpr("t match { case Pair(x, y) => x + y }") // SMatch, SCase
    test(SSuper("M1", "Scalan", "field1", None))
    testExpr("(x, y) => x + y") // SFunc
    testExpr("(1, 2, 3)") // STuple
    testExpr("f(1)(1, 2, 3)") // SApply
    testExpr("x: Int") // SAscr
    testExpr("{ val x = 1; x + 1 }") // SBlock
    testExpr("y.x") // SSelect
    testExpr("y = x") // SAssign
    testExpr("if (c) x else y") // SIf
    testExpr("x: @unchecked") // SAnnotated
    testExpr("") // SConst(null)
    testExpr("10") // SConst(10)
    testExpr(""" "abc" """) // SConst("abc")
  }
  it("serialize SBodyItem") {
    testDef("val y: Int = x") // SValDef
    testDef("type Index = Int") // STpeDef
    testDef("import scalan._") // SImportStat
    testDef("def foo(x: Int): String") // SMethodDef
    testDef("object Point {}") // SObjectDef
    testDef("class Point(x: Int, y: Int) {}") // SClassDef
    testDef("trait PointOps { def x: Int; def y: Int }") // STraitDef
  }

  it("serialize SModuleDef") {
    test(parseModule(reactiveModule))
  }
}
