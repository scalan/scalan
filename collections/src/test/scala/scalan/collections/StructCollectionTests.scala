package scalan.collections

import scala.language.reflectiveCalls
import scalan.compilation.{StructsCompiler, DummyCompiler}
import scalan.{ScalanDslExp, BaseViewTests}
import scalan.common.{Lazy, SegmentsDslExp}

class StructCollectionTests extends BaseViewTests {
  class Ctx extends TestCompilerContext {
    class ScalanCake extends ScalanDslExp with CollectionsDslExp with SegmentsDslExp {
      val eKeySchema = structElement(Seq(
        "a" -> element[Int]))
      val eRowSchema = structElement(Seq(
        "b" -> element[String],
        "c" -> element[Double]))

      val eSI = structItemElement(AnyElement, eRowSchema)

      lazy val t1 = fun { (in: Rep[Struct]) =>
        val items = StructItemCollection(in)(AnyElement, eRowSchema)
        items(0)
      }(Lazy(eRowSchema), eSI)

      lazy val t2 = fun { (in: Rep[Struct]) =>
        val items = StructItemCollection(in)(AnyElement, eRowSchema)
        val zs = items.zip(items)(eSI)
        val Pair(ia, ib) = zs(0)
        Pair(ia.value, ib.value)
      }(Lazy(eRowSchema), pairElement(AnyElement, AnyElement))

      lazy val t3 = fun { (in: Rep[Struct]) =>
        val items = StructItemCollection(in)(AnyElement, eRowSchema)
        val zs = items.map(item => {
          patternMatch(item)(
            MkBranch[StructItem[String, Struct]]()(eRowSchema.getItemElem("b")).make(i => i.value.length.toDouble),
            MkBranch[StructItem[Double, Struct]]()(eRowSchema.getItemElem("c")).make(i => i.value)
          )(None)
        })
        zs
      }(Lazy(eRowSchema), element[Collection[Double]])
    }

    override val compiler = new DummyCompiler(new ScalanCake)
      with StructsCompiler[ScalanCake]
  }

  test("getItems") {
    val ctx = new Ctx
    import ctx.compiler.scalan._
    ctx.test("t1", t1)
  }

  test("zip") {
    val ctx = new Ctx
    import ctx.compiler.scalan._
    ctx.test("t2", t2)
  }

  test("map") {
    val ctx = new Ctx
    import ctx.compiler.scalan._
    ctx.test("t3", t3)
  }

}
