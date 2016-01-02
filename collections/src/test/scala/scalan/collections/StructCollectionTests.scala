package scalan.collections

import scala.language.reflectiveCalls
import scalan.compilation.DummyCompiler
import scalan.primitives.StructsCompiler
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
    }

    override val compiler = new DummyCompiler(new ScalanCake)
      with StructsCompiler[ScalanCake]
  }

  test("getItems") {
    val ctx = new Ctx
    import ctx.compiler.scalan._
    ctx.test("t1", t1)
    ctx.test("t2", t2)
  }

}
