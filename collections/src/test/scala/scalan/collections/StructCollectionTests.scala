package scalan.collections

import scala.language.reflectiveCalls
import scalan.BaseViewTests
import scalan.common.{Lazy, SegmentsDslExp}

class StructCollectionTests extends BaseViewTests {
  class Ctx extends ViewTestsCtx with CollectionsDslExp with SegmentsDslExp {
    val eKeySchema = structElement(Seq(
      "a" -> element[Int]))
    val eRowSchema = structElement(Seq(
      "b" -> element[String],
      "c" -> element[Double]))

    lazy val t1 = fun { (in: Rep[Struct]) =>
      val items = StructItemCollection(in)(AnyElement, eRowSchema)
      items(0)
    }(Lazy(eRowSchema), structItemElement(AnyElement, eRowSchema))
  }

  test("getItems") {
    val ctx = new Ctx
    import ctx._
    emit("t1", t1)
  }

}
