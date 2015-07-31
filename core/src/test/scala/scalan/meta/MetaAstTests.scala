package scalan.meta

import scala.language.reflectiveCalls
import scalan.common._
import scalan._

class MetaAstTests extends BaseCtxTests {

  test("EntityElem.entityDef") {
    val ctx = new TestContext("EntityElem.entityDef")
                   with ViewExamples with CommonExamples with SegmentsDslExp with KindsDslExp
    import ctx._
    {
      val e = element[Segment].asEntityElem[Segment]
      val d = e.entityDef
      assert(d.name == "Segment")
    }
    {
      val e = element[Kind[Array,Int]].asEntityElem[Kind[Array,Int]]
      val d = e.entityDef
      assert(d.name == "Kind")
    }
  }

}
