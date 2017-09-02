package scalan.meta

import scala.language.reflectiveCalls
import scalan.common._
import scalan._

class MetaAstTests extends BaseCtxTests {

  test("EntityElem.entityDef") {
    val ctx = new TestContext with ViewExamples with CommonExamples with SegmentsDsl with KindsDsl
    import ctx._
    {
      val e = element[Segment].asEntityElem
      val d = entityDef(e)
      assert(d.name == "Segment")
    }
//    {
//      val e = element[Kind[Array,Int]].asEntityElem
//      val d = entityDef(e)
//      assert(d.name == "Kind")
//    }
  }

}
