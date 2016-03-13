package scalan.linalgebra

import java.lang.reflect.Method

import scalan._

class Issue186Test extends BaseCtxTests {

  test("issue186") {
    class Ctx extends TestContext with LADslExp { override def isInvokeEnabled(d: Def[_], m: Method) = false }
    val ctx = new Ctx
    import ctx._
    def sparseData2denseData = fun { data: Rep[SparseVectorData[Double]] =>
      val v = SparseVector(data)
      DenseVector(v.items).toData
    }
    ctx.emit("sparseData2denseData", sparseData2denseData)
  }
}
