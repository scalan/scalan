package tests.scalan.staged

import tests.{GraphVizExport, BaseShouldTests}
import scalan.ScalanCtxStaged

class TransformingSuite extends BaseShouldTests {

  val prefix = "test-out/scalan/staged/Transforming/"

  def getCtx = new ScalanCtxStaged with GraphVizExport {
    this.invokeEnabled = true
    lazy val test = fun { xyz: Rep[(Int, (Int, Int))] =>
      val x = xyz._1
      val y = xyz._2
      val z = xyz._3
      x * y + x * z === x * (y + z)
    }
  }

  "Transforming" should "created ProjectionTree" in {
    val ctx = getCtx
    import ctx._
    emitDepGraph(test, s"${prefix}testFunc.dot", false)
    val lam = test.getLambda
    val t = lam.projectionTreeFrom(lam.x)
    println(t)
  }


}
