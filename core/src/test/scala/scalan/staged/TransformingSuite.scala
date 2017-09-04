package scalan.staged

import java.io.File
import java.lang.reflect.Method

import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.BaseShouldTests

import scalan.ScalanDsl

class TransformingSuite extends BaseShouldTests {

  def getCtx = new ScalanDsl with GraphVizExport {
    override def isInvokeEnabled(d: Def[_], m: Method) = true

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
    emitDepGraph(test, prefix, "testFunc")(GraphVizConfig.default)
    val lam = test.getLambda
    val t = lam.projectionTreeFrom(lam.x)
    println(t)
  }


}
