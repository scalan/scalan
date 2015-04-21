package scalan.linalgebra

/**
 * Created by Victor Smirnov on 4/15/15.
 */

import java.io.File
import java.lang.reflect.Method

import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.{ScalanCommunityDslSeq, BaseShouldTests, ScalanCtxExp, ScalanCtxSeq}

class LASuite extends BaseShouldTests {

  lazy val vector1 = Array(Pair(0, 1.0), Pair(1, 2.0), Pair(2, 3.0), Pair(3, 4.0), Pair(4, 5.0))
  lazy val vector2 = Array(Pair(0, 1.0), Pair(2, 3.0), Pair(3, 4.0))
  lazy val len = 5

  "in seq context1" should "execute functions" in {
    val ctx = new ScalanCtxSeq with ScalanCommunityDslSeq with LinearAlgebraExamples {}
    val i = 3
    val in = (vector1, (len, i))
    val res = ctx.applySparseVector(in)
    println("res: " + res)
    res should be(4)
  }
}
