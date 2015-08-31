package scalan.linalgebra

/**
 * Created by Victor Smirnov on 4/15/15.
 */

import java.io.File
import java.lang.reflect.Method

import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.{ScalanCommunityDslSeq, BaseShouldTests, ScalanCtxExp, ScalanCtxSeq}

class LASuite extends BaseShouldTests {

  lazy val vector1 = Array((0, 1.0), (1, 2.0), (2, 3.0), (3, 4.0), (4, 5.0))
  lazy val vector2 = Array((0, 1.0), (2, 3.0), (3, 4.0))
  lazy val len = 5

  "in seq context1" should "execute functions" in {
    val ctx = new ScalanCtxSeq with ScalanCommunityDslSeq with LinearAlgebraExamples {}
    val i = 3
    val in = (vector1, (len, i))
    val res = ctx.applySparseVector(in)
    println("res: " + res)
    res should be(4)
  }

  "ConstVector" should "return same results as DenseVector" in {
    val ctx = new ScalanCtxSeq with ScalanCommunityDslSeq with LinearAlgebraExamples {}
    val cv = ctx.constVector((2.0, 3))
    val dv = ctx.denseVector(Array(2.0, 2.0, 2.0))

    cv.length should be(dv.length)
    cv.items should be(dv.items)
    cv.nonZeroIndices should be(dv.nonZeroIndices)
    cv.nonZeroValues should be(dv.nonZeroValues)
    cv.nonZeroItems.arr should be(dv.nonZeroItems.arr)
    cv.apply(1) should be(dv.apply(1))
    cv.filterBy( x => x>0 ).items should be(dv.filterBy( x => x>0 ).items)
    cv.+^(2.0).items should be(dv.+^(2.0).items)
    cv.+^(dv) should be(dv.+^(cv))
    cv.-^(2.0).items should be(dv.-^(2.0).items)
    cv.-^(dv) should be(dv.-^(cv))
    cv.*^(2.0).items should be(dv.*^(2.0).items)
    cv.*^(dv) should be(dv.*^(cv))
    cv.pow_^(2).items should be(dv.pow_^(2).items)
    cv.dot(dv) should be(dv.dot(dv))
    cv.euclideanNorm should be(dv.euclideanNorm)
  }

  "ConstMatrix" should "return same results as DenseFlatMatrix" in {
    val ctx = new ScalanCtxSeq with ScalanCommunityDslSeq with LinearAlgebraExamples {}
    val cm = ctx.constMatrix((2.0, (3, 2)))
    val fm = ctx.flatMatrix((Array(2.0, 2.0, 2.0, 2.0, 2.0, 2.0), 3))
    val dv = ctx.denseVector(Array(2.0, 2.0, 2.0))

    cm.numColumns should be(fm.numColumns)
    cm.numRows should be(fm.numRows)
    cm.rmValues should be(fm.rmValues)
    cm.apply(1).items should be(fm.apply(1).items)
    cm.countNonZeroesByColumns.items should be(fm.countNonZeroesByColumns.items)
    cm.*(dv).items should be(fm.*(dv).items)
    cm.*(fm.transpose).rmValues should be(fm.*(cm.transpose).rmValues)
    cm.+^^(fm).rmValues should be(fm.+^^(cm).rmValues)
    cm.*^^(fm).rmValues should be(fm.*^^(cm).rmValues)
    cm.*^^(2.0).rmValues should be(fm.*^^(2.0).rmValues)

  }

}
