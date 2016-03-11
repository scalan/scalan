package scalan.linalgebra

/**
 * Created by Victor Smirnov on 4/15/15.
 */

import java.io.File
import java.lang.reflect.Method

import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.{BaseShouldTests, ScalanDslExp, ScalanDslStd}

class LASuite extends BaseShouldTests {

  lazy val vector1 = Array((0, 1.0), (1, 2.0), (2, 3.0), (3, 4.0), (4, 5.0))
  lazy val vector2 = Array((0, 1.0), (2, 3.0), (3, 4.0))

  class Seq extends LADslStd with LinearAlgebraExamples {}
  val seq = new Seq
  import seq._
  lazy val v = toRep(3.0)
  lazy val len = toRep(3)
  lazy val res = f1(cv, cv)
  def cv = ConstVector(v, len)
  def f1(v1: Vec[Double], v2: Vec[Double]): DoubleRep = ((v1 +^ v2) -^ (v1 *^ v2) +^ (v1 /^ v2)) dot (v2 /^ v1)
  def vvm_converters(v1: Vec[Double], v2: Vec[Double], res: DoubleRep, f: (Vec[Double], Vec[Double]) => DoubleRep) = {
    f(v1.convertTo[DenseVector[Double]], v2.convertTo[DenseVector[Double]]) should be(res)
    f(v1.convertTo[DenseVector[Double]], v2.convertTo[ConstVector[Double]]) should be(res)
    f(v1.convertTo[DenseVector[Double]], v2.convertTo[SparseVector[Double]]) should be(res)
    f(v1.convertTo[DenseVector[Double]], v2.convertTo[SparseVectorBoxed[Double]]) should be(res)
    f(v1.convertTo[DenseVector[Double]], v2.convertTo[ShiftVector[Double]]) should be(res)
    f(v1.convertTo[DenseVector[Double]], v2.convertTo[ShiftVectorBoxed[Double]]) should be(res)

    f(v1.convertTo[ConstVector[Double]], v2.convertTo[DenseVector[Double]]) should be(res)
    f(v1.convertTo[ConstVector[Double]], v2.convertTo[ConstVector[Double]]) should be(res)
    f(v1.convertTo[ConstVector[Double]], v2.convertTo[SparseVector[Double]]) should be(res)
    f(v1.convertTo[ConstVector[Double]], v2.convertTo[SparseVectorBoxed[Double]]) should be(res)
    f(v1.convertTo[ConstVector[Double]], v2.convertTo[ShiftVector[Double]]) should be(res)
    f(v1.convertTo[ConstVector[Double]], v2.convertTo[ShiftVectorBoxed[Double]]) should be(res)

    f(v1.convertTo[SparseVector[Double]], v2.convertTo[DenseVector[Double]]) should be(res)
    f(v1.convertTo[SparseVector[Double]], v2.convertTo[ConstVector[Double]]) should be(res)
    f(v1.convertTo[SparseVector[Double]], v2.convertTo[SparseVector[Double]]) should be(res)
    f(v1.convertTo[SparseVector[Double]], v2.convertTo[SparseVectorBoxed[Double]]) should be(res)
    f(v1.convertTo[SparseVector[Double]], v2.convertTo[ShiftVector[Double]]) should be(res)
    f(v1.convertTo[SparseVector[Double]], v2.convertTo[ShiftVectorBoxed[Double]]) should be(res)

    f(v1.convertTo[SparseVectorBoxed[Double]], v2.convertTo[DenseVector[Double]]) should be(res)
    f(v1.convertTo[SparseVectorBoxed[Double]], v2.convertTo[ConstVector[Double]]) should be(res)
    f(v1.convertTo[SparseVectorBoxed[Double]], v2.convertTo[SparseVector[Double]]) should be(res)
    f(v1.convertTo[SparseVectorBoxed[Double]], v2.convertTo[SparseVectorBoxed[Double]]) should be(res)
    f(v1.convertTo[SparseVectorBoxed[Double]], v2.convertTo[ShiftVector[Double]]) should be(res)
    f(v1.convertTo[SparseVectorBoxed[Double]], v2.convertTo[ShiftVectorBoxed[Double]]) should be(res)

    f(v1.convertTo[ShiftVector[Double]], v2.convertTo[DenseVector[Double]]) should be(res)
    f(v1.convertTo[ShiftVector[Double]], v2.convertTo[ConstVector[Double]]) should be(res)
    f(v1.convertTo[ShiftVector[Double]], v2.convertTo[SparseVector[Double]]) should be(res)
    f(v1.convertTo[ShiftVector[Double]], v2.convertTo[SparseVectorBoxed[Double]]) should be(res)
    f(v1.convertTo[ShiftVector[Double]], v2.convertTo[ShiftVector[Double]]) should be(res)
    f(v1.convertTo[ShiftVector[Double]], v2.convertTo[ShiftVectorBoxed[Double]]) should be(res)

    f(v1.convertTo[ShiftVectorBoxed[Double]], v2.convertTo[DenseVector[Double]]) should be(res)
    f(v1.convertTo[ShiftVectorBoxed[Double]], v2.convertTo[ConstVector[Double]]) should be(res)
    f(v1.convertTo[ShiftVectorBoxed[Double]], v2.convertTo[SparseVector[Double]]) should be(res)
    f(v1.convertTo[ShiftVectorBoxed[Double]], v2.convertTo[SparseVectorBoxed[Double]]) should be(res)
    f(v1.convertTo[ShiftVectorBoxed[Double]], v2.convertTo[ShiftVector[Double]]) should be(res)
    f(v1.convertTo[ShiftVectorBoxed[Double]], v2.convertTo[ShiftVectorBoxed[Double]]) should be(res)
  }
  
  "in seq context1" should "execute functions" in {
    val ctx = new Seq
    val i = 3
    val in = (vector1, (len, i))
    val res = ctx.applySparseVector(in)
    res should be(4)
  }

  "ConstVector" should "return same results as DenseVector" in {
    val ctx = new Seq
    val cv = ctx.constVector((2.0, 3))
    val dv = ctx.denseVector(Array(2.0, 2.0, 2.0))
    val sv = ctx.sparseVector((Array(0, 1, 2), (Array(2.0, 2.0, 2.0), 3)))
    val sv1 = ctx.sparseVectorBoxed((Array((0, 2.0), (1, 2.0), (2, 2.0)), 3))

    cv.length should be(dv.length)
    cv.items should be(dv.items)
    cv.nonZeroIndices should be(dv.nonZeroIndices)
    cv.nonZeroValues should be(dv.nonZeroValues)
    cv.nonZeroItems.arr should be(dv.nonZeroItems.arr)
    cv.apply(1) should be(dv.apply(1))
    cv.filterBy( x => x>0 ).items should be(dv.filterBy( x => x>0 ).items)
    cv.+^(2.0).items should be(dv.+^(2.0).items)
    cv.+^(dv) should be(dv.+^(cv))
    cv.+^(sv) should be(sv.+^(cv))
    cv.+^(sv1) should be(sv1.+^(cv))
    cv.-^(2.0).items should be(dv.-^(2.0).items)
    cv.-^(dv) should be(dv.-^(cv))
    cv.*^(2.0).items should be(dv.*^(2.0).items)
    cv.*^(dv) should be(dv.*^(cv))
    cv.pow_^(2).items should be(dv.pow_^(2).items)
    cv.dot(dv) should be(dv.dot(dv))
    cv.dot(sv) should be(sv.dot(sv))
    cv.dot(sv1) should be(sv1.dot(sv1))
    cv.euclideanNorm should be(dv.euclideanNorm)
  }

  "ConstMatrix" should "return same results as DenseFlatMatrix" in {
    val ctx = new Seq
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

  "DiagonalMatrix" should "return same results as DenseFlatMatrix" ignore {
    val ctx = new Seq
    val dm = ctx.diagonalMatrix(Array(1.0, 2.0, 3.0, 4.0))
    val fm = ctx.flatMatrix((Array(1.0, 0.0, 0.0, 0.0, 0.0, 2.0, 0.0, 0.0, 0.0, 0.0, 3.0, 0.0, 0.0, 0.0, 0.0, 4.0), 4))
    val dv = ctx.denseVector(Array(2.0, 3.0, 4.0, 5.0))

    dm.numColumns should be(fm.numColumns)
    dm.numRows should be(fm.numRows)
    dm.rmValues should be(fm.rmValues)
    dm.apply(1).items should be(fm.apply(1).items)
    dm.apply(2).items should be(fm.apply(2).items)
    dm.rows(2).items should be(fm.rows(2).items)
    dm.apply(1, 1) should be(fm.apply(1, 1))
    dm.apply(1, 2) should be(fm.apply(1, 2))
    dm.countNonZeroesByColumns.items should be(fm.countNonZeroesByColumns.items)
    dm.*(dv).items should be(fm.*(dv).items)
    dm.*(fm.transpose).rmValues should be(fm.*(dm.transpose).rmValues)
    dm.+^^(fm).rmValues should be(fm.+^^(dm).rmValues)
    dm.*^^(fm).rmValues should be(fm.*^^(dm).rmValues)
    dm.*^^(2.0).rmValues should be(fm.*^^(2.0).rmValues)

  }

  "vvm converters" should "be equivalent for all Vector types" ignore vvm_converters(cv, cv, res, f1)
}
