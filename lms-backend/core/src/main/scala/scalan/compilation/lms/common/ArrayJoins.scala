package scalan.compilation.lms.common

import scala.lms.common._
import scala.lms.internal.Transforming
import scala.reflect.SourceContext
import scalan.compilation.lms.LmsBackendFacade
import scalan.compilation.lms.cxx.sharedptr.CxxShptrCodegen

trait ArrayJoins extends Base with ArrayBuilderOps {

  def arrayInnerJoin[K, B, C, R](xs: Rep[Array[(K, B)]], ys: Rep[Array[(K, C)]], f: Rep[((B, C)) => R])
                                    (implicit ordK: Ordering[K], nK: Numeric[K], mK: Manifest[K],
                                     mB: Manifest[B], mC: Manifest[C], mR: Manifest[R]): Rep[Array[(K, R)]]

  def arrayOuterJoin[K, B, C, R](xs: Rep[Array[(K, B)]], ys: Rep[Array[(K, C)]], f: Rep[((B, C)) => R],
                                     f1: Rep[B => R], f2: Rep[C => R])
                                    (implicit ordK: Ordering[K], nK: Numeric[K], mK: Manifest[K],
                                     mB: Manifest[B], mC: Manifest[C], mR: Manifest[R]): Rep[Array[(K, R)]]
}

trait ArrayJoinsExp extends ArrayJoins with EffectExp with VariablesExp with Transforming { self: LmsBackendFacade =>

  def arrayInnerJoin[K, B, C, R](xs: Rep[Array[(K, B)]], ys: Rep[Array[(K, C)]], f: Rep[((B, C)) => R])
                                (implicit ordK: Ordering[K], nK: Numeric[K], mK: Manifest[K],
                                 mB: Manifest[B], mC: Manifest[C], mR: Manifest[R]) = {
    val res = ArrayBuilder.make[(K, R)]
    var i1 = 0
    var i2 = 0
    val n1 = xs.length
    val n2 = ys.length
    while (readVar(i1) < n1 && readVar(i2) < n2) {
      val x = xs.at(readVar(i1))
      val y = ys.at(readVar(i2))
      if (x._1 == y._1) {
        res += (x._1, f((x._2, y._2)))
        i1 = readVar(i1) + 1
        i2 = readVar(i2) + 1
      } else if (x._1 < y._1) i1 = readVar(i1) + 1 else i2 = readVar(i2) + 1
    }
    res.result
  }

  def arrayOuterJoin[K, B, C, R](xs: Rep[Array[(K, B)]], ys: Rep[Array[(K, C)]], f: Rep[((B, C)) => R],
                                 f1: Rep[B => R], f2: Rep[C => R])
                                (implicit ordK: Ordering[K], nK: Numeric[K], mK: Manifest[K],
                                 mB: Manifest[B], mC: Manifest[C], mR: Manifest[R]) = {
    val res = ArrayBuilder.make[(K, R)]
    var i1 = 0
    var i2 = 0
    val n1 = xs.length
    val n2 = ys.length
    while (readVar(i1) < n1 && readVar(i2) < n2) {
      val i = readVar(i1)
      val j = readVar(i2)
      if (i == n1) {
        val y = ys.at(j)
        res += (y._1, f2(y._2))
        i2 = j + 1
      } else if (j == n2) {
        val x = xs.at(i)
        res += (x._1, f1(x._2))
        i1 = i + 1
      } else {
        val x = xs.at(i)
        val y = ys.at(j)
        if (x._1 == y._1) {
          res += (x._1, f((x._2, y._2)))
          i1 = i + 1
          i2 = j + 1
        } else if (x._1 < y._1) i1 = i + 1 else i2 = j + 1
      }
    }
    res.result
  }
}

trait ScalaGenArrayJoins extends ScalaGenBase

trait CxxShptrGenArrayJoins extends CxxShptrCodegen
