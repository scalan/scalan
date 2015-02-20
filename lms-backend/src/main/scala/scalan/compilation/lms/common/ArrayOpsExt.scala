package scalan.compilation.lms.common

import java.util.HashMap

import scala.virtualization.lms.common._
import scala.virtualization.lms.epfl.test7.ArrayLoopsFatExp

trait ArrayOpsExtExp extends BaseExp with LiftVariables with LiftPrimitives with LiftNumeric with ListOpsExp
with NumericOpsExp with PrimitiveOpsExp with FunctionsExp with HashMapOpsExp with EqualExp with BooleanOpsExp
with TupleOpsExp with ArrayLoopsFatExp with OrderingOpsExp with IfThenElseFatExp with ArrayOpsExp with WhileExp
with ArrayBuilderOpsExp with CastingOpsExp with EitherOpsExp with MathOpsExp with ExceptionOpsExp {

  def array_new[A: Manifest](len: Rep[Int]): Rep[Array[A]] = ArrayNew[A](len)

  def mapFromArray[K: Manifest, V: Manifest](arr: Exp[Array[(K, V)]]): Exp[HashMap[K, V]] = {
    val h = HashMap[K, V]()
    for (pair <- arr) {
      h.update(pair._1, pair._2)
    }
    h
  }

  def arrayGet[A: Manifest](a: Exp[Array[A]], i: Exp[Int]): Exp[A] = {
    a.at(i)
  }

  def arrayGather[A: Manifest](a: Exp[Array[A]], idxs: Exp[Array[Int]]): Exp[Array[A]] = {
    array(idxs.length)(i => a.at(idxs.at(i)))
  }

  def arrayLength[A: Manifest](a: Exp[Array[A]]): Exp[Int] = {
    a.length
  }

  def mapArray[A: Manifest, B: Manifest](a: Exp[Array[A]], f: Rep[A] => Rep[B]): Exp[Array[B]] = {
    //    a.map(f)
    array(a.length)(i => f(a.at(i)))
  }

  def flatMapArray[A: Manifest, B: Manifest](arr: Exp[Array[A]], f: Rep[A] => Rep[Array[B]]): Exp[Array[B]] = {
    val buf = ArrayBuilder.make[B]
    for (x <- arr; y <- f(x)) {
      buf += y
    }
    buf.result
  }

  def findArray[A: Manifest](a: Exp[Array[A]], f: Rep[A] => Rep[Boolean]): Exp[Array[Int]] = {
    arrayIf(a.length) { i => (f(a.at(i)), i)}
  }

  def filterArray[A: Manifest](a: Exp[Array[A]], f: Rep[A] => Rep[Boolean]): Exp[Array[A]] = {
    arrayIf(a.length) { i => (f(a.at(i)), a.at(i))}
  }

  def countArray[A: Manifest](a: Exp[Array[A]], f: Rep[A] => Rep[Boolean]): Exp[Int] = {
    var count = 0
    for (x <- a) {
      if (f(x)) count += 1
    }
    count
  }

  def replicate[A: Manifest](length: Exp[Int], v: Exp[A]): Exp[Array[A]] = {
    array(length)(i => v)
  }

  def indexRangeD(length: Exp[Int]): Exp[Array[Int]] = {
    array(length)(i => i)
    array(length)(i => i)
  }

  def newArray[A: Manifest](length: Rep[Int]): Rep[Array[A]] = NewArray[A](length)

  def opZipWith[A: Manifest, B: Manifest, R: Manifest](f: (Rep[A], Rep[B]) => Rep[R], a: Exp[Array[A]], b: Exp[Array[B]]): Exp[Array[R]] = {
    array(a.length)(i => f(a.at(i), b.at(i)))
  }

  def opZip[A: Manifest, B: Manifest](a: Exp[Array[A]], b: Exp[Array[B]]): Exp[Array[(A, B)]] = {
    array[(A, B)](a.length)(i => (a.at(i), b.at(i)))
  }

  def arraySort[A: Manifest](a: Exp[Array[A]]): Exp[Array[A]] = {
    a.sort
  }

  def strideArray[A: Manifest](xs: Exp[Array[A]], start: Exp[Int], length: Exp[Int], stride: Exp[Int]) =
    array(length) { i =>
      xs.at(start + i * stride)
    }

  def updateArray[A: Manifest](xs: Exp[Array[A]], index: Exp[Int], value: Exp[A]) = {
    xs.update(index, value)
    xs
  }

  def arraySum[A: Manifest](xs: Exp[Array[A]])(implicit n: Numeric[A]): Exp[A] = {
    var sum = n.zero
    for (x <- xs) sum += x
    sum
  }

  def arrayMax[A: Manifest](xs: Exp[Array[A]])(implicit o: Ordering[A]): Exp[A] = {
    var max = xs.at(0) // we need Optional type to correctly implement min/max, but it is abselnt in CE
    for (x <- xs) if (x > max) max = x
    max
  }

  def arrayMin[A: Manifest](xs: Exp[Array[A]])(implicit o: Ordering[A]): Exp[A] = {
    var min = xs.at(0) // we need Optional type to correctly implement min/max, but it is abselnt in CE
    for (x <- xs) if (x < min) min = x
    min
  }

  def arrayAvg[A: Manifest](xs: Exp[Array[A]])(implicit n: Numeric[A]): Exp[Double] = {
    var sum = n.zero
    for (x <- xs) sum += x
    sum.AsInstanceOf[Double] / xs.length
  }


  def sum[A: Manifest](a: Exp[Array[A]]): Exp[A] = {
    sum(a.length) { i => a.at(i).AsInstanceOf[Double]}.AsInstanceOf[A]
  }

  def reduce[A: Manifest](a: Exp[Array[A]], zero: Exp[A], accumulate: Rep[(A, A)] => Rep[A]): Exp[A] = {
    var state = zero
    for (x <- a) {
      state = accumulate((state.AsInstanceOf[A], x))
    }
    state
  }

  def fold[A: Manifest, S: Manifest](a: Exp[Array[A]], init: Exp[S], func: Rep[(S, A)] => Rep[S]): Exp[S] = {
    var state = init
    for (x <- a) {
      state = func((state.AsInstanceOf[S], x))
    }
    state
  }

  def sumBy[A: Manifest, S: Manifest](a: Exp[Array[A]], func: Rep[A] => Rep[S])(implicit n: Numeric[S]): Exp[S] = {
    var sum = n.zero
    for (x <- a) {
      sum += func(x)
    }
    sum
  }

}

