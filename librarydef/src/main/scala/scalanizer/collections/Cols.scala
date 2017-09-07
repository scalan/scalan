package scalanizer.collections

import scalan.meta.ScalanAst.KernelType
import scalan.meta.scalanizer.HotSpot
import scalanizer.linalgebra.LinearAlgebra

trait Cols {self: LinearAlgebra =>
  trait Col[A] {
    def arr: Array[A]
    def length: Int
    def apply(i: Int): A
//    def map[B: ClassTag](f: A => B): Col[B] = Col(arr.map(f))
//    def reduce(implicit m: NumMonoid[A]): A = arr.reduce(m.append)
//    def zip[B](ys: Col[B]): PairCol[A, B] = new PairCol(this, ys)
  }

  object Col {
//    def apply[T](arr: Array[T]): Col[T] = fromArray(arr)
    def fromArray[T](arr: Array[T]): Col[T] = new ColOverArray(arr)
    @HotSpot(KernelType.Scala)
    def ddmvm(v: Array[Double]): Int = {
//      scalanizer.collections.implOfCols.HotSpotKernels.ddmvmKernel(v)
      val c = Col.fromArray(v)
      c.length
    }
  }

  class ColOverArray[A](val arr: Array[A]) extends Col[A] {
    def length = arr.length
    def apply(i: Int) = arr(i)
  }
  object ColOverArray {
    def fromArray[T](arr: Array[T]): Col[T] = new ColOverArray(arr)
  }
//  class PairCol[A, B](val as: Col[A], val bs: Col[B]) extends Col[(A, B)] {
//    def arr: Array[(A, B)] = (as.arr zip bs.arr)
//    def length = as.length
//    def apply(i: Int) = (as(i), bs(i))
//  }
}

