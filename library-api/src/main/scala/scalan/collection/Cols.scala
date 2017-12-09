package scalan.collection

import scala.reflect.ClassTag

trait Col[A] {
  def arr: Array[A]
  def length: Int
  def apply(i: Int): A
//  def map[B: ClassTag](f: A => B): Col[B]
  //    def reduce(implicit m: NumMonoid[A]): A = arr.reduce(m.append)
  //    def zip[B](ys: Col[B]): PairCol[A, B] = new PairCol(this, ys)
}

trait ColBuilder {
  def fromArray[T](arr: Array[T]): Col[T]
  def ddmvm(v: Array[Double]): Int = {
    val xs = Array.fill(v.length)(0)
    val c = xs.zip(v).map(d => d)
    c.length
  }
}


