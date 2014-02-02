package scalan.primitives

import java.io.{BufferedReader, FileReader, PrintWriter}
import scalan.common.Monoid
import scalan.{ScalanStaged, ScalanSeq, Scalan}
import scala.collection.generic.CanBuildFrom

trait ArrayOps { self: Scalan  =>
  type RA[T] = Rep[Array[T]]
  implicit class RepArrayOps[T](xs: RA[T]) {
    def apply(n: Rep[Int]) = array_apply(xs, n)
    def length = array_length(xs)
    def map[R:Manifest](f: Rep[T=>R]) = array_map(xs, f)
    def sum(implicit m: Monoid[Rep[T]]) = array_sum(xs)
    def zip[U](ys: RA[U]): RA[(T,U)] = array_zip(xs, ys)
  }

  def array_apply[T](xs: RA[T], n: Rep[Int]): Rep[T]
  def array_length[T](xs: RA[T]) : Rep[Int]
  def array_map[T,R:Manifest](xs: RA[T], f: Rep[T=>R]): RA[R]
  def array_sum[T](xs: RA[T])(implicit m: Monoid[Rep[T]]) : Rep[T]
  def array_zip[T,U](xs: RA[T], ys:RA[U]): RA[(T,U)]
}

trait ArrayOpsSeq extends ArrayOps { self: ScalanSeq =>
  def array_apply[T](x: RA[T], n: Rep[Int]): Rep[T] = x(n)
  def array_length[T](a: RA[T]) : Rep[Int] = a.length
  def array_map[T, R:Manifest](xs: Array[T], f: T => R) = Array.tabulate(xs.length)(i => f(xs(i))) //xs.map(f)
  def array_sum[T](xs: RA[T])(implicit m: Monoid[Rep[T]]) = xs.fold(m.zero)((x,y) => m.append(x,y))
  def array_zip[T,U](xs: Array[T], ys:Array[U]): Array[(T,U)] = (xs, ys).zipped.toArray
  //def test[B](xs: Array[Int], f: Int=>B)(implicit cbf:CanBuildFrom[Array[Int],B,Array[B]], mb: Manifest[B]): Array[B] = xs.map(f)
}

trait ArrayOpsExp extends ArrayOps { self: ScalanStaged =>
  import implicitManifests._

  def withElemOfArray[T,R](xs: RA[T])(block: Elem[T] => R): R =
    withElemOf(xs){ eTArr =>
      block(eTArr.ea)
    }

  trait ArrayDef[T] extends Def[Array[T]]
  case class ArrayLength[T](xs: Exp[Array[T]]) extends Def[Int] {
    def objType = element[Int]
  }
  case class ArrayApply[T](xs: Exp[Array[T]], index: Exp[Int]) extends Def[T] {
    def objType = withElemOf(xs){ _.ea }
  }
  case class ArrayMap[T,R](xs: Exp[Array[T]], f: Exp[T=>R]) extends ArrayDef[R] {
    def objType = withResultElem(f){implicit eR => element[Array[R]]}
  }
  case class ArraySum[T](xs: Exp[Array[T]], implicit val m: Monoid[Rep[T]]) extends Def[T] {
    def objType = withElemOf(xs){ _.ea }
  }
  case class ArrayZip[T:Elem,U:Elem](xs: Exp[Array[T]], ys: Exp[Array[U]]) extends ArrayDef[(T,U)] {
    def objType = element[Array[(T,U)]]
  }

  def array_apply[T](xs: Exp[Array[T]], n: Exp[Int]): Rep[T] =
    withElemOfArray(xs){ implicit eT => ArrayApply(xs, n) }
  def array_length[T](a: Exp[Array[T]]) : Rep[Int] = ArrayLength(a)
  def array_map[T, R:Manifest](xs: Exp[Array[T]], f: Exp[T=>R]) =
    withResultElem(f) { implicit eR => ArrayMap(xs, f) }

  def array_sum[T](xs: RA[T])(implicit m: Monoid[Rep[T]]) =
    withElemOfArray(xs){ implicit eT => ArraySum(xs, m) }

  def array_zip[T,U](xs: RA[T], ys:RA[U]): RA[(T,U)] = {
    implicit val eT = withElemOf(xs){ _.ea }
    implicit val eU = withElemOf(ys){ _.ea }
    ArrayZip(xs, ys)
  }
}

