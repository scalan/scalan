package scalan.primitives

import java.io.{BufferedReader, FileReader, PrintWriter}
import scalan.{ScalanStaged, ScalanSeq, Scalan}
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

trait ArrayOps { self: Scalan  =>
  type Arr[T] = Rep[Array[T]]
  implicit class RepArrayOps[T](xs: Arr[T]) {
    def apply(n: Rep[Int]) = array_apply(xs, n)
    def length = array_length(xs)
    def map[R:ClassTag](f: Rep[T=>R]) = array_map(xs, f)
    def sum(implicit m: RepMonoid[T]) = array_sum(xs)
    def zip[U](ys: Arr[U]): Arr[(T,U)] = array_zip(xs, ys)
  }

  def array_apply[T](xs: Arr[T], n: Rep[Int]): Rep[T]
  def array_length[T](xs: Arr[T]) : Rep[Int]
  def array_map[T,R:ClassTag](xs: Arr[T], f: Rep[T=>R]): Arr[R]
  def array_sum[T](xs: Arr[T])(implicit m: RepMonoid[T]) : Rep[T]
  def array_zip[T,U](xs: Arr[T], ys:Arr[U]): Arr[(T,U)]
  def array_Replicate[T:ClassTag](len: Rep[Int], v: Rep[T]): Arr[T]
}

trait ArrayOpsSeq extends ArrayOps { self: ScalanSeq =>
  def array_apply[T](x: Arr[T], n: Rep[Int]): Rep[T] = x(n)
  def array_length[T](a: Arr[T]) : Rep[Int] = a.length
  def array_map[T, R:ClassTag](xs: Array[T], f: T => R) = Array.tabulate(xs.length)(i => f(xs(i))) //xs.map(f)
  def array_sum[T](xs: Arr[T])(implicit m: RepMonoid[T]) = xs.fold(m.zero)((x,y) => m.append(x,y))
  def array_zip[T,U](xs: Array[T], ys:Array[U]): Array[(T,U)] = (xs, ys).zipped.toArray
  def array_Replicate[T:ClassTag](len: Rep[Int], v: Rep[T]): Arr[T] = Array.fill(len)(v)
}

trait ArrayOpsExp extends ArrayOps { self: ScalanStaged =>
  import TagImplicits._

  def withElemOfArray[T,R](xs: Arr[T])(block: Elem[T] => R): R =
    withElemOf(xs){ eTArr =>
      block(eTArr.ea)
    }

  trait ArrayDef[T] extends Def[Array[T]] {
  }
  trait ArrayMethod[T] {
    def name[A](e: Elem[A]): String
    def xs: Exp[Array[T]]
    lazy val uniqueOpId = withElemOfArray(xs){ name(_) }
  }
  case class ArrayLength[T](xs: Exp[Array[T]]) extends Def[Int] with ArrayMethod[T] {
    def selfType = element[Int]
  }
  case class ArrayApply[T](xs: Exp[Array[T]], index: Exp[Int]) extends Def[T] with ArrayMethod[T] {
    def selfType = withElemOf(xs){ _.ea }
  }
  case class ArrayMap[T,R](xs: Exp[Array[T]], f: Exp[T=>R]) extends ArrayDef[R] with ArrayMethod[T] {
    def selfType = withResultElem(f){implicit eR => element[Array[R]]}
  }
  case class ArraySum[T](xs: Exp[Array[T]], implicit val m: RepMonoid[T]) extends Def[T] with ArrayMethod[T] {
    def selfType = withElemOf(xs){ _.ea }
  }
  case class ArrayZip[T:Elem,U:Elem](xs: Exp[Array[T]], ys: Exp[Array[U]]) extends ArrayDef[(T,U)] {
    lazy val uniqueOpId = name(element[T], element[U])
    def selfType = element[Array[(T,U)]]
  }
  case class ArrayReplicate[T:Elem](len: Exp[Int], v: Exp[T]) extends ArrayDef[T] {
    lazy val uniqueOpId = name(element[T])
    def selfType = element[Array[T]]
  }

  def array_apply[T](xs: Exp[Array[T]], n: Exp[Int]): Rep[T] =
    withElemOfArray(xs){ implicit eT => ArrayApply(xs, n) }
  def array_length[T](a: Exp[Array[T]]) : Rep[Int] = ArrayLength(a)
  def array_map[T, R:ClassTag](xs: Exp[Array[T]], f: Exp[T=>R]) =
    withResultElem(f) { implicit eR => ArrayMap(xs, f) }

  def array_sum[T](xs: Arr[T])(implicit m: RepMonoid[T]) =
    withElemOfArray(xs){ implicit eT => ArraySum(xs, m) }

  def array_zip[T,U](xs: Arr[T], ys:Arr[U]): Arr[(T,U)] = {
    implicit val eT = withElemOf(xs){ _.ea }
    implicit val eU = withElemOf(ys){ _.ea }
    ArrayZip(xs, ys)
  }

  def array_Replicate[T:ClassTag](len: Rep[Int], v: Rep[T]): Arr[T] =
    withElemOf(v){ implicit eT =>
      ArrayReplicate(len, v)
    }
}

