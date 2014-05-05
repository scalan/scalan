package scalan.primitives

import java.io.{BufferedReader, FileReader, PrintWriter}
import scalan.{ScalanStaged, ScalanSeq, Scalan}
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag
import scalan.staged.BaseExp

trait ArrayOps { self: Scalan  =>
  type Arr[T] = Rep[Array[T]]
  implicit class RepArrayOps[T](xs: Arr[T]) {
    def apply(n: Rep[Int]) = array_apply(xs, n)
    def length = array_length(xs)
    def map[R:Elem](f: Rep[T=>R]) = array_map(xs, f)
    def sum(implicit m: RepMonoid[T]) = array_sum(xs)
    def zip[U](ys: Arr[U]): Arr[(T,U)] = array_zip(xs, ys)
    def slice(offset: Rep[Int], length: Rep[Int]): Arr[T] = array_slice(xs, offset, length)
  }

  def array_apply[T](xs: Arr[T], n: Rep[Int]): Rep[T]
  def array_length[T](xs: Arr[T]) : Rep[Int]
  def array_map[T,R:Elem](xs: Arr[T], f: Rep[T=>R]): Arr[R]
  def array_sum[T](xs: Arr[T])(implicit m: RepMonoid[T]) : Rep[T]
  def array_zip[T,U](xs: Arr[T], ys:Arr[U]): Arr[(T,U)]
  def array_replicate[T:Elem](len: Rep[Int], v: Rep[T]): Arr[T]
  def array_slice[T](xs: Arr[T], offset: Rep[Int], length: Rep[Int]): Arr[T]
}

trait ArrayOpsSeq extends ArrayOps { self: ScalanSeq =>
  import TagImplicits.elemToClassTag
  def array_apply[T](x: Arr[T], n: Rep[Int]): Rep[T] = x(n)
  def array_length[T](a: Arr[T]) : Rep[Int] = a.length
  def array_map[T, R:Elem](xs: Array[T], f: T => R) = Array.tabulate(xs.length)(i => f(xs(i))) //xs.map(f)
  def array_sum[T](xs: Arr[T])(implicit m: RepMonoid[T]) = xs.fold(m.zero)((x,y) => m.append(x,y))
  def array_zip[T,U](xs: Array[T], ys:Array[U]): Array[(T,U)] = (xs, ys).zipped.toArray
  def array_replicate[T:Elem](len: Rep[Int], v: Rep[T]): Arr[T] = Array.fill(len)(v)
  def array_slice[T](xs: Arr[T], offset: Rep[Int], length: Rep[Int]): Arr[T] = 
    genericArrayOps(xs).slice(offset, length)
}

trait ArrayOpsExp extends ArrayOps with BaseExp { self: ScalanStaged =>
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
    override def mirror(t: Transformer) = ArrayLength(t(xs))
  }
  case class ArrayApply[T](xs: Exp[Array[T]], index: Exp[Int]) extends Def[T] with ArrayMethod[T] {
    implicit lazy val eT = withElemOfArray(xs){e => e}
    def selfType = eT
    override def mirror(t: Transformer) = ArrayApply(t(xs), t(index))
  }
  case class ArrayMap[T,R](xs: Exp[Array[T]], f: Exp[T=>R]) extends ArrayDef[R] with ArrayMethod[T] {
    implicit lazy val eR = withResultElem(f){e => e}
    def selfType = element[Array[R]]
    override def mirror(t: Transformer) = ArrayMap(t(xs), t(f))
  }
  case class ArraySum[T](xs: Exp[Array[T]], implicit val m: RepMonoid[T]) extends Def[T] with ArrayMethod[T] {
    implicit lazy val eT = withElemOfArray(xs){e => e}
    def selfType = eT
    override def mirror(t: Transformer) = ArraySum[T](t(xs), m)
  }
  case class ArrayZip[T:Elem,U:Elem](xs: Exp[Array[T]], ys: Exp[Array[U]]) extends ArrayDef[(T,U)] {
    lazy val uniqueOpId = name(element[T], element[U])
    def selfType = element[Array[(T,U)]]
    override def mirror(t: Transformer) = ArrayZip(t(xs), t(ys))
  }
  case class ArrayReplicate[T:Elem](len: Exp[Int], v: Exp[T]) extends ArrayDef[T] {
    lazy val uniqueOpId = name(element[T])
    def selfType = element[Array[T]]
    override def mirror(t: Transformer) = ArrayReplicate(t(len), t(v))
  }
  case class ArraySlice[T: Elem](xs: Exp[Array[T]], offset: Exp[Int], length: Exp[Int]) extends ArrayDef[T] {
    lazy val uniqueOpId = name(element[T])
    def selfType = element[Array[T]]
    override def mirror(t: Transformer) = ArraySlice(t(xs), t(offset), t(length))
  }

  def array_apply[T](xs: Exp[Array[T]], n: Exp[Int]): Rep[T] =
    withElemOfArray(xs){ implicit eT => ArrayApply(xs, n) }
  def array_length[T](a: Exp[Array[T]]) : Rep[Int] = ArrayLength(a)
  def array_map[T, R:Elem](xs: Exp[Array[T]], f: Exp[T=>R]) = ArrayMap(xs, f)

  def array_sum[T](xs: Arr[T])(implicit m: RepMonoid[T]) =
    withElemOfArray(xs){ implicit eT => ArraySum(xs, m) }

  def array_zip[T,U](xs: Arr[T], ys:Arr[U]): Arr[(T,U)] = {
    implicit val eT = xs.elem.ea
    implicit val eU = ys.elem.ea
    ArrayZip(xs, ys)
  }

  def array_replicate[T:Elem](len: Rep[Int], v: Rep[T]): Arr[T] =
    ArrayReplicate(len, v)
  
  def array_slice[T](xs: Arr[T], offset: Rep[Int], length: Rep[Int]): Arr[T] = 
    withElemOfArray(xs) { implicit eT => ArraySlice(xs, offset, length) }

  override def rewrite[T](d: Exp[T])(implicit eT: LElem[T]) = d match {
    case Def(d1) => d1 match {
      case ArrayMap(xs, Def(l: Lambda[_,_])) if l.isIdentity => xs
      case _ => super.rewrite(d)
    }
    case _ => super.rewrite(d)
  }
}

