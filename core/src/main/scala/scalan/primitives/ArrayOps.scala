package scalan.primitives

import java.io.{BufferedReader, FileReader, PrintWriter}
import scalan.{ScalanStaged, ScalanSeq, Scalan}
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag
import scalan.staged.BaseExp
import scalan.common.OverloadHack.Overloaded1

trait ArrayOps { self: Scalan  =>
  type Arr[T] = Rep[Array[T]]
  implicit class RepArrayOps[T](xs: Arr[T]) {
    def apply(n: Rep[Int]): Rep[T] = array_apply(xs, n)
    def apply(ns: Arr[Int])(implicit eT: Elem[T]): Arr[T] = array_apply(xs, ns)
    def length = array_length(xs)
    def map[R:Elem](f: Rep[T=>R]) = array_map(xs, f)
    // def map[R:Elem](f: Rep[T] => Rep[R])(implicit o: Overloaded1) = array_map(xs, fun(f))
    def sum(implicit m: RepMonoid[T]) = array_sum(xs)
    def zip[U](ys: Arr[U]): Arr[(T,U)] = array_zip(xs, ys)
    def slice(offset: Rep[Int], length: Rep[Int]): Arr[T] = array_slice(xs, offset, length)
    def filter(f: Rep[T=>Boolean]) = array_filter(xs, f)
    // def filter(f: Rep[T] => Rep[Boolean])(implicit o: Overloaded1) = array_filter(xs, fun(f))    
  }

  def array_apply[T](xs: Arr[T], n: Rep[Int]): Rep[T]
  def array_apply[T: Elem](xs: Arr[T], is: Arr[Int])(implicit o: Overloaded1): Arr[T]
  def array_length[T](xs: Arr[T]) : Rep[Int]
  def array_map[T,R:Elem](xs: Arr[T], f: Rep[T=>R]): Arr[R]
  def array_sum[T](xs: Arr[T])(implicit m: RepMonoid[T]) : Rep[T]
  def array_zip[T,U](xs: Arr[T], ys:Arr[U]): Arr[(T,U)]
  def array_replicate[T:Elem](len: Rep[Int], v: Rep[T]): Arr[T]
  def array_slice[T](xs: Arr[T], offset: Rep[Int], length: Rep[Int]): Arr[T]
  def array_rangeFrom0(n: Rep[Int]): Arr[Int]
  def array_filter[T](xs: Arr[T], f: Rep[T => Boolean]): Arr[T]
}

trait ArrayOpsSeq extends ArrayOps { self: ScalanSeq =>
  import TagImplicits.elemToClassTag
  def array_apply[T](x: Arr[T], n: Rep[Int]): Rep[T] = x(n)
  def array_apply[T: Elem](x: Arr[T], is: Arr[Int])(implicit o: Overloaded1): Arr[T] = 
    Array.tabulate(is.length)(i => x(is(i)))
  def array_length[T](a: Arr[T]) : Rep[Int] = a.length
  def array_map[T, R:Elem](xs: Array[T], f: T => R) = genericArrayOps(xs).map(f)
  def array_sum[T](xs: Arr[T])(implicit m: RepMonoid[T]) = xs.fold(m.zero)((x,y) => m.append(x,y))
  def array_zip[T,U](xs: Array[T], ys:Array[U]): Array[(T,U)] = (xs, ys).zipped.toArray
  def array_replicate[T:Elem](len: Rep[Int], v: Rep[T]): Arr[T] = Array.fill(len)(v)
  def array_slice[T](xs: Arr[T], offset: Rep[Int], length: Rep[Int]): Arr[T] = 
    genericArrayOps(xs).slice(offset, length)
  def array_rangeFrom0(n: Rep[Int]): Arr[Int] = 0.until(n).toArray
  def array_filter[T](xs: Array[T], f: T => Boolean): Array[T] = 
    genericArrayOps(xs).filter(f)
}

trait ArrayOpsExp extends ArrayOps with BaseExp { self: ScalanStaged =>
  def withElemOfArray[T,R](xs: Arr[T])(block: Elem[T] => R): R =
    withElemOf(xs){ eTArr =>
      block(eTArr.ea)
    }

  trait ArrayDef[T] extends Def[Array[T]] {
    implicit def eT: Elem[T]
    lazy val self: Rep[Array[T]] = this
    lazy val selfType = element[Array[T]]
    lazy val uniqueOpId = name(eT)
  }
  trait ArrayMethod[T] {
    def name[A](e: Elem[A]): String
    def xs: Exp[Array[T]]
    lazy val uniqueOpId = withElemOfArray(xs){ name(_) }
  }
  case class ArrayLength[T](xs: Exp[Array[T]]) extends Def[Int] with ArrayMethod[T] {
    lazy val self: Rep[Int] = this
    def selfType = element[Int]
    override def mirror(t: Transformer) = ArrayLength(t(xs))
  }
  case class ArrayApply[T](xs: Exp[Array[T]], index: Exp[Int]) extends Def[T] with ArrayMethod[T] {
    implicit lazy val eT = withElemOfArray(xs){e => e}
    def selfType = eT
    lazy val self: Rep[T] = this
    override def mirror(t: Transformer) = ArrayApply(t(xs), t(index))
  }
  case class ArrayApplyMany[T](xs: Exp[Array[T]], indices: Exp[Array[Int]]) extends ArrayDef[T] {
    implicit lazy val eT = withElemOfArray(xs){e => e}
    override def mirror(t: Transformer) = ArrayApplyMany(t(xs), t(indices))
  }
  case class ArrayMap[T,R](xs: Exp[Array[T]], f: Exp[T=>R]) extends ArrayDef[R] {
    implicit lazy val eT = withResultElem(f){e => e}
    override def mirror(t: Transformer) = ArrayMap(t(xs), t(f))
  }
  case class ArraySum[T](xs: Exp[Array[T]], implicit val m: RepMonoid[T]) extends Def[T] with ArrayMethod[T] {
    implicit lazy val eT = withElemOfArray(xs){e => e}
    def selfType = eT
    lazy val self: Rep[T] = this
    override def mirror(t: Transformer) = ArraySum[T](t(xs), m)
  }
  case class ArrayZip[T:Elem,U:Elem](xs: Exp[Array[T]], ys: Exp[Array[U]]) extends ArrayDef[(T,U)] {
    lazy val eT = element[(T, U)]
    override def mirror(t: Transformer) = ArrayZip(t(xs), t(ys))
  }
  case class ArrayReplicate[T](len: Exp[Int], v: Exp[T])(implicit val eT: Elem[T]) extends ArrayDef[T] {
    override def mirror(t: Transformer) = ArrayReplicate(t(len), t(v))
  }
  case class ArraySlice[T](xs: Exp[Array[T]], offset: Exp[Int], length: Exp[Int])(implicit val eT: Elem[T]) extends ArrayDef[T] {
    override def mirror(t: Transformer) = ArraySlice(t(xs), t(offset), t(length))
  }
  case class ArrayRangeFrom0(n: Exp[Int]) extends ArrayDef[Int] {
    def eT = element[Int]
    override def mirror(t: Transformer) = ArrayRangeFrom0(t(n))
  }
  case class ArrayFilter[T](xs: Exp[Array[T]], f: Exp[T=>Boolean])(implicit val eT: Elem[T]) extends ArrayDef[T] {
    override def mirror(t: Transformer) = ArrayFilter(t(xs), t(f))
  }

  def array_apply[T](xs: Exp[Array[T]], n: Exp[Int]): Rep[T] =
    withElemOfArray(xs){ implicit eT => ArrayApply(xs, n) }
  def array_apply[T: Elem](xs: Exp[Array[T]], is: Exp[Array[Int]])(implicit o: Overloaded1): Arr[T] =
    ArrayApplyMany(xs, is)
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

  def array_rangeFrom0(n: Rep[Int]): Arr[Int] = 
    ArrayRangeFrom0(n)
    
  def array_filter[T](xs: Arr[T], f: Rep[T => Boolean]): Arr[T] =
    withElemOfArray(xs) { implicit eT => ArrayFilter(xs, f) }
    
  override def rewrite[T](d: Exp[T])(implicit eT: LElem[T]) = d match {
    case Def(d1) => d1 match {
      case ArrayMap(xs, Def(l: Lambda[_,_])) if l.isIdentity => xs
      case _ => super.rewrite(d)
    }
    case _ => super.rewrite(d)
  }
}

