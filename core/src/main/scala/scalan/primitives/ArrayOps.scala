package scalan.primitives

import java.io.{ BufferedReader, FileReader, PrintWriter }
import scalan.{ ScalanStaged, ScalanSeq, Scalan }
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag
import scalan.staged.BaseExp
import scalan.common.OverloadHack.Overloaded1

trait ArrayOps { self: Scalan =>
  type Arr[T] = Rep[Array[T]]
  implicit class RepArrayOps[T: Elem](xs: Arr[T]) {
    def apply(n: Rep[Int]): Rep[T] = array_apply(xs, n)
    def apply(ns: Arr[Int])(implicit o: Overloaded1): Arr[T] = array_applyMany(xs, ns)
    def length = array_length(xs)
    def mapBy[R: Elem](f: Rep[T => R]) = array_map(xs, f)
    def map[R: Elem](f: Rep[T] => Rep[R])(implicit o: Overloaded1) = array_map(xs, fun(f))
    def sum(implicit m: RepMonoid[T]) = array_sum(xs)
    def zip[U](ys: Arr[U]): Arr[(T, U)] = array_zip(xs, ys)
    def slice(start: Rep[Int], length: Rep[Int]): Arr[T] = array_slice(xs, start, length)
    def filterBy(f: Rep[T => Boolean]) = array_filter(xs, f)
    def filter(f: Rep[T] => Rep[Boolean])(implicit o: Overloaded1) = array_filter(xs, fun(f))    
    def grouped(size: Rep[Int]) = array_grouped(xs, size)
    def stride[T](start: Rep[Int], length: Rep[Int], stride: Rep[Int]) =
      array_stride(xs, start, length, stride)
    def update(index: Rep[Int], value: Rep[T]) = array_update(xs, index, value)
    def updateMany(indexes: Arr[Int], values: Arr[T]) = array_updateMany(xs, indexes, values)
  }
  
  object Array {
    def rangeFrom0(n: Rep[Int]) = array_rangeFrom0(n)
    def tabulate[T: Elem](n: Rep[Int])(f: Rep[Int] => Rep[T]): Arr[T] =
      rangeFrom0(n).map(f)
    def replicate[T: Elem](len: Rep[Int], v: Rep[T]) = array_replicate(len, v)
  }

  def array_apply[T](xs: Arr[T], n: Rep[Int]): Rep[T]
  def array_applyMany[T](xs: Arr[T], is: Arr[Int]): Arr[T]
  def array_length[T](xs: Arr[T]): Rep[Int]
  def array_map[T, R: Elem](xs: Arr[T], f: Rep[T => R]): Arr[R]
  def array_sum[T](xs: Arr[T])(implicit m: RepMonoid[T]): Rep[T]
  def array_zip[T, U](xs: Arr[T], ys: Arr[U]): Arr[(T, U)]
  def array_replicate[T: Elem](len: Rep[Int], v: Rep[T]): Arr[T]
  def array_slice[T](xs: Arr[T], start: Rep[Int], length: Rep[Int]): Arr[T]
  def array_rangeFrom0(n: Rep[Int]): Arr[Int]
  def array_filter[T](xs: Arr[T], f: Rep[T => Boolean]): Arr[T]
  def array_grouped[T](xs: Arr[T], size: Rep[Int]): Arr[Array[T]]
  def array_stride[T](xs: Arr[T], start: Rep[Int], length: Rep[Int], stride: Rep[Int]): Arr[T]
  def array_update[T](xs: Arr[T], index: Rep[Int], value: Rep[T]): Arr[T] = ???
  def array_updateMany[T](xs: Arr[T], indexes: Arr[Int], values: Arr[T]): Arr[T] = ???
}

trait ArrayOpsSeq extends ArrayOps { self: ScalanSeq =>
  import TagImplicits.elemToClassTag
  def array_apply[T](x: Arr[T], n: Rep[Int]): Rep[T] = x(n)
  def array_applyMany[T](x: Arr[T], is: Arr[Int]): Arr[T] = {
    implicit val ct = arrayToClassTag(x)
    scala.Array.tabulate(is.length)(i => x(is(i)))
  }
  def array_length[T](a: Arr[T]): Rep[Int] = a.length
  def array_map[T, R: Elem](xs: Array[T], f: T => R) = genericArrayOps(xs).map(f)
  def array_sum[T](xs: Arr[T])(implicit m: RepMonoid[T]) = xs.fold(m.zero)((x, y) => m.append(x, y))
  def array_zip[T, U](xs: Array[T], ys: Array[U]): Array[(T, U)] = (xs, ys).zipped.toArray
  def array_replicate[T: Elem](len: Rep[Int], v: Rep[T]): Arr[T] = scala.Array.fill(len)(v)
  def array_slice[T](xs: Arr[T], start: Rep[Int], length: Rep[Int]): Arr[T] =
    genericArrayOps(xs).slice(start, length)
  def array_rangeFrom0(n: Rep[Int]): Arr[Int] = 0.until(n).toArray
  def array_filter[T](xs: Array[T], f: T => Boolean): Array[T] =
    genericArrayOps(xs).filter(f)
  def array_grouped[T](xs: Arr[T], size: Rep[Int]): Arr[Array[T]] = {
    implicit val ct = arrayToClassTag(xs)
    xs.iterator.grouped(size).map(_.toArray).toArray
  }
  def array_stride[T](xs: Arr[T], start: Rep[Int], length: Rep[Int], stride: Rep[Int]): Arr[T] = {
    implicit val ct = arrayToClassTag(xs)
    scala.Array.tabulate(length) { i =>
      xs(start + i * stride)
    }
  }

  def arrayToClassTag[T](xs: Rep[Array[T]]): ClassTag[T] = ClassTag(xs.getClass.getComponentType)
}

trait ArrayOpsExp extends ArrayOps with BaseExp { self: ScalanStaged =>
  def withElemOfArray[T, R](xs: Arr[T])(block: Elem[T] => R): R =
    withElemOf(xs) { eTArr =>
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
    lazy val uniqueOpId = withElemOfArray(xs) { name(_) }
  }
  case class ArrayLength[T](xs: Exp[Array[T]]) extends Def[Int] with ArrayMethod[T] {
    lazy val self: Rep[Int] = this
    def selfType = element[Int]
    override def mirror(t: Transformer) = ArrayLength(t(xs))
  }
  case class ArrayApply[T](xs: Exp[Array[T]], index: Exp[Int]) extends Def[T] with ArrayMethod[T] {
    implicit lazy val eT = withElemOfArray(xs) { e => e }
    def selfType = eT
    lazy val self: Rep[T] = this
    override def mirror(t: Transformer) = ArrayApply(t(xs), t(index))
  }
  case class ArrayApplyMany[T](xs: Exp[Array[T]], indices: Exp[Array[Int]]) extends ArrayDef[T] {
    implicit lazy val eT = withElemOfArray(xs) { e => e }
    override def mirror(t: Transformer) = ArrayApplyMany(t(xs), t(indices))
  }
  case class ArrayMap[T, R](xs: Exp[Array[T]], f: Exp[T => R]) extends ArrayDef[R] {
    implicit lazy val eT = withResultElem(f) { e => e }
    override def mirror(t: Transformer) = ArrayMap(t(xs), t(f))
  }
  case class ArraySum[T](xs: Exp[Array[T]], implicit val m: RepMonoid[T]) extends Def[T] with ArrayMethod[T] {
    implicit lazy val eT = withElemOfArray(xs) { e => e }
    def selfType = eT
    lazy val self: Rep[T] = this
    override def mirror(t: Transformer) = ArraySum[T](t(xs), m)
  }
  case class ArrayZip[T: Elem, U: Elem](xs: Exp[Array[T]], ys: Exp[Array[U]]) extends ArrayDef[(T, U)] {
    lazy val eT = element[(T, U)]
    override def mirror(t: Transformer) = ArrayZip(t(xs), t(ys))
  }
  case class ArrayReplicate[T](len: Exp[Int], v: Exp[T])(implicit val eT: Elem[T]) extends ArrayDef[T] {
    override def mirror(t: Transformer) = ArrayReplicate(t(len), t(v))
  }
  case class ArrayStride[T](xs: Exp[Array[T]], start: Exp[Int], length: Exp[Int], stride: Exp[Int])(implicit val eT: Elem[T]) extends ArrayDef[T] {
    override def mirror(t: Transformer) = ArrayStride(t(xs), t(start), t(length), t(stride))
  }
  case class ArrayRangeFrom0(n: Exp[Int]) extends ArrayDef[Int] {
    def eT = element[Int]
    override def mirror(t: Transformer) = ArrayRangeFrom0(t(n))
  }
  case class ArrayFilter[T](xs: Exp[Array[T]], f: Exp[T => Boolean])(implicit val eT: Elem[T]) extends ArrayDef[T] {
    override def mirror(t: Transformer) = ArrayFilter(t(xs), t(f))
  }
//  case class ArrayGrouped[T: Elem](xs: Exp[Array[T]], chunkLength: Exp[Int]) extends ArrayDef[Array[T]] {
//    override lazy val eT = element[Array[T]]
//    override implicit lazy val selfType = arrayElement(eT)
//    override def mirror(t: Transformer) = ArrayGrouped(t(xs), t(chunkLength))
//  }

  def array_apply[T](xs: Exp[Array[T]], n: Exp[Int]): Rep[T] =
    withElemOfArray(xs) { implicit eT => ArrayApply(xs, n) }
  def array_applyMany[T](xs: Exp[Array[T]], is: Exp[Array[Int]]): Arr[T] =
    withElemOfArray(xs) { implicit eT => ArrayApplyMany(xs, is) }
  def array_length[T](a: Exp[Array[T]]): Rep[Int] = ArrayLength(a)
  def array_map[T, R: Elem](xs: Exp[Array[T]], f: Exp[T => R]) = ArrayMap(xs, f)

  def array_sum[T](xs: Arr[T])(implicit m: RepMonoid[T]) =
    withElemOfArray(xs) { implicit eT => ArraySum(xs, m) }

  def array_zip[T, U](xs: Arr[T], ys: Arr[U]): Arr[(T, U)] = {
    implicit val eT = xs.elem.ea
    implicit val eU = ys.elem.ea
    ArrayZip(xs, ys)
  }

  def array_replicate[T: Elem](len: Rep[Int], v: Rep[T]): Arr[T] =
    ArrayReplicate(len, v)

  def array_slice[T](xs: Arr[T], start: Rep[Int], length: Rep[Int]): Arr[T] =
    array_stride(xs, start, length, 1)

  def array_rangeFrom0(n: Rep[Int]): Arr[Int] =
    ArrayRangeFrom0(n)

  def array_filter[T](xs: Arr[T], f: Rep[T => Boolean]): Arr[T] =
    withElemOfArray(xs) { implicit eT => ArrayFilter(xs, f) }

  def array_grouped[T](xs: Arr[T], size: Rep[Int]): Arr[Array[T]] = {
    implicit val eT = xs.elem.ea
    Array.tabulate(xs.length / size) { i => xs.slice(i * size, size) }
    // ArrayGrouped(xs, size) 
  }
  
  def array_stride[T](xs: Arr[T], start: Rep[Int], length: Rep[Int], stride: Rep[Int]): Arr[T] = {
    implicit val eT = xs.elem.ea
    ArrayStride(xs, start, length, stride)
  }

  override def rewrite[T](d: Exp[T])(implicit eT: LElem[T]) = d match {
    case Def(d1) => d1 match {
      case ArrayApply(Def(ArrayRangeFrom0(_)), i) => i
      case ArrayApply(Def(ArrayStride(xs, start, _, stride)), i) =>
        implicit val eT = xs.elem.ea
        xs(start + i * stride)
      case ArrayApply(Def(ArrayMap(xs, f)), i) =>
        implicit val eT = xs.elem.ea
        f(xs(i))
      // TODO doesn't compile
      // case ArrayApplyMany(Def(ArrayRangeFrom0(_)), is) => is
      case ArrayApplyMany(Def(ArrayStride(xs, start, _, stride)), is) =>
        implicit val eT = xs.elem.ea
        xs(is.map { i => start + i * stride })
      case ArrayApplyMany(Def(ArrayMap(xs, f)), is) =>
        implicit val eT = xs.elem.ea
        xs(is).mapBy(f)
      // TODO doesn't compile
      // case ArrayLength(Def(ArrayRangeFrom0(n))) => n
      case ArrayLength(Def(ArrayMap(xs, _))) =>
        implicit val eT = xs.elem.ea
        xs.length
      case ArrayMap(xs, Def(l: Lambda[_, _])) if l.isIdentity => xs
      case ArrayMap(Def(ArrayMap(xs, f)), g) =>
        implicit val eT = xs.elem.ea
        xs.map { x => g(f(x)) }
      case ArrayFilter(Def(ArrayMap(xs, f)), g) =>
        implicit val eT = xs.elem.ea
        xs.filter { x => g(f(x)) }
      // must be last ArrayMap rule
      // really ugly, but seems to be necessary
//      case ArrayMap(e2: Exp[Array[Array[a]]] @unchecked, f: Exp[Function1[_, b]] @unchecked) if e2.elem.asInstanceOf[Elem[Array[Any]]].ea.isInstanceOf[ArrayElem[_]] => e2.asRep[Array[Array[a]]] match {
//        case Def(ArrayGrouped(xs, n)) =>
//          implicit val eA = e2.elem.asInstanceOf[Elem[Array[Array[Any]]]].ea.ea.asInstanceOf[Elem[a]]
//          val f1 = f.asRep[Array[a] => b]
//          implicit val eB = f1.elem.eb
//          Array.tabulate(xs.length / n) { i =>
//            f1(xs.slice(i * n, n))
//          }
//        case _ => super.rewrite(d)
//      }
      case _ => super.rewrite(d)
    }
    case _ => super.rewrite(d)
  }
}
