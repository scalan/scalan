package scalan.arrays

import scala.reflect.ClassTag
import scalan.common.OverloadHack.Overloaded1
import scalan.staged.BaseExp
import scalan.{Scalan, ScalanExp, ScalanSeq}

trait ArrayOps { self: Scalan =>
  type Arr[T] = Rep[Array[T]]
  implicit class RepArrayOps[T: Elem](xs: Arr[T]) {
    def apply(n: Rep[Int]): Rep[T] = array_apply(xs, n)
    def apply(ns: Arr[Int])(implicit o: Overloaded1): Arr[T] = array_applyMany(xs, ns)
    def length = array_length(xs)
    def mapBy[R: Elem](f: Rep[T => R]) = array_map(xs, f)
    def map[R: Elem](f: Rep[T] => Rep[R]) = array_map(xs, fun(f))
    def reduce(implicit m: RepMonoid[T]) = array_reduce(xs)

    def fold[S: Elem](init: Rep[S], f: Rep[((S, T)) => S]): Rep[S] = array_fold[T, S](xs, init, f)

    def scan(implicit m: RepMonoid[T]) = array_scan(xs)
    def zip[U](ys: Arr[U]): Arr[(T, U)] = array_zip(xs, ys)
    def slice(start: Rep[Int], length: Rep[Int]): Arr[T] = array_slice(xs, start, length)
    def filterBy(f: Rep[T => Boolean]) = array_filter(xs, f)
    def filter(f: Rep[T] => Rep[Boolean]) = array_filter(xs, fun(f))
    def grouped(size: Rep[Int]) = array_grouped(xs, size)
    def stride(start: Rep[Int], length: Rep[Int], stride: Rep[Int]) =
      array_stride(xs, start, length, stride)
    def update(index: Rep[Int], value: Rep[T]) = array_update(xs, index, value)
    def updateMany(indexes: Arr[Int], values: Arr[T]) = array_updateMany(xs, indexes, values)
  }

  class ArrayCompanion {
    def rangeFrom0(n: Rep[Int]) = array_rangeFrom0(n)
    def tabulate[T: Elem](n: Rep[Int])(f: Rep[Int] => Rep[T]): Arr[T] =
      rangeFrom0(n).map(f)
    def replicate[T: Elem](len: Rep[Int], v: Rep[T]) = array_replicate(len, v)
    def empty[T: Elem] = replicate(0, element[T].defaultRepValue)
  }
  val Array: ArrayCompanion

  // require: n in xs.indices
  def array_apply[T](xs: Arr[T], n: Rep[Int]): Rep[T]

  // require: forall i -> is(i) in xs.indices
  // provide: res.length == is.length
  def array_applyMany[T](xs: Arr[T], is: Arr[Int]): Arr[T]

  def array_length[T](xs: Arr[T]): Rep[Int]

  // provide: xs.length == res.length
  def array_map[T, R: Elem](xs: Arr[T], f: Rep[T => R]): Arr[R]
  
  def array_reduce[T](xs: Arr[T])(implicit m: RepMonoid[T]): Rep[T]
  def array_fold[T,S:Elem](xs: Arr[T], init:Rep[S], f:Rep[((S,T))=>S]): Rep[S]

  // provide: res._1.length == xs.length && res._2 = array_reduce(xs)
  def array_scan[T](xs: Arr[T])(implicit m: RepMonoid[T], elem : Elem[T]): Rep[(Array[T], T)]

  // require: xs.length == ys.length
  // provide: res.length == xs.length
  def array_zip[T, U](xs: Arr[T], ys: Arr[U]): Arr[(T, U)]
  
  // provide: res.length == len
  def array_replicate[T: Elem](len: Rep[Int], v: Rep[T]): Arr[T]
  
  // require: start in xs.indices && start + length in xs.indices
  // provide: res.length == length
  def array_slice[T](xs: Arr[T], start: Rep[Int], length: Rep[Int]): Arr[T]
  
  // provide: res.length = n
  def array_rangeFrom0(n: Rep[Int]): Arr[Int]
  
  def array_filter[T](xs: Arr[T], f: Rep[T => Boolean]): Arr[T]
  def array_grouped[T](xs: Arr[T], size: Rep[Int]): Arr[Array[T]]
  
  // require: start in xs.indices && start + length * stride in xs.indices
  // provide: res.length == length
  def array_stride[T](xs: Arr[T], start: Rep[Int], length: Rep[Int], stride: Rep[Int]): Arr[T]
  
  // require: index in xs.indices
  // provide: res.length == xs.length 
  def array_update[T](xs: Arr[T], index: Rep[Int], value: Rep[T]): Arr[T] = ???
  
  // require: forall i -> indexes(i) in xs.indices && indexes.length == values.length
  // provide: res.length == xs.length 
  def array_updateMany[T](xs: Arr[T], indexes: Arr[Int], values: Arr[T]): Arr[T] = ???
}

trait ArrayOpsSeq extends ArrayOps { self: ScalanSeq =>
  import TagImplicits.elemToClassTag

  class ArrayCompanion1 extends ArrayCompanion {
    @inline
    def apply[T: ClassTag](xs: T*) = scala.Array(xs: _*)
  }
  val Array: ArrayCompanion1 = new ArrayCompanion1

  def array_apply[T](x: Arr[T], n: Rep[Int]): Rep[T] = x(n)
  def array_applyMany[T](x: Arr[T], is: Arr[Int]): Arr[T] = {
    implicit val ct = arrayToClassTag(x)
    scala.Array.tabulate(is.length)(i => x(is(i)))
  }
  def array_length[T](a: Arr[T]): Rep[Int] = a.length
  def array_map[T, R: Elem](xs: Array[T], f: T => R) = genericArrayOps(xs).map(f)
  def array_reduce[T](xs: Arr[T])(implicit m: RepMonoid[T]) = xs.fold(m.zero)((x, y) => m.append((x, y)))
  def array_fold[T, S: Elem](xs: Arr[T], init: Rep[S], f: Rep[((S, T)) => S]): Rep[S] = {
    var state = init
    for (x <- xs) {
      state = f((state, x))
    }
    state
  }
  def array_zip[T, U](xs: Array[T], ys: Array[U]): Array[(T, U)] = (xs, ys).zipped.toArray
  def array_scan[T](xs: Array[T])(implicit m: RepMonoid[T], elem : Elem[T]): Rep[(Array[T], T)] = {
    val scan = xs.scan(m.zero)((x, y) => m.append((x, y)))
    val sum = scan.last
    (scan.dropRight(1).toArray, sum)
  }
  def array_replicate[T: Elem](len: Rep[Int], v: Rep[T]): Arr[T] = scala.Array.fill(len)(v)
  def array_slice[T](xs: Arr[T], start: Rep[Int], length: Rep[Int]): Arr[T] =
    genericArrayOps(xs).slice(start, start + length)
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

trait ArrayOpsExp extends ArrayOps with BaseExp { self: ScalanExp =>
  def withElemOfArray[T, R](xs: Arr[T])(block: Elem[T] => R): R =
    withElemOf(xs) { eTArr =>
      block(eTArr.eItem)
    }

  trait ArrayDef[T] extends Def[Array[T]] {
    implicit def eT: Elem[T]
    lazy val selfType = element[Array[T]]
    lazy val uniqueOpId = name(eT)
  }
  trait ArrayMethod[T] {
    def name[A](e: Elem[A]): String
    def xs: Exp[Array[T]]
    lazy val uniqueOpId = withElemOfArray(xs) { name(_) }
  }
  case class ArrayLength[T](xs: Exp[Array[T]]) extends Def[Int] with ArrayMethod[T] {
    def selfType = element[Int]
    override def mirror(t: Transformer) = ArrayLength(t(xs))
  }
  case class ArrayApply[T](xs: Exp[Array[T]], index: Exp[Int]) extends Def[T] with ArrayMethod[T] {
    lazy val selfType = xs.elem.eItem
    override def mirror(t: Transformer) = ArrayApply(t(xs), t(index))
  }
  case class ArrayApplyMany[T](xs: Exp[Array[T]], indices: Exp[Array[Int]]) extends ArrayDef[T] {
    lazy val eT = xs.elem.eItem
    override def mirror(t: Transformer) = ArrayApplyMany(t(xs), t(indices))
  }
  case class ArrayMap[T, R](xs: Exp[Array[T]], f: Exp[T => R]) extends ArrayDef[R] {
    implicit lazy val eT = withResultElem(f) { e => e }
    override def mirror(t: Transformer) = ArrayMap(t(xs), t(f))
  }
  case class ArrayReduce[T](xs: Exp[Array[T]], implicit val m: RepMonoid[T]) extends Def[T] with ArrayMethod[T] {
    def selfType = xs.elem.eItem
    override def mirror(t: Transformer) = ArrayReduce[T](t(xs), m)
  }
  case class ArrayFold[T,S:Elem](xs: Exp[Array[T]], init:Exp[S], f:Exp[((S,T))=>S]) extends BaseDef[S] with ArrayMethod[T] {
    override def mirror(t: Transformer) = ArrayFold(t(xs), t(init), t(f))
  }
  case class ArrayScan[T](xs: Exp[Array[T]], implicit val m: RepMonoid[T])(implicit val selfType: Elem[(Array[T], T)]) extends Def[(Array[T], T)] with ArrayMethod[T] {
    override def mirror(t: Transformer) = ArrayScan[T](t(xs), m)
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

  val Array: ArrayCompanion = new ArrayCompanion
  def array_apply[T](xs: Exp[Array[T]], n: Exp[Int]): Rep[T] =
    withElemOfArray(xs) { implicit eT => ArrayApply(xs, n) }
  def array_applyMany[T](xs: Exp[Array[T]], is: Exp[Array[Int]]): Arr[T] =
    withElemOfArray(xs) { implicit eT => ArrayApplyMany(xs, is) }
  def array_length[T](a: Exp[Array[T]]): Rep[Int] = ArrayLength(a)
  def array_map[T, R: Elem](xs: Exp[Array[T]], f: Exp[T => R]) = ArrayMap(xs, f)

  def array_reduce[T](xs: Arr[T])(implicit m: RepMonoid[T]) =
    withElemOfArray(xs) { implicit eT => ArrayReduce(xs, m) }
  def array_fold[T,S:Elem](xs: Arr[T], init:Rep[S], f:Rep[((S,T))=>S]): Rep[S] =
    withElemOfArray(xs) { implicit eT => ArrayFold(xs, init, f) }

  def array_scan[T](xs: Arr[T])(implicit m: RepMonoid[T], elem : Elem[T]): Rep[(Array[T], T)] =
    ArrayScan(xs, m)

  def array_zip[T, U](xs: Arr[T], ys: Arr[U]): Arr[(T, U)] = {
    implicit val eT = xs.elem.eItem
    implicit val eU = ys.elem.eItem
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
    implicit val eT = xs.elem.eItem
    Array.tabulate(xs.length div size) { i => xs.slice(i * size, size) }
  }

  def array_stride[T](xs: Arr[T], start: Rep[Int], length: Rep[Int], stride: Rep[Int]): Arr[T] = {
    implicit val eT = xs.elem.eItem
    ArrayStride(xs, start, length, stride)
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    case ArrayApply(Def(d2), i) => d2 match {
      case ArrayApplyMany(xs, is) =>
        implicit val eT = xs.elem.eItem
        xs(is(i))
      case ArrayMap(xs, f) =>
        implicit val eT = xs.elem.eItem
        f(xs(i))
      case ArrayZip(xs: Arr[a] @unchecked, ys: Arr[b] @unchecked) =>
        val xs1 = xs.asRep[Array[a]]
        val ys1 = ys.asRep[Array[b]]
        implicit val e1 = xs1.elem.eItem
        implicit val e2 = ys1.elem.eItem
        (RepArrayOps(xs1)(e1)(i), RepArrayOps(ys1)(e2)(i))
      case ArrayReplicate(_, x) => x
      case ArrayStride(xs, start, _, stride) =>
        implicit val eT = xs.elem.eItem
        xs(start + i * stride)
      case ArrayRangeFrom0(_) => i
      case _ =>
        super.rewriteDef(d)
    }
    case ArrayApplyMany(Def(d2: Def[Array[a]] @unchecked), is) =>
      d2.asDef[Array[a]] match {
        case ArrayApplyMany(xs, is1) =>
          implicit val eT = xs.elem.eItem
          xs(is1(is))
        case ArrayMap(xs, f) =>
          implicit val eT = xs.elem.eItem
          xs(is).mapBy(f)
        case ArrayZip(xs: Arr[a] @unchecked, ys: Arr[b] @unchecked) =>
          val xs1 = xs.asRep[Array[a]]
          val ys1 = ys.asRep[Array[b]]
          implicit val e1 = xs1.elem.eItem
          implicit val e2 = ys1.elem.eItem
          (RepArrayOps(xs1)(e1)(is), RepArrayOps(ys1)(e2)(is))
        case ArrayReplicate(_, x) =>
          implicit val eT = x.elem
          Array.replicate(is.length, x)
        case ArrayStride(xs, start, _, stride) =>
          implicit val eT = xs.elem.eItem
          xs(is.map { i => start + i * stride})
        case ArrayRangeFrom0(_) => is
        case _ =>
          super.rewriteDef(d)
      }
    case ArrayLength(Def(d2: Def[Array[a]]@unchecked)) =>
      d2.asDef[Array[a]] match {
        case Const(scalaArray) => toRep(scalaArray.length)
        case ArrayApplyMany(_, is) => is.length
        case ArrayMap(xs, _) =>
          implicit val eT = xs.elem.eItem
          xs.length
        case ArrayZip(xs, _) =>
          implicit val eT = xs.elem.eItem
          xs.length
        case ArrayReplicate(length, _) => length
        case ArrayStride(_, _, length, _) => length
        case ArrayRangeFrom0(n) => n
        case _ =>
          super.rewriteDef(d)
      }
    case ArrayMap(xs, Def(l: Lambda[_, _])) if l.isIdentity => xs
    case ArrayMap(Def(d2), f: Rep[Function1[a, b]]@unchecked) =>
      d2.asDef[Array[a]] match {
        case ArrayMap(xs: Rep[Array[c]]@unchecked, g) =>
          val xs1 = xs.asRep[Array[c]]
          val g1 = g.asRep[c => a]
          implicit val eB = f.elem.eRange
          implicit val eC = xs.elem.eItem
          xs1.map { x => f(g1(x))}
        case ArrayReplicate(length, x) =>
          implicit val eB = f.elem.eRange
          Array.replicate(length, f(x))
        case _ =>
          super.rewriteDef(d)
      }
    case ArrayFilter(Def(d2: Def[Array[a]]@unchecked), f) =>
      d2.asDef[Array[a]] match {
        case ArrayFilter(xs, g) =>
          implicit val eT = xs.elem.eItem
          xs.filter { x => f(x) && g(x)}
        case _ =>
          super.rewriteDef(d)
      }
    case ArrayZip(Def(ArrayReplicate(len, v1: Rep[a])), Def(ArrayReplicate(_, v2: Rep[b]))) =>
      implicit val eA = v1.elem
      implicit val eB = v2.elem
      Array.replicate(len, (v1, v2))
    case _ => super.rewriteDef(d)
  }
}
