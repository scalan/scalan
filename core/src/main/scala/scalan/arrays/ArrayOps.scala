package scalan.arrays

import scala.reflect.ClassTag
import scalan.common.OverloadHack.Overloaded1
import scalan.staged.BaseExp
import scalan.{Scalan, ScalanExp, ScalanStd}
import scala.reflect.runtime.universe._

trait ArrayOps { self: Scalan =>
  type Arr[T] = Rep[Array[T]]
  implicit class RepArrayOps[T: Elem](xs: Arr[T]) {
    def apply(n: Rep[Int]): Rep[T] = array_apply(xs, n)

    def apply(ns: Arr[Int])(implicit o: Overloaded1): Arr[T] = array_applyMany(xs, ns)

    def length = array_length(xs)

    def mapBy[R: Elem](f: Rep[T => R]) = array_map(xs, f)

    def map[R: Elem](f: Rep[T] => Rep[R]) = array_map(xs, fun(f))

    def flatMapBy[R: Elem](f: Rep[T => Array[R]]) = array_flat_map(xs, f)

    def flatMap[R: Elem](f: Rep[T] => Arr[R]) = array_flat_map(xs, fun(f))

    def reduce(implicit m: RepMonoid[T]) = array_reduce(xs)

    def fold[S: Elem](init: Rep[S], f: Rep[((S, T)) => S]): Rep[S] = array_fold[T, S](xs, init, f)
    def foldLeft[S: Elem](init: Rep[S])(f: Rep[(S, T)] => Rep[S]): Rep[S] = array_fold[T, S](xs, init, fun(f))

    def mapReduceBy[K: Elem, V: Elem](map: Rep[T => (K, V)], reduce: Rep[((V, V)) => V]) = array_map_reduce(xs, map, reduce)

    def mapReduce[K: Elem, V: Elem](map: Rep[T] => Rep[(K, V)], reduce: (Rep[V], Rep[V]) => Rep[V]) = array_map_reduce(xs, fun(map), fun2(reduce))

    def scan(implicit m: RepMonoid[T]) = array_scan(xs)

    def zip[U](ys: Arr[U]): Arr[(T, U)] = array_zip(xs, ys)

    def sort(implicit o: Ordering[T]): Arr[T] = array_sort(xs)(o)

    def slice(start: Rep[Int], length: Rep[Int]): Arr[T] = array_slice(xs, start, length)

    def filterBy(f: Rep[T => Boolean]) = array_filter(xs, f)

    def filter(f: Rep[T] => Rep[Boolean]) = array_filter(xs, fun(f))

    def find(f: Rep[T] => Rep[Boolean]) = array_find(xs, fun(f))

    def grouped(size: Rep[Int]) = array_grouped(xs, size)

    def stride(start: Rep[Int], length: Rep[Int], stride: Rep[Int]) =
      array_stride(xs, start, length, stride)

    def update(index: Rep[Int], value: Rep[T]) = array_update(xs, index, value)

    def :+(value: Rep[T]) = array_append(xs, value)

    def append(value: Rep[T]) = array_append(xs, value)

    def +:(value: Rep[T]) = array_cons(value, xs)

    def prepend(value: Rep[T]) = array_cons(value, xs)

    def updateMany(indexes: Arr[Int], values: Arr[T]) = array_updateMany(xs, indexes, values)

    // new functions to support SQL-like queries
    def sum(implicit n: Numeric[T]): Rep[T] = array_sum(xs)

    def max(implicit o: Ordering[T]): Rep[T] = array_max(xs)

    def min(implicit o: Ordering[T]): Rep[T] = array_min(xs)

    def avg(implicit n: Numeric[T]): Rep[Double] = array_avg(xs)

    def sortBy[O: Elem](by: Rep[T => O])(implicit o: Ordering[O]): Arr[T] = array_sort_by(xs, by)

    def groupBy[G: Elem](by: Rep[T => G]) = array_group_by(xs, by)

    def count(f: Rep[T => Boolean]) = array_count(xs, f)

    def sumBy[S: Elem](f: Rep[T => S])(implicit n: Numeric[S]): Rep[S] = array_sum_by(xs, f)

    def toList = array_toList(xs)
    def reverse = array_reverse(xs)
  }

  implicit class RepNArrayOps[T: Elem](xss: Arr[Array[T]]) {
    def flatten = array_flatten(xss)
  }

  // name to avoid conflict with scala.Array
  object SArray {
    def rangeFrom0(n: Rep[Int]) = array_rangeFrom0(n)
    def tabulate[T: Elem](n: Rep[Int])(f: Rep[Int] => Rep[T]): Arr[T] =
      rangeFrom0(n).map(f)
    def repeat[T: Elem](n: Rep[Int])(f: Rep[Int => T]): Arr[T] = rangeFrom0(n).mapBy(f)
    def replicate[T: Elem](len: Rep[Int], v: Rep[T]) = array_replicate(len, v)
    def singleton[T: Elem](v: Rep[T]) = array_replicate(1, v)
    def empty[T: Elem] = array_empty[T]
    def fromSyms[T:Elem](syms: Seq[Rep[T]]): Arr[T] = array_fromSyms(syms)
  }

  trait ArrayFunctor extends Functor[Array] {
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[Array[T]]
    def lift[T](implicit eT: Elem[T]) = element[Array[T]]
    def unlift[T](implicit eFT: Elem[Array[T]]) = eFT.eItem
    def getElem[T](fa: Rep[Array[T]]) = rep_getElem(fa)
    def unapply[A](e: Elem[_]) = e match {
      case ae: ArrayElem[_] => Some(ae.asElem[Array[A]])
      case _ => None
    }
    def map[A:Elem,B:Elem](xs: Rep[Array[A]])(f: Rep[A] => Rep[B]) = xs.mapBy(fun(f))
  }
  implicit val arrayContainer: Functor[Array] = new ArrayFunctor {}

  abstract class ArrayElem[A](implicit eItem: Elem[A])
    extends EntityElem1[A, Array[A], Array](eItem, container[Array]) {
  }

  case class ScalaArrayElem[A](override val eItem: Elem[A]) extends ArrayElem[A]()(eItem) {
    def parent: Option[Elem[_]] = Some(arrayElement(eItem))
    override def isEntityType = eItem.isEntityType
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eItem))
    }
    lazy val tag = {
      implicit val tag1 = eItem.tag
      weakTypeTag[Array[A]]
    }
    protected def getDefaultRep =
      SArray.empty(eItem)

    override def canEqual(other: Any) = other.isInstanceOf[ScalaArrayElem[_]]
  }

  // require: n in xs.indices
  def array_apply[T](xs: Arr[T], n: Rep[Int]): Rep[T]

  // require: forall i -> is(i) in xs.indices
  // provide: res.length == is.length
  def array_applyMany[T](xs: Arr[T], is: Arr[Int]): Arr[T]

  def array_length[T](xs: Arr[T]): Rep[Int]

  // provide: xs.length == res.length
  def array_map[T, R: Elem](xs: Arr[T], f: Rep[T => R]): Arr[R]
  def array_flat_map[T, R: Elem](xs: Arr[T], f: Rep[T => Array[R]]): Arr[R]

  def array_reduce[T](xs: Arr[T])(implicit m: RepMonoid[T]): Rep[T]
  def array_fold[T,S:Elem](xs: Arr[T], init:Rep[S], f:Rep[((S,T))=>S]): Rep[S]

  def array_map_reduce[T,K:Elem,V:Elem](xs: Arr[T], map:Rep[T=>(K,V)], reduce:Rep[((V,V))=>V]): MM[K,V]

  // provide: res._1.length == xs.length && res._2 = array_reduce(xs)
  def array_scan[T](xs: Arr[T])(implicit m: RepMonoid[T], elem : Elem[T]): Rep[(Array[T], T)]

  // require: xs.length == ys.length
  // provide: res.length == xs.length
  def array_zip[T, U](xs: Arr[T], ys: Arr[U]): Arr[(T, U)]

  def array_sort[T](xs: Arr[T])(implicit o:Ordering[T]): Arr[T]

  // provide: res.length == len
  def array_replicate[T: Elem](len: Rep[Int], v: Rep[T]): Arr[T]

  // require: start in xs.indices && start + length in xs.indices
  // provide: res.length == length
  def array_slice[T](xs: Arr[T], start: Rep[Int], length: Rep[Int]): Arr[T]

  // provide: res.length = n
  def array_rangeFrom0(n: Rep[Int]): Arr[Int]

  def array_filter[T](xs: Arr[T], f: Rep[T => Boolean]): Arr[T]
  def array_find[T](xs: Arr[T], f: Rep[T => Boolean]): Arr[Int]
  def array_grouped[T](xs: Arr[T], size: Rep[Int]): Arr[Array[T]]

  // require: start in xs.indices && start + length * stride in xs.indices
  // provide: res.length == length
  def array_stride[T](xs: Arr[T], start: Rep[Int], length: Rep[Int], stride: Rep[Int]): Arr[T]

  // require: index in xs.indices
  // provide: res.length == xs.length
  def array_update[T](xs: Arr[T], index: Rep[Int], value: Rep[T]): Arr[T]

  def array_append[T](xs: Arr[T], value: Rep[T]): Arr[T]

  // require: forall i -> indexes(i) in xs.indices && indexes.length == values.length
  // provide: res.length == xs.length
  def array_updateMany[T](xs: Arr[T], indexes: Arr[Int], values: Arr[T]): Arr[T]

  def array_sum[T:Elem](xs: Arr[T])(implicit n: Numeric[T]): Rep[T]
  def array_max[T:Elem](xs: Arr[T])(implicit o: Ordering[T]): Rep[T]
  def array_min[T:Elem](xs: Arr[T])(implicit o: Ordering[T]): Rep[T]
  def array_avg[T:Elem](xs: Arr[T])(implicit n: Numeric[T]): Rep[Double]
  def array_sort_by[T:Elem, O:Elem](xs: Arr[T], by: Rep[T => O])(implicit o:Ordering[O]): Arr[T]
  def array_group_by[T:Elem, G:Elem](xs: Arr[T], by: Rep[T => G]): MM[G, ArrayBuffer[T]]
  def array_count[T:Elem](xs: Arr[T], f: Rep[T => Boolean]): Rep[Int]
  def array_sum_by[T:Elem, S:Elem](xs: Arr[T], f: Rep[T => S])(implicit n: Numeric[S]): Rep[S]

  def array_empty[T: Elem]: Arr[T]
  def array_toList[T:Elem](xs: Arr[T]): Lst[T]
  def array_reverse[T:Elem](xs: Arr[T]): Arr[T]
  def array_flatten[T:Elem](xs: Arr[Array[T]]): Arr[T]
  def array_cons[T:Elem](value: Rep[T], xs: Arr[T]): Arr[T]
  def array_binary_search[T:Elem](i: Rep[T], is: Arr[T])(implicit o: Ordering[T]): Rep[Int]

  def array_randomGaussian(m: Rep[Double], e: Rep[Double], arr: Arr[Double]): Arr[Double]
  def array_fromSyms[T:Elem](syms: Seq[Rep[T]]): Arr[T]

  def array_concat[A: Elem](arrs: Arr[A]*): Arr[A] = {
    val arr: Arr[Array[A]] = SArray.fromSyms(arrs)
    arr.flatten
  }
}

trait ArrayOpsStd extends ArrayOps {
  self: ScalanStd =>

  import TagImplicits.elemToClassTag  // do not remove, it's NOT unused

  def array_apply[T](x: Arr[T], n: Rep[Int]): Rep[T] = x(n)

  def array_applyMany[T](x: Arr[T], is: Arr[Int]): Arr[T] = {
    implicit val ct = arrayToClassTag(x)
    scala.Array.tabulate(is.length)(i => x(is(i)))
  }

  def array_length[T](a: Arr[T]): Rep[Int] = a.length

  def array_map[T, R: Elem](xs: Array[T], f: T => R) = genericArrayOps(xs).map(f)

  def array_flat_map[T, R: Elem](xs: Array[T], f: T => Array[R]) = genericArrayOps(xs).flatMap(x => f(x).toSeq)

  def array_reduce[T](xs: Arr[T])(implicit m: RepMonoid[T]) = xs.fold(m.zero)(m.append)

  def array_fold[T, S: Elem](xs: Arr[T], init: Rep[S], f: Rep[((S, T)) => S]): Rep[S] = {
    var state = init
    for (x <- xs) {
      state = f((state, x))
    }
    state
  }

  def array_update[T](xs: Arr[T], index: Rep[Int], value: Rep[T]): Arr[T] = {
    implicit val ct = arrayToClassTag(xs)
    //xs.update(index, value) //in-place operation contradicts with lms
    val xs1 = xs.clone()
    xs1.update(index, value)
    xs1
  }

  def array_updateMany[T](xs: Arr[T], indexes: Arr[Int], values: Arr[T]): Arr[T] = {
    implicit val ct = arrayToClassTag(xs)
    //(0 until indexes.length).foreach(i => xs.update(indexes(i), values(i))) //in-place operation contradicts with lms
    val xs1 = xs.clone()
    (0 until indexes.length).foreach(i => xs1.update(indexes(i), values(i)))
    xs1
  }

  def array_append[T](xs: Arr[T], value: Rep[T]): Arr[T] = {
    implicit val ct = arrayToClassTag(xs)
    xs :+ value
  }

  def array_sum_by[T: Elem, S: Elem](xs: Arr[T], f: Rep[T => S])(implicit n: Numeric[S]): Rep[S] = {
    var sum = n.zero
    for (x <- xs) {
      sum += f(x)
    }
    sum
  }

  def array_map_reduce[T, K: Elem, V: Elem](xs: Arr[T], map: (T) => (K, V), reduce: ((V, V)) => V) = {
    val result = scala.collection.mutable.Map.empty[K, V]
    xs.foldLeft(result)((r, x) => {
      val pair = map(x)
      val key = pair._1
      val value = pair._2
      result.update(key, if (result.contains(key)) reduce((result(key), value)) else value)
      result
    })
  }

  def array_zip[T, U](xs: Array[T], ys: Array[U]): Array[(T, U)] = (xs, ys).zipped.toArray

  def array_sort[T](xs: Arr[T])(implicit o: Ordering[T]): Arr[T] = {
    scala.util.Sorting.quickSort(xs)
    xs
  }

  def array_scan[T](xs: Array[T])(implicit m: RepMonoid[T], elem: Elem[T]): Rep[(Array[T], T)] = {
    val scan = xs.scan(m.zero)(m.append)
    val sum = scan.last
    (scan.dropRight(1), sum)
  }

  def array_replicate[T: Elem](len: Rep[Int], v: Rep[T]): Arr[T] = scala.Array.fill(len)(v)

  def array_slice[T](xs: Arr[T], start: Rep[Int], length: Rep[Int]): Arr[T] =
    genericArrayOps(xs).slice(start, start + length)

  def array_rangeFrom0(n: Rep[Int]): Arr[Int] = 0.until(n).toArray

  def array_filter[T](xs: Array[T], f: T => Boolean): Array[T] = genericArrayOps(xs).filter(f)

  def array_find[T](xs: Array[T], f: T => Boolean): Array[Int] = {
    val buf = scala.collection.mutable.ArrayBuffer.empty[Int]
    for (i <- xs.indices) {
      if (f(xs(i))) buf += i
    }
    buf.toArray
  }
  def array_count[T:Elem](xs: Arr[T], f: Rep[T] => Rep[Boolean]): Rep[Int] = {
    genericArrayOps(xs).count(f)
  }
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
  def array_sum[T:Elem](xs: Arr[T])(implicit n: Numeric[T]): Rep[T] = genericArrayOps(xs).sum
  def array_max[T:Elem](xs: Arr[T])(implicit o: Ordering[T]): Rep[T] = genericArrayOps(xs).max
  def array_min[T:Elem](xs: Arr[T])(implicit o: Ordering[T]): Rep[T]  = genericArrayOps(xs).min
  def array_avg[T:Elem](xs: Arr[T])(implicit n: Numeric[T]): Rep[Double] = genericArrayOps(xs).sum.toDouble / xs.length
  def array_sort_by[T:Elem, O:Elem](xs: Arr[T], by: Rep[T => O])(implicit o:Ordering[O]): Arr[T] = genericArrayOps(xs).sortBy[O](by)
  def array_group_by[T:Elem, G:Elem](xs: Arr[T], by: Rep[T => G]): MM[G, ArrayBuffer[T]] = {
    val result = scala.collection.mutable.Map.empty[G, ArrayBuffer[T]]
    for (x <- xs) {
      val key = by(x)
      if (result.contains(key)) {
        result(key) += x
      } else {
        result.update(key, ArrayBuffer(x))
      }
    }
    result
  }

  def array_empty[T: Elem]: Arr[T] = scala.Array.empty[T]
  def array_reverse[T:Elem](xs: Arr[T]): Arr[T] = genericArrayOps(xs).reverse
  def array_flatten[T:Elem](xs: Arr[Array[T]]): Arr[T] = genericArrayOps(xs).flatten
  def array_cons[T:Elem](value: Rep[T], xs: Arr[T]): Arr[T] = {
    val buf = new Array[T](xs.length + 1)
    buf(0) = value
    xs.copyToArray(buf, 1)
    buf
  }
  def array_toList[T:Elem](xs: Array[T]): Lst[T] = xs.to[List]
  def arrayToClassTag[T](xs: Rep[Array[T]]): ClassTag[T] = ClassTag(xs.getClass.getComponentType)

  def array_binary_search[T:Elem](i: T, is: Array[T])(implicit o: Ordering[T]): Rep[Int] = {
    element[T] match {
      case BooleanElement =>
        !!!(s"binarySearch isn't defined for array of ${element[T]}")
      case ByteElement =>
        java.util.Arrays.binarySearch(is.asInstanceOf[Array[Byte]], i.asInstanceOf[Byte])
      case CharElement =>
        java.util.Arrays.binarySearch(is.asInstanceOf[Array[Char]], i.asInstanceOf[Char])
      case ShortElement =>
        java.util.Arrays.binarySearch(is.asInstanceOf[Array[Short]], i.asInstanceOf[Short])
      case IntElement =>
        java.util.Arrays.binarySearch(is.asInstanceOf[Array[Int]], i.asInstanceOf[Int])
      case LongElement =>
        java.util.Arrays.binarySearch(is.asInstanceOf[Array[Long]], i.asInstanceOf[Long])
      case FloatElement =>
        java.util.Arrays.binarySearch(is.asInstanceOf[Array[Float]], i.asInstanceOf[Float])
      case DoubleElement =>
        java.util.Arrays.binarySearch(is.asInstanceOf[Array[Double]], i.asInstanceOf[Double])
      case _ => // all others are AnyRef ancestors
        java.util.Arrays.binarySearch(is.asInstanceOf[Array[AnyRef]], i.asInstanceOf[AnyRef])
    }
  }

  def array_randomGaussian(m: Rep[Double], e: Rep[Double], arr: Arr[Double]): Arr[Double] =
    arr.map(_ => scala.util.Random.nextGaussian() * e + m)

  def array_fromSyms[T:Elem](syms: Seq[Rep[T]]): Arr[T] = syms.toArray
}

trait ArrayOpsExp extends ArrayOps with BaseExp { self: ScalanExp =>
  def withElemOfArray[T, R](xs: Arr[T])(block: Elem[T] => R): R = withElemOf(xs) { eTArr => block(eTArr.eItem) }

  abstract class ArrayDef[T](implicit val eItem: Elem[T]) extends Def[Array[T]] {
    lazy val selfType = element[Array[T]]
  }
  case class ArrayLength[T](xs: Exp[Array[T]]) extends BaseDef[Int]
  case class ArrayApply[T](xs: Exp[Array[T]], index: Exp[Int]) extends Def[T] {
    lazy val selfType = xs.elem.eItem
  }
  case class ArrayApplyMany[T](xs: Exp[Array[T]], indices: Exp[Array[Int]]) extends ArrayDef[T]()(xs.elem.eItem)
  case class ArrayMap[T, R](xs: Exp[Array[T]], f: Exp[T => R]) extends ArrayDef[R]()(f.elem.eRange)
  case class ArrayMapFilter[T, R](xs: Exp[Array[T]], f: Exp[T => (Boolean,R)]) extends ArrayDef[R]()(f.elem.eRange.eSnd)
  case class ArrayFlatMap[T, R](xs: Exp[Array[T]], f: Exp[T => Array[R]]) extends ArrayDef[R]()(f.elem.eRange.eItem)
  case class ArrayReduce[T](xs: Exp[Array[T]], implicit val m: RepMonoid[T]) extends Def[T] {
    def selfType = xs.elem.eItem
  }
  case class ArrayFold[T,S](xs: Exp[Array[T]], init:Exp[S], f:Exp[((S,T))=>S])(implicit val eS: Elem[S]) extends BaseDef[S]
  case class ArrayScan[T](xs: Exp[Array[T]], implicit val m: RepMonoid[T])(implicit val eT: Elem[T]) extends BaseDef[(Array[T], T)]
  case class ArraySort[T](xs: Exp[Array[T]], implicit val o:Ordering[T])(implicit eItem: Elem[T]) extends ArrayDef[T]
  case class ArrayZip[T, U](xs: Exp[Array[T]], ys: Exp[Array[U]])(implicit val eT: Elem[T], val eU: Elem[U]) extends ArrayDef[(T, U)]
  case class ArrayMapReduce[T,K,V](in: Exp[Array[T]], map:Exp[T=>(K,V)], reduce:Exp[((V,V))=>V])(implicit val eK: Elem[K], val eV: Elem[V]) extends MMapDef[K,V]

  case class ArrayReplicate[T](len: Exp[Int], v: Exp[T])(implicit eItem: Elem[T]) extends ArrayDef[T]
  case class ArrayStride[T](xs: Exp[Array[T]], start: Exp[Int], length: Exp[Int], stride: Exp[Int])(implicit eItem: Elem[T]) extends ArrayDef[T]
  case class ArrayUpdate[T](xs: Exp[Array[T]], index: Exp[Int], value: Exp[T])(implicit eItem: Elem[T]) extends ArrayDef[T]
  case class ArrayUpdateMany[T](xs: Exp[Array[T]], indexes: Exp[Array[Int]], values: Exp[Array[T]])(implicit eItem: Elem[T]) extends ArrayDef[T]
  case class ArrayAppend[T](xs: Exp[Array[T]], value: Exp[T])(implicit eItem: Elem[T]) extends ArrayDef[T]
  case class ArrayCons[T](value: Exp[T], xs: Exp[Array[T]])(implicit eItem: Elem[T]) extends ArrayDef[T]
  case class ArrayReverse[T](xs: Exp[Array[T]])(implicit eItem: Elem[T]) extends ArrayDef[T]
  case class ArrayFlatten[T](xss: Arr[Array[T]])(implicit eItem: Elem[T]) extends ArrayDef[T]
  case class ArrayRangeFrom0(n: Exp[Int]) extends ArrayDef[Int]
  case class ArrayFilter[T](xs: Exp[Array[T]], f: Exp[T => Boolean])(implicit eItem: Elem[T]) extends ArrayDef[T]
  case class ArrayFind[T](xs: Exp[Array[T]], f: Exp[T => Boolean]) extends ArrayDef[Int]
  case class ArraySortBy[T, O](xs: Exp[Array[T]], f: Exp[T => O], o: Ordering[O])(implicit eItem: Elem[T], val eO: Elem[O]) extends ArrayDef[T]
  case class ArrayGroupBy[T, G](xs: Exp[Array[T]], by: Exp[T => G])(implicit val eT: Elem[T], val eG: Elem[G]) extends MMapDef[G, ArrayBuffer[T]]
  case class ArraySum[T](xs: Exp[Array[T]], n: Numeric[T])(implicit val eT: Elem[T]) extends BaseDef[T]
  case class ArraySumBy[T, S](xs: Exp[Array[T]], f: Exp[T => S], n: Numeric[S])(implicit val eT: Elem[T], val eS: Elem[S]) extends BaseDef[S]
  case class ArrayMax[T](xs: Exp[Array[T]], o: Ordering[T])(implicit val eT: Elem[T]) extends BaseDef[T]
  case class ArrayMin[T](xs: Exp[Array[T]], o: Ordering[T])(implicit val eT: Elem[T]) extends BaseDef[T]
  case class ArrayAvg[T](xs: Exp[Array[T]], n: Numeric[T]) extends BaseDef[Double]
  case class ArrayCount[T](xs: Exp[Array[T]], f: Exp[T => Boolean])(implicit val eT: Elem[T]) extends BaseDef[Int]
  case class ArrayEmpty[T]()(implicit eItem: Elem[T]) extends ArrayDef[T]
  case class ArrayToList[T](xs: Exp[Array[T]])(implicit val eT: Elem[T]) extends ListDef[T]

  case class ArrayBinarySearch[T](i: Exp[T], xs: Exp[Array[T]], o: Ordering[T])(implicit val eT: Elem[T]) extends BaseDef[Int]

  case class ArrayRandomGaussian[T](m: Exp[T], e: Exp[T], xs: Exp[Array[T]])(implicit eItem: Elem[T]) extends ArrayDef[T]

  case class SymsArray[A](symbols: Seq[Exp[A]])(implicit override val eItem: Elem[A]) extends ArrayDef[A] {
    val length: Rep[Int] = symbols.length
  }

  case class ArrayInnerJoin[K, B, C, R](xs: Exp[Array[(K, B)]], ys: Exp[Array[(K, C)]], f: Exp[((B, C)) => R])
                                       (implicit val ordK: Ordering[K], val nK: Numeric[K], val eK: Elem[K],
                                        val eB: Elem[B], val eC: Elem[C], val eR: Elem[R])
    extends ArrayDef[(K, R)]

  case class ArrayOuterJoin[K, B, C, R](xs: Exp[Array[(K, B)]], ys: Exp[Array[(K, C)]], f: Exp[((B, C)) => R],
                                        f1: Exp[B => R], f2: Exp[C => R])
                                       (implicit val ordK: Ordering[K], val nK: Numeric[K], val eK: Elem[K],
                                        val eB: Elem[B], val eC: Elem[C], val eR: Elem[R])
    extends ArrayDef[(K, R)]

  def array_update[T](xs: Arr[T], index: Rep[Int], value: Rep[T]): Arr[T] = {
    implicit val eT = xs.elem.eItem
    ArrayUpdate(xs, index, value)
  }

  def array_updateMany[T](xs: Arr[T], indexes: Arr[Int], values: Arr[T]): Arr[T] = {
    implicit val eT = xs.elem.eItem
    ArrayUpdateMany(xs, indexes, values)
  }

  def array_append[T](xs: Arr[T], value: Rep[T]): Arr[T] = {
    implicit val eT = xs.elem.eItem
    ArrayAppend(xs, value)
  }

  def array_sum[T:Elem](xs: Arr[T])(implicit n: Numeric[T]): Rep[T] = ArraySum(xs, n)
  def array_max[T:Elem](xs: Arr[T])(implicit o: Ordering[T]): Rep[T] = ArrayMax(xs, o)
  def array_min[T:Elem](xs: Arr[T])(implicit o: Ordering[T]): Rep[T]  = ArrayMin(xs, o)
  def array_avg[T:Elem](xs: Arr[T])(implicit n: Numeric[T]): Rep[Double] = ArrayAvg(xs, n)
  def array_sort_by[T:Elem, O:Elem](xs: Arr[T], by: Rep[T => O])(implicit o:Ordering[O]): Arr[T] = ArraySortBy(xs, by, o)
  def array_group_by[T:Elem, G:Elem](xs: Arr[T], by: Rep[T => G]) = ArrayGroupBy(xs, by)
  def array_count[T:Elem](xs: Arr[T], f: Rep[T => Boolean]): Rep[Int] = ArrayCount(xs, f)
  def array_sum_by[T:Elem,S:Elem](xs: Arr[T], f: Rep[T=>S])(implicit n: Numeric[S]): Rep[S] = ArraySumBy(xs, f, n)

  def array_apply[T](xs: Exp[Array[T]], n: Exp[Int]): Rep[T] =
    withElemOfArray(xs) { implicit eT => ArrayApply(xs, n) }
  def array_applyMany[T](xs: Exp[Array[T]], is: Exp[Array[Int]]): Arr[T] =
    withElemOfArray(xs) { implicit eT => ArrayApplyMany(xs, is) }
  def array_length[T](a: Exp[Array[T]]): Rep[Int] = ArrayLength(a)
  def array_map[T, R: Elem](xs: Exp[Array[T]], f: Exp[T => R]) = ArrayMap(xs, f)
  def array_flat_map[T, R: Elem](xs: Exp[Array[T]], f: Exp[T => Array[R]]) = ArrayFlatMap(xs, f)

  def array_reduce[T](xs: Arr[T])(implicit m: RepMonoid[T]) =
    withElemOfArray(xs) { implicit eT => ArrayReduce(xs, m) }
  def array_fold[T,S:Elem](xs: Arr[T], init:Rep[S], f:Rep[((S,T))=>S]): Rep[S] =
    withElemOfArray(xs) { implicit eT => ArrayFold(xs, init, f) }

  def array_map_reduce[T,K:Elem,V:Elem](xs: Exp[Array[T]], map:Exp[T=>(K,V)], reduce:Exp[((V,V))=>V]) =
    ArrayMapReduce(xs, map, reduce)

  def array_scan[T](xs: Arr[T])(implicit m: RepMonoid[T], elem : Elem[T]): Rep[(Array[T], T)] =
    ArrayScan(xs, m)

  def array_zip[T, U](xs: Arr[T], ys: Arr[U]): Arr[(T, U)] = {
    implicit val eT = xs.elem.eItem
    implicit val eU = ys.elem.eItem
    ArrayZip(xs, ys)
  }

  def array_sort[T](xs: Arr[T])(implicit o:Ordering[T]): Arr[T] = {
    implicit val eT = xs.elem.eItem
    ArraySort(xs, o)
  }

  def array_replicate[T: Elem](len: Rep[Int], v: Rep[T]): Arr[T] =
    ArrayReplicate(len, v)

  def array_slice[T](xs: Arr[T], start: Rep[Int], length: Rep[Int]): Arr[T] =
    array_stride(xs, start, length, 1)

  def array_rangeFrom0(n: Rep[Int]): Arr[Int] =
    ArrayRangeFrom0(n)

  def array_filter[T](xs: Arr[T], f: Rep[T => Boolean]): Arr[T] =
    withElemOfArray(xs) { implicit eT => ArrayFilter(xs, f) }

  def array_find[T](xs: Arr[T], f: Rep[T => Boolean]): Arr[Int] =
    ArrayFind(xs, f)

  def array_grouped[T](xs: Arr[T], size: Rep[Int]): Arr[Array[T]] = {
    implicit val eT = xs.elem.eItem
    SArray.tabulate(xs.length div size) { i => xs.slice(i * size, size) }
  }

  def array_stride[T](xs: Arr[T], start: Rep[Int], length: Rep[Int], stride: Rep[Int]): Arr[T] = {
    implicit val eT = xs.elem.eItem
    ArrayStride(xs, start, length, stride)
  }

  def array_toList[T:Elem](xs: Arr[T]): Lst[T] =
    ArrayToList(xs)

  def array_empty[T: Elem]: Arr[T] = ArrayEmpty[T]()

  def array_reverse[T:Elem](xs: Arr[T]): Arr[T] = ArrayReverse(xs)

  def array_flatten[T:Elem](xs: Arr[Array[T]]): Arr[T] = ArrayFlatten(xs)

  def array_cons[T:Elem](value: Rep[T], xs: Arr[T]): Arr[T] = ArrayCons(value, xs)

//  def accessOnlyFirst(x: Exp[_], exp: Exp[_]): Boolean = {
//    exp match {
//      case Def(Tup(l, r)) => accessOnlyFirst(x, l) && accessOnlyFirst(x, r)
//      case Def(Second(t)) => accessOnlyFirst(x, t)
//      case Def(First(t)) => (t == x || accessOnlyFirst(x, t))
//      case Def(ApplyBinOp(op, l, r)) => accessOnlyFirst(x, l) && accessOnlyFirst(x, r)
//      case Def(ApplyUnOp(opr, opd)) => accessOnlyFirst(x, opd)
//      case Def(Const(_)) => true
//      case _ => false
//    }
//  }
//
//  def accessOnlySecond(x: Exp[_], exp: Exp[_]): Boolean = {
//    exp match {
//      case Def(Tup(l, r)) => accessOnlySecond(x, l) && accessOnlySecond(x, r)
//      case Def(First(t)) => accessOnlySecond(x, t)
//      case Def(Second(t)) => (t == x || accessOnlySecond(x, t))
//      case Def(ApplyBinOp(op, l, r)) => accessOnlySecond(x, l) && accessOnlySecond(x, r)
//      case Def(ApplyUnOp(opr, opd)) => accessOnlySecond(x, opd)
//      case Def(Const(_)) => true
//      case _ => false
//    }
//  }
//
//   def firstOnlyExp(oldX: Exp[_], newX: Exp[_], exp: Exp[_]): Exp[_] = {
//    exp match {
//      case Def(t: Tup[a, b]) => {
//        implicit val eA = t.a.elem
//        implicit val eB = t.b.elem
//        Tup[a,b](firstOnlyExp(oldX, newX, t.a).asRep[a], firstOnlyExp(oldX, newX, t.b).asRep[b])
//      }
//      case Def(s: Second[a,b]) => {
//        val pair = s.pair
//        implicit val eA = pair.elem.eFst
//        implicit val eB = pair.elem.eSnd
//        Second[a,b](firstOnlyExp(oldX, newX, pair).asRep[(a,b)])
//      }
//      case Def(f: First[a, b]) => {
//        val pair = f.pair
//        if (pair == oldX) {
//          newX
//        } else {
//          implicit val eA = pair.elem.eFst
//          implicit val eB = pair.elem.eSnd
//          First[a, b](firstOnlyExp(oldX, newX, pair).asRep[(a, b)])
//        }
//      }
//      case Def(bin: ApplyBinOp[a,r]) =>
//        ApplyBinOp[a,r](bin.op, firstOnlyExp(oldX, newX, bin.lhs).asRep[a], firstOnlyExp(oldX, newX, bin.rhs).asRep[a])
//      case Def(un: ApplyUnOp[a, r]) =>
//         ApplyUnOp[a,r](un.op, firstOnlyExp(oldX, newX, un.arg).asRep[a])
//      case _ => exp
//    }
//  }
//  def firstOnly[A:Elem,B:Elem](l: Lambda[_, _]): Exp[A => B] = {
//    val newSym = fresh[A => B]
//    val newX = fresh[A]
//    val first = firstOnlyExp(l.x, newX, l.y)
//    val newLam = new Lambda[A, B](None, newX, first.asRep[B], newSym, l.mayInline)
//    toExp(newLam, newSym)
//  }
//
//  def secondOnlyExp(oldX: Exp[_], newX: Exp[_], exp: Exp[_]): Exp[_] = {
//    exp match {
//      case Def(t: Tup[a, b]) => {
//        implicit val eA = t.a.elem
//        implicit val eB = t.b.elem
//        Tup[a, b](secondOnlyExp(oldX, newX, t.a).asRep[a], secondOnlyExp(oldX, newX, t.b).asRep[b])
//      }
//      case Def(f: First[a, b]) => {
//        val pair = f.pair
//        implicit val eA = pair.elem.eFst
//        implicit val eB = pair.elem.eSnd
//        First[a, b](secondOnlyExp(oldX, newX, pair).asRep[(a, b)])
//      }
//      case Def(s: Second[a, b]) => {
//        val pair = s.pair
//        if (pair == oldX) {
//          newX
//        } else {
//          implicit val eA = pair.elem.eFst
//          implicit val eB = pair.elem.eSnd
//          Second[a, b](secondOnlyExp(oldX, newX, pair).asRep[(a, b)])
//        }
//      }
//      case Def(bin: ApplyBinOp[a,r]) =>
//        ApplyBinOp[a,r](bin.op, secondOnlyExp(oldX, newX, bin.lhs).asRep[a], secondOnlyExp(oldX, newX, bin.rhs).asRep[a])
//      case Def(un: ApplyUnOp[a, r]) =>
//         ApplyUnOp[a,r](un.op, secondOnlyExp(oldX, newX, un.arg).asRep[a])
//      case _ => exp
//    }
//  }

//  def secondOnly[A:Elem,B:Elem](l: Lambda[_, _]): Exp[A => B] = {
//    val newSym = fresh[A => B]
//    val newX = fresh[A]
//    val second = secondOnlyExp(l.x, newX, l.y)
//    val newLam = new Lambda[A, B](None, newX, second.asRep[B], newSym, l.mayInline)
//    toExp(newLam, newSym)
//  }

  def array_binary_search[T:Elem](i: Rep[T], is: Arr[T])(implicit o: Ordering[T]): Rep[Int] = {
    ArrayBinarySearch(i, is, o)
  }

  def array_randomGaussian(m: Rep[Double], e: Rep[Double], arr: Arr[Double]): Arr[Double] = {
    ArrayRandomGaussian(m, e, arr)
  }

  def array_fromSyms[T:Elem](syms: Seq[Rep[T]]): Arr[T] = {
    SymsArray(syms)
  }

  object ArgAccess extends Enumeration {
    type ArgAccess = Value
    val Non, First, Second, Both = Value

    implicit class ArgAccessOps(acc: ArgAccess) {
      def &&(other: ArgAccess): ArgAccess = acc match {
        case Non => other match {
          case Non => Non
          case o => o
        }
        case First => other match {
          case _ if other <= First => First
          case _ => Both
        }
        case Second => other match {
          case Non => Second
          case First => Both
          case Second => Second
          case Both => Both
        }
        case _ => Both
      }
    }
  }
  import ArgAccess._

  def getArgAccess[A,B](l: Lambda[(A,B),_]): ArgAccess = {
    val sch = l.scheduleAll
    val first  = l.x._1
    val second = l.x._2
    val accFirst  = if (sch.exists(te => te.sym == first)) ArgAccess.First else Non
    val accSecond = if (sch.exists(te => te.sym == second)) ArgAccess.Second else Non
    accFirst && accSecond
  }

  object LambdaWithAccess {
    def unapply[T](s: Exp[T]): Option[(Def[T], ArgAccess)] = s match {
      case Def(l: Lambda[(a,b),_]@unchecked) => Some((l, getArgAccess(l)))
      case _ => None
    }
  }

  def sliceFirst[A,B,C](f: Exp[((A,B)) => C]): Exp[A => C] = {
    val elem = f.elem.eDom
    implicit val eA = elem.eFst
    implicit val eB = elem.eSnd
    implicit val eC = f.elem.eRange
    fun { x: Exp[A] => f(Pair(x, fresh[B])) }
  }

  def sliceSecond[A,B,C](f: Exp[((A,B)) => C]): Exp[B => C] = {
    val elem = f.elem.eDom
    implicit val eA = elem.eFst
    implicit val eB = elem.eSnd
    implicit val eC = f.elem.eRange
    fun { x: Exp[B] => f(Pair(fresh[A], x)) }
  }

  case class ArrayStruct[Val, ValSchema]
      (val arrays: Rep[ValSchema])
      (implicit val eVal: Elem[Val], val eValSchema: Elem[ValSchema])
    extends ArrayDef[Val]

  def arrayStruct[Val <: Struct, ValSchema <: Struct]
      (arrays: Rep[ValSchema])
      (implicit eVal: Elem[Val], eValSchema: Elem[ValSchema]): Arr[Val] =
    reifyObject(ArrayStruct(arrays)(eVal, eValSchema))

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
          ArrayZip(RepArrayOps(xs1)(e1)(is), RepArrayOps(ys1)(e2)(is))(e1, e2)
        case ArrayReplicate(_, x) =>
          implicit val eT = x.elem
          SArray.replicate(is.length, x)
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
        case ArrayFilter(xs, f) =>
          implicit val eT = xs.elem.eItem
          ArrayCount(xs, f)
        case ArraySort(xs, o) =>
          implicit val eT = xs.elem.eItem
          xs.length
        case ArrayReplicate(length, _) => length
        case ArrayStride(_, _, length, _) => length
        case ArrayRangeFrom0(n) => n
        case ArrayEmpty()  => toRep(0)
        case _ =>
          super.rewriteDef(d)
      }

    case ArrayFilter(Def(zip: ArrayZip[x, y]), p_ @ LambdaWithAccess(_, acc)) if acc == ArgAccess.First =>
      val pred = p_.asRep[((x,y)) => Boolean]
      val xs1 = zip.xs
      val ys1 = zip.ys
      implicit val eX = xs1.elem.eItem
      implicit val eY = ys1.elem.eItem
      val positions = ArrayFind(xs1, sliceFirst(pred))
      val res = ArrayZip(ArrayApplyMany(xs1, positions), ArrayApplyMany(ys1, positions))
      res

    case ArrayFilter(Def(zip: ArrayZip[x, y]), p_ @ LambdaWithAccess(_, acc)) if acc == ArgAccess.Second =>
      val pred = p_.asRep[((x,y)) => Boolean]
      val xs1 = zip.xs
      val ys1 = zip.ys
      implicit val eX = xs1.elem.eItem
      implicit val eY = ys1.elem.eItem
      val positions = ArrayFind(ys1, sliceSecond(pred))
      val res = ArrayZip(ArrayApplyMany(xs1, positions), ArrayApplyMany(ys1, positions))
      res

    //    case ArrayFind(Def(zip: ArrayZip[x, y]), p_ @ LambdaWithAccess(_, acc)) if acc == ArgAccess.First =>
    //      val pred = p_.asRep[((x,y)) => Boolean]
    //      val xs1 = zip.xs
    //      ArrayFind(xs1, sliceFirst(pred))
    //
    //    case ArrayFind(Def(zip: ArrayZip[x, y]), p_ @ LambdaWithAccess(_, acc)) if acc == ArgAccess.Second =>
    //      val pred = p_.asRep[((x,y)) => Boolean]
    //      val ys1 = zip.ys
    //      ArrayFind(ys1, sliceSecond(pred))

    case ArrayCount(Def(zip: ArrayZip[x, y]), p_ @ LambdaWithAccess(_, acc)) if acc == ArgAccess.First =>
      val pred = p_.asRep[((x,y)) => Boolean]
      val xs1 = zip.xs
      implicit val eX = xs1.elem.eItem
      ArrayCount(xs1, sliceFirst(pred))
    case ArrayCount(Def(zip: ArrayZip[x, y]), p_ @ LambdaWithAccess(_, acc)) if acc == ArgAccess.Second =>
      val pred = p_.asRep[((x,y)) => Boolean]
      val ys1 = zip.ys
      implicit val eY = ys1.elem.eItem
      ArrayCount(ys1, sliceSecond(pred))
    case ArrayMap(Def(zip: ArrayZip[x, y]), f_ @ LambdaWithAccess(l: Lambda[_,b], acc)) if acc == ArgAccess.First =>
      val f = f_.asRep[((x,y)) => b]
      val xs1 = zip.xs
      implicit val eX = xs1.elem.eItem
      implicit val eB = l.eB
      val res = ArrayMap(xs1, sliceFirst(f))
      res
    case ArrayMap(Def(zip: ArrayZip[x, y]), f_ @ LambdaWithAccess(l: Lambda[_,b], acc)) if acc == ArgAccess.Second =>
      val f = f_.asRep[((x,y)) => b]
      val ys1 = zip.ys
      implicit val eY = ys1.elem.eItem
      implicit val eB = l.eB
      val res = ArrayMap(ys1, sliceSecond(f))
      res

    case ArrayMap(xs, Def(IdentityLambda())) => xs
    //    case ArrayMap(xs: Rep[Array[a]], Def(ConstantLambda(v))) =>
    //      val xs1 = xs.asRep[Array[a]]
    //      implicit val eA = xs1.elem.eItem
    //      SArray.replicate(xs1.length, v)(v.elem)

    // should be the last rule in this rewriteDef (with ArrayMap on top)
    case ArrayMap(Def(d2), f: Rep[Function1[a, b]]@unchecked) =>
      d2.asDef[Array[a]] match {
//        case ArrayMap(xs: Rep[Array[c]]@unchecked, g) =>
//          val xs1 = xs.asRep[Array[c]]
//          val g1 = g.asRep[c => a]
//          implicit val eB = f.elem.eRange
//          implicit val eC = xs.elem.eItem
//          val res = xs1.map { x => f(g1(x))}
//          res
        case ArrayReplicate(length, x) =>
          implicit val eB = f.elem.eRange
          SArray.replicate(length, f(x))
        case _ =>
          super.rewriteDef(d)
      }
    case ArraySumBy(Def(zip: ArrayZip[x, y]), f_ @ LambdaWithAccess(l: Lambda[a,b], acc), n) if acc == ArgAccess.Second  =>
      val f = f_.asRep[((x,y)) => b]
      val xs1 = zip.xs
      implicit val eX = xs1.elem.eItem
      implicit val eB = l.eB
      ArraySumBy(xs1, sliceFirst(f), n.asInstanceOf[Numeric[b]])
    case ArraySumBy(Def(zip: ArrayZip[x, y]), f_ @ LambdaWithAccess(l: Lambda[a,b], acc), n) if acc == ArgAccess.Second =>
      val f = f_.asRep[((x,y)) => b]
      val ys1 = zip.ys
      implicit val eY = ys1.elem.eItem
      implicit val eB = l.eB
      ArraySumBy(ys1, sliceSecond(f), n.asInstanceOf[Numeric[b]])

    case ArraySumBy(Def(ArrayApplyMany(xs, Def(find: ArrayFind[c]))), by: Rep[Function1[a, b]] @unchecked, n) =>
      val a1 = xs.asRep[Array[a]]
      val a2 = find.xs
      implicit val eA = a1.elem.eItem
      implicit val eC = a2.elem.eItem
      implicit val eB = by.elem.eRange
      implicit val num = n.asInstanceOf[Numeric[b]]
      val len = a2.length
      loopUntil2(0, num.zero)({ (i, sum) => i === len }, { (i, sum) => (i + 1, IF (find.f(a2(i))) THEN sum + by(a1(i)) ELSE sum) })._2
    case ArraySumBy(Def(many: ArrayApplyMany[_]), by:Rep[Function1[a, b]] @unchecked, n) =>
      val xs = many.xs.asRep[Array[a]]
      val indices = many.indices
      implicit val eA = xs.elem.eItem
      implicit val eB = by.elem.eRange
      implicit val num = n.asInstanceOf[Numeric[b]]
      indices.fold[b](num.zero, fun { p => p._1 + by(xs(p._2))})
    case ArraySumBy(Def(filter: ArrayFilter[_]), by:Rep[Function1[a, b]] @unchecked, n) =>
      val xs = filter.xs.asRep[Array[a]]
      implicit val eA = xs.elem.eItem
      implicit val eB = by.elem.eRange
      implicit val num = n.asInstanceOf[Numeric[b]]
      xs.fold[b](num.zero,
        fun { p => {
          val sum = p._1
          val x = p._2
          IF (filter.f(x)) THEN sum + by(x) ELSE sum
        }})
    case ArrayFilter(Def(d2: Def[Array[a]]@unchecked), f) =>
      d2.asDef[Array[a]] match {
        case ArrayFilter(xs, g) =>
          implicit val eT = xs.elem.eItem
          xs.filter { x => f(x) && g(x)}
        case _ =>
          super.rewriteDef(d)
      }
    case ArrayZip(l, Def(r: ArrayZip[_, _])) =>
      implicit val e1 = l.elem.eItem
      implicit val e2 = r.selfType.eItem.eFst
      implicit val e3 = r.selfType.eItem.eSnd
      val res = SArray.tabulate(l.length)(i => Pair(l(i), Pair(r.xs(i), r.ys(i))))
      res
    case ArrayZip(Def(ArrayReplicate(len, v1: Rep[a])), Def(ArrayReplicate(_, v2: Rep[b]))) =>
      implicit val eA = v1.elem
      implicit val eB = v2.elem
      SArray.replicate(len, (v1, v2))

    // Rule: fuse ArrayZip into SArray.tabulate
    // should be the last with ArrayZip on top
    case ArrayZip(l, Def(map: ArrayMap[x, y]@unchecked)) =>
      map.xs match {
        case Def(range: ArrayRangeFrom0) =>
          implicit val eL = l.elem.eItem
          val f = map.f.asInstanceOf[Rep[Int =>y]]
          val res = SArray.tabulate(range.n)(i => Pair(l(i), f(i)))
          res
        case _ =>
          super.rewriteDef(d)
      }

    // Rule: fuse ArrayMap into ArrayMapReduce
    case mr@ArrayMapReduce(Def(map1: ArrayMap[x, y]@unchecked), map2: Rep[Function1[_, (k, v)]], reduce) =>
      val xs = map1.xs.asRep[Array[x]]
      implicit val eX = xs.elem.eItem
      implicit val eY = map1.elem.eItem
      implicit val eK = mr.elemKey.asElem[k]
      implicit val eV = mr.elemValue.asElem[v]
      val res = xs.mapReduceBy[k, v](fun { e => map2.asRep[y => (k, v)](map1.f(e)) }, reduce.asRep[((v, v)) => v])
      res

    case _ => super.rewriteDef(d)
  }
}
