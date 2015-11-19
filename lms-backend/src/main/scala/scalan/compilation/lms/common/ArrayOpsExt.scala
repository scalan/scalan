package scalan.compilation.lms.common

import java.util.HashMap

import scala.reflect.SourceContext
import scala.lms.common._
import scala.lms.internal.Transforming
import scalan.compilation.lms.LmsBackendFacade
import scalan.compilation.lms.cxx.sharedptr.CxxShptrCodegen

trait ArrayOpsExt extends Base {
  def array_insert[T: Manifest](x: Rep[Array[T]], n: Rep[Int], y: Rep[T])(implicit pos: SourceContext): Rep[Array[T]]
  def array_reverse[T:Manifest](x: Rep[Array[T]])(implicit pos: SourceContext): Rep[Array[T]]
  def infix_insert[T: Manifest](x: Rep[Array[T]], n: Rep[Int], y: Rep[T])(implicit pos: SourceContext): Rep[Array[T]] =
    array_insert(x, n, y)
  def infix_reverse[T:Manifest](x: Rep[Array[T]])(implicit pos: SourceContext): Rep[Array[T]] =
    array_reverse(x)
}

// Naming convention is e.g. `arrayNew` instead of `array_new` to
// avoid conflict with ArrayOps; these methods are based on
// ArrayLoops instead
trait ArrayOpsExtExp extends ArrayOpsExt with Transforming with EffectExp { self: LmsBackendFacade =>

  case class ArrayInsert[T](a: Exp[Array[T]], n: Exp[Int], y: Exp[T])(implicit val m: Manifest[T]) extends Def[Array[T]]
  case class ArrayReverse[T](x: Exp[Array[T]])(implicit val m: Manifest[T]) extends Def[Array[T]]

  def array_insert[T:Manifest](a: Exp[Array[T]], i: Exp[Int], x: Exp[T])(implicit pos: SourceContext) = ArrayInsert(a,i,x)
  def array_reverse[T:Manifest](x: Exp[Array[T]])(implicit pos: SourceContext) = ArrayReverse(x)

  def arrayEmpty[A: Manifest] = array_obj_fromseq[A](Seq.empty)

  def map_fromArray[K: Manifest, V: Manifest](arr: Exp[Array[(K, V)]]): Exp[HashMap[K, V]] = {
    val h = HashMap[K, V]()
    for (pair <- arr) {
      h.update(pair._1, pair._2)
    }
    h
  }

  def arrayApply[A: Manifest](a: Exp[Array[A]], i: Exp[Int]): Exp[A] = {
    a.at(i)
  }

  def arrayApplyMany[A: Manifest](a: Exp[Array[A]], idxs: Exp[Array[Int]]): Exp[Array[A]] = {
    array(idxs.length)(i => a.at(idxs.at(i)))
  }

  def arrayLength[A: Manifest](a: Exp[Array[A]]): Exp[Int] = {
    a.length
  }

  def arrayMap[A: Manifest, B: Manifest](a: Exp[Array[A]], f: Rep[A] => Rep[B]): Exp[Array[B]] = {
    //    a.map(f)
    array(a.length)(i => f(a.at(i)))
  }

//  def flatMapArray[A: Manifest, B: Manifest](arr: Exp[Array[A]], f: Rep[A] => Rep[Array[B]]): Exp[Array[B]] = {
//    flatten(arr.length)(i => f(arr.at(i)))
//  }
  def arrayFlatMap[A: Manifest, B: Manifest](arr: Exp[Array[A]], f: Rep[A] => Rep[Array[B]]): Exp[Array[B]] = {
    val buf = ArrayBuilder.make[B]
    for (x <- arr; y <- f(x)) {
      buf += y
    }
    buf.result
  }

  def arrayFind[A: Manifest](a: Exp[Array[A]], f: Rep[A] => Rep[Boolean]): Exp[Array[Int]] = {
    arrayIf(a.length) { i => (f(a.at(i)), i)}
  }

  def arrayFilter[A: Manifest](a: Exp[Array[A]], f: Rep[A] => Rep[Boolean]): Exp[Array[A]] = {
    arrayIf(a.length) { i => { val item = a.at(i); (f(item), item) }}
  }

  def arrayCount[A: Manifest](a: Exp[Array[A]], f: Rep[A] => Rep[Boolean]): Exp[Int] = {
    sumIfInt(a.length)(i => (f(a.at(i)), 1))
  }
//  def countArray[A: Manifest](a: Exp[Array[A]], f: Rep[A] => Rep[Boolean]): Exp[Int] = {
//    var count = 0
//    for (x <- a) {
//      if (f(x)) count += 1
//    }
//    count
//  }

  def arrayReplicate[A: Manifest](length: Exp[Int], v: Exp[A]): Exp[Array[A]] = {
    array(length)(i => v)
  }

  def arrayRangeFrom0(length: Exp[Int]): Exp[Array[Int]] = {
    array(length)(i => i)
  }

  def newArray[A: Manifest](length: Rep[Int]): Rep[Array[A]] = NewArray[A](length)

  def arrayZipWith[A: Manifest, B: Manifest, R: Manifest](f: (Rep[A], Rep[B]) => Rep[R], a: Exp[Array[A]], b: Exp[Array[B]]): Exp[Array[R]] = {
    array(a.length)(i => f(a.at(i), b.at(i)))
  }

  def arrayZip[A: Manifest, B: Manifest](a: Exp[Array[A]], b: Exp[Array[B]]): Exp[Array[(A, B)]] = {
    array[(A, B)](a.length)(i => (a.at(i), b.at(i)))
  }

  def arraySort[A: Manifest](a: Exp[Array[A]]): Exp[Array[A]] = {
    a.sort
  }

  def arrayReverse[A: Manifest](a: Exp[Array[A]]): Exp[Array[A]] = {
    a.reverse
  }

  def arrayStride[A: Manifest](xs: Exp[Array[A]], start: Exp[Int], length: Exp[Int], stride: Exp[Int]) =
    array(length) { i =>
      xs.at(start + i * stride)
    }

  def arrayUpdate[A: Manifest](xs: Exp[Array[A]], index: Exp[Int], value: Exp[A]) = {
    val newArr =  xs.mutable
    newArr.update(index, value)
    newArr
  }

  def arrayUpdateMany[A: Manifest](xs: Exp[Array[A]], indexes: Exp[Array[Int]], values: Exp[Array[A]]) = {
    val newArr =  xs.mutable
    for(i <- 0 until indexes.length) {
      newArr.update(indexes(i), values(i))
    }
    newArr
  }
//  def updateArray[A: Manifest](xs: Exp[Array[A]], index: Exp[Int], value: Exp[A]) = {
//    val newArr =  array_obj_new(xs.length)
//    array_copy(xs, 0, newArr, 0, xs.length)
//    newArr.update(index, value)
//    newArr
//
////    //inplace update of immutable array...
////    xs.update(index, value)
////    xs
//  }

  def arraySum[A](xs: Exp[Array[A]])(implicit n: Numeric[A], m: Manifest[A]): Exp[A] = {
    var sum = n.zero
    for (x <- xs) sum += x
    sum
  }

  def arrayMax[A](xs: Exp[Array[A]])(implicit o: Ordering[A], m: Manifest[A]): Exp[A] = {
    var max = xs.at(0) // we need Optional type to correctly implement min/max, but it is abselnt in CE
    for (x <- xs) if (x > max) max = x
    max
  }

  def arrayMin[A](xs: Exp[Array[A]])(implicit o: Ordering[A], m: Manifest[A]): Exp[A] = {
    var min = xs.at(0) // we need Optional type to correctly implement min/max, but it is abselnt in CE
    for (x <- xs) if (x < min) min = x
    min
  }

  def arrayAvg[A](xs: Exp[Array[A]])(implicit n: Numeric[A], m: Manifest[A]): Exp[Double] = {
    var sum = n.zero
    for (x <- xs) sum += x
    sum.AsInstanceOf[Double] / xs.length
  }


  def sumArray[A: Manifest](a: Exp[Array[A]]): Exp[A] = {
    sum(a.length) { i => a.at(i).AsInstanceOf[Double]}.AsInstanceOf[A]
  }

  def reduceArray[A: Manifest](a: Exp[Array[A]], zero: Exp[A], accumulate: Rep[(A, A)] => Rep[A]): Exp[A] = {
    var state = zero
    for (x <- a) {
      state = accumulate((state.AsInstanceOf[A], x))
    }
    state
  }

  /* This is not always woking */
  def scanArray[A: Manifest](a: Exp[Array[A]], zero: Exp[A], accumulate: Rep[(A, A)] => Rep[A]): Exp[(Array[A], A)] = {
    var sum = zero
    val len = a.length
    val res = newArray(len).mutable
    for (i <- 0 until len) {
      res.update(i, sum)
      sum += a.at(i)
    }
    (res, sum: Rep[A])
  }
//  def scanArray[A: Manifest](a: Exp[Array[A]], zero: Exp[A], accumulate: Rep[(A, A)] => Rep[A]): Exp[(Array[A], A)] = {
//    var state = zero
//    val arr1 = array(a.length)(i => {
//      val res = state
//      val loc = if (i==0) zero else a.at(i-1)
//      state = accumulate((state.AsInstanceOf[A], loc))
//      res
//    })
//    Tuple2(arr1, accumulate((arr1.at(a.length - 1), a.at(a.length - 1))))
//  }

  def arrayFold[A: Manifest, S: Manifest](a: Exp[Array[A]], init: Exp[S], func: Rep[(S, A)] => Rep[S]): Exp[S] = {
    var state = init
    for (x <- a) {
      state = func((state.AsInstanceOf[S], x))
    }
    state
  }

  def arraySumBy[A: Manifest, S: Manifest](a: Exp[Array[A]], func: Rep[A] => Rep[S])(implicit n: Numeric[S]): Exp[S] = {
    var sum = n.zero
    for (x <- a) {
      sum += func(x)
    }
    sum
  }

  def arrayAppend[A: Manifest](xs: Rep[Array[A]], value: Rep[A]): Rep[Array[A]] = {
    ArrayAppend(xs, value)
  }

  case class ArrayAppend[A](xs: Rep[Array[A]], value: Rep[A])(implicit val m: Manifest[A]) extends Def[Array[A]]

  def arraySortBy[A: Manifest, B: Manifest](a: Exp[Array[A]], by: Rep[A] => Rep[B]): Exp[Array[A]] = ArraySortBy(a, by)
  private[this] def arraySortBy1[A: Manifest, B: Manifest](a: Exp[Array[A]], by: Rep[A => B]): Exp[Array[A]] = ArraySortBy(a, by)

  case class ArraySortBy[A: Manifest, B: Manifest](a: Exp[Array[A]], by: Exp[A => B]) extends Def[Array[A]] {
    val mA = manifest[A]
    val mB = manifest[B]
  }

  def arrayCons[A: Manifest](value: Rep[A], xs: Rep[Array[A]]): Rep[Array[A]] = {
    xs.insert(0, value)
  }

  def arrayToList[A: Manifest](xs: Rep[Array[A]]): Rep[List[A]] =
    list_fromseq(array_toseq(xs))

  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = {
    (e match {
      case ArrayToSeq(arr) =>
        ArrayToSeq(f(arr))
      case ArrayIndex(arr, i) =>
        ArrayIndex(f(arr), f(i))
      case a @ ArrayAppend(arr, v) =>
        ArrayAppend(f(arr), f(v))(a.m)
      case a @ ArraySortBy(arr, by) => ArraySortBy(f(arr), f(by))(a.mA, a.mB)
      case e @ ArrayReverse(x) => ArrayReverse(f(x))(e.m)
      case ArrayInsert(l,i,r) => ArrayInsert(f(l),f(i),f(r))(mtype(manifest[A]))
      case _ =>
        super.mirrorDef(e,f)
    }).asInstanceOf[Def[A]]
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] =
    (e match {
      case Reflect(ArrayForeach(a,x,b), u, es) => reflectMirrored(Reflect(ArrayForeach(f(a),f(x).asInstanceOf[Sym[A]],f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(a @ ArraySortBy(arr, by), u, es) => reflectMirrored(Reflect(ArraySortBy(f(arr), f(by))(a.mA, a.mB), mapOver(f, u), f(es)))(mtype(manifest[A]), pos)
      case _ =>
        super.mirror(e,f)
    }).asInstanceOf[Exp[A]]

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case ArrayUpdate(a,i,x) => Nil // syms(a) <-- any use to return a?
    case ArrayInsert(a,i,x) => Nil // syms(a) <-- any use to return a?
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case ArrayUpdate(a,i,x) => syms(x)
    case ArrayInsert(a,i,x) => syms(a) ::: syms(x)
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case ArrayUpdate(a,i,x) => Nil
    case ArrayInsert(a,i,x) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case ArrayUpdate(a,i,x) => Nil //syms(a)
    case ArrayInsert(a,i,x) => Nil //syms(a)
    case _ => super.copySyms(e)
  }
}

trait ScalaGenArrayOpsExt extends ScalaGenBase {
  val IR: ArrayOpsExtExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a @ ArrayAppend(xs, v) =>
      gen"""val $sym = {
           |  val len = $xs.length
           |  val d = new Array[${remap(a.m)}](len + 1)
           |  System.arraycopy($xs, 0, d, 0, len)
           |  d(len) = $v
           |  d
           |}"""
    case a@ArraySortBy(arr, by) =>
      stream.println("val " + quote(sym) + " = " + quote(arr) + ".sortBy(" + quote(by)+ ")")
    case a @ ArrayInsert(xs,i,y) =>
      gen"""val $sym = {
            |  val len = $xs.length
            |  val d = new Array[${remap(a.m)}](len + 1)
            |  System.arraycopy($xs, 0, d, 0, $i)
            |  d($i) = $y
            |  System.arraycopy($xs, $i, d, $i + 1, len - $i)
            |  d
            |}"""
    case ArrayReverse(x) =>
      emitValDef(sym, src"$x.reverse")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CxxShptrGenArrayOpsExt extends CxxShptrCodegen {
  val IR: ArrayOpsExtExp with ArrayOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a @ ArrayAppend(xs, v) =>
/////////////////////////////////////////////////////
// Creates new array, copies values to it from xs and adds new element
      emitNode(sym, ArrayNew(Const(0))(a.m))
      val xsLen = src"${xs}_len"
      gen"""size_t $xsLen = $xs->size();
           |$sym->resize($xsLen + 1);
           |std::copy($xs->begin(), $xs->end(), $sym->begin());
           |(*$sym)[$xsLen] = $v;"""
////////////////////////////////////////////////////
// Modifies xs adding new element and assigns new symbol to xs
//      gen"""$xs->push_back($v);"""
//      emitValDef(sym, src"$xs")
    case a @ ArrayInsert(xs,i,y) =>
      emitNode(sym, ArrayNew(Const(0))(a.m))
      val xsLen = src"${xs}_len"
      gen"""size_t $xsLen = $xs->size();
           |$sym->resize($xsLen + 1);
           |std::copy($xs->begin(), $xs->begin() + $i, $sym->begin());
           |(*$sym)[$i] = $y;
           |std::copy($xs->begin() + $i, $xs->end(), $sym->begin() + $i + 1);"""
    // TODO implement for ArraySortBy
    case _ =>
      super.emitNode(sym, rhs)
  }
}
