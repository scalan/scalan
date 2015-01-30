package scalan.compilation.lms

import scala.virtualization.lms.internal.GenericCodegen
import scalan.compilation.lms.common.{VectorOpsExp, ScalaGenVectorOps, ScalaGenEitherOps, EitherOpsExp, SystemOpsExp, ScalaGenSystemOps}
import virtualization.lms.common._
import virtualization.lms.epfl.test7._
import java.util.HashMap

trait LmsBackend extends BaseExp { self =>

  type Codegen <: GenericCodegen {
    val IR: self.type
  }

  def codegen: Codegen
}

trait LmsBackendFacade extends LiftVariables with LiftPrimitives with LiftNumeric with ListOpsExp with NumericOpsExp with StringOpsExp with RangeOpsExp with PrimitiveOpsExp
with EqualExp with BooleanOpsExp with TupleOpsExp with ArrayLoopsFatExp with IfThenElseFatExp with CastingOpsExp with FunctionsExp
with HashMapOpsExp with ArrayOpsExp with IterableOpsExp with WhileExp with OrderingOpsExp with ArrayBuilderOpsExp
with ExceptionOpsExp with EitherOpsExp with SystemOpsExp with VectorOpsExp {
  /*type RepD[T] = Rep[T]
  */

  def Not(arg: Exp[Boolean]) = {
    !arg
  }

  def Neg[T: Manifest](arg: Exp[T])(implicit n: Numeric[T]) = {
    n.zero - arg
  }

  def DoubleToInt(arg: Exp[Double]) = {
    arg.toInt
  }

  def DoubleToFloat(arg: Exp[Double]) = {
    arg.toFloat
  }

  def IntToDouble(arg: Exp[Int]) = {
    arg.toDouble
  }

  def IntToFloat(arg: Exp[Int]) = {
    arg.toFloat
  }

  def sumLeft[A: Manifest, B: Manifest](a: Exp[A]): Exp[Either[A, B]] = make_left[A, B](a)
  def sumRight[A: Manifest, B: Manifest](b: Exp[B]): Exp[Either[A, B]] = make_right[A, B](b)


  def FloatToDouble(arg: Exp[Float]) = {
    arg.toDouble
  }

  def FloatToInt(arg: Exp[Float]) = {
    arg.toInt
  }

  def ToString(arg: Exp[_]) = {
    String.valueOf(arg)
  }

  def And(left: Exp[Boolean], right: Exp[Boolean]) = {
    left && right
  }

  def Or(left: Exp[Boolean], right: Exp[Boolean]) = {
    left || right
  }

  def GT[A: Manifest](left: Exp[A], right: Exp[A])(implicit ord: Ordering[A]) = {
    left > right
  }

  def GTEQ[A: Manifest](left: Exp[A], right: Exp[A])(implicit ord: Ordering[A]) = {
    left >= right
  }

  def LT[A: Manifest](left: Exp[A], right: Exp[A])(implicit ord: Ordering[A]) = {
    left < right
  }

  def LTEQ[A: Manifest](left: Exp[A], right: Exp[A])(implicit ord: Ordering[A]) = {
    left <= right
  }

  def block[A: Manifest, B: Manifest](left: Exp[A], right: Exp[B]) = {
    val l = left
    right
  }

  def throwException(msg: Exp[String]) = fatal(msg)

  //  def loopUntil[A:Manifest](state: Exp[A], cond: Rep[A] => Rep[Boolean], step: Rep[A] => Rep[A]): Exp[A] = {
  //    if (cond(state)) loopUntil(step(state), cond, step) else state
  //  }

  def loopUntil[A: Manifest](init: Exp[A], cond: Rep[A] => Rep[Boolean], step: Rep[A] => Rep[A]): Exp[A] = {
    var state = init
    while (!cond(state)) state = step(state)
    state
  }

  //
  // ArrayBuffer
  //
  def arrayBufferUsingFunc[T: Manifest](count: Rep[Int], f: Exp[Int] => Exp[T]): Exp[scala.collection.mutable.ArrayBuilder[T]] = {
    val buf = ArrayBuilder.make[T]
    for (i <- 0 until count) {
      buf += f(i)
    }
    buf
  }

  def emptyArrayBuffer[T: Manifest](): Exp[scala.collection.mutable.ArrayBuilder[T]] = {
    ArrayBuilder.make[T]
  }

  def arrayBufferFromElem[T: Manifest](elem: Exp[T]): Exp[scala.collection.mutable.ArrayBuilder[T]] = {
    val buf = ArrayBuilder.make[T]
    buf += elem
    buf
  }

  def arrayBufferApply[T: Manifest](buf: Exp[scala.collection.mutable.ArrayBuilder[T]], i: Exp[Int]): Exp[T] = ???

  def arrayBufferLength[T: Manifest](buf: Exp[scala.collection.mutable.ArrayBuilder[T]]): Exp[Int] = buf.result.length

  def arrayBufferMap[A: Manifest, B: Manifest](buf: Exp[scala.collection.mutable.ArrayBuilder[A]], f: Rep[A] => Rep[B]): Exp[scala.collection.mutable.ArrayBuilder[B]] = ???

  def arrayBufferUpdate[T: Manifest](buf: Exp[scala.collection.mutable.ArrayBuilder[T]], i: Exp[Int], v: Exp[T]): Exp[scala.collection.mutable.ArrayBuilder[T]] = ???

  def arrayBufferInsert[T: Manifest](buf: Exp[scala.collection.mutable.ArrayBuilder[T]], i: Exp[Int], v: Exp[T]): Exp[scala.collection.mutable.ArrayBuilder[T]] = ???

  def arrayBufferRemove[T: Manifest](buf: Exp[scala.collection.mutable.ArrayBuilder[T]], i: Exp[Int], n: Exp[Int]): Exp[scala.collection.mutable.ArrayBuilder[T]] = ???

  def arrayBufferAppend[T: Manifest](buf: Exp[scala.collection.mutable.ArrayBuilder[T]], v: Exp[T]): Exp[scala.collection.mutable.ArrayBuilder[T]] = {
    buf += v
    buf
  }

  def arrayBufferAppendArray[T: Manifest](buf: Exp[scala.collection.mutable.ArrayBuilder[T]], a: Exp[Array[T]]): Exp[scala.collection.mutable.ArrayBuilder[T]] = {
    for (x <- a) {
      buf += x
    }
    buf
  }

  def arrayBufferReset[T: Manifest](buf: Exp[scala.collection.mutable.ArrayBuilder[T]]): Exp[scala.collection.mutable.ArrayBuilder[T]] = {
    buf.clear()
    buf
  }

  def arrayBufferToArray[T: Manifest](buf: Exp[scala.collection.mutable.ArrayBuilder[T]]): Exp[Array[T]] = {
    buf.result
  }

  //
  // Map
  //
  def emptyMap[K: Manifest, V: Manifest](): Exp[HashMap[K, V]] = {
    HashMap[K, V]()
  }

  def mapFromArray[K: Manifest, V: Manifest](arr: Exp[Array[(K, V)]]): Exp[HashMap[K, V]] = {
    val h = HashMap[K, V]()
    for (pair <- arr) {
      h.update(pair._1, pair._2)
    }
    h
  }

  def mapUsingFunc[K: Manifest, V: Manifest](count: Rep[Int], f: Exp[Int] => Exp[(K, V)]): Exp[HashMap[K, V]] = {
    val h = HashMap[K, V]()
    for (i <- 0 until count) {
      val pair = f(i)
      h.update(pair._1, pair._2)
    }
    h
  }

  def multiMapAppend[K: Manifest, V: Manifest](map: Exp[HashMap[K, scala.collection.mutable.ArrayBuilder[V]]], key: Exp[K], value: Exp[V]): Exp[HashMap[K, scala.collection.mutable.ArrayBuilder[V]]] = {
    if (map.contains(key)) {
      map(key) += value
      map
    } else {
      map.update(key, arrayBufferFromElem(value))
      map
    }
  }

  def mapUnion[K: Manifest, V: Manifest](left: Exp[HashMap[K, V]], right: Exp[HashMap[K, V]]): Exp[HashMap[K, V]] = {
    for (k <- right.keys) {
      left.update(k, right(k))
    }
    left
  }

  def mapDifference[K: Manifest, V: Manifest](left: Exp[HashMap[K, V]], right: Exp[HashMap[K, V]]): Exp[HashMap[K, V]] = {
    val h = HashMap[K, V]()
    for (k <- left.keys) {
      if (!right.contains(k)) h.update(k, left(k))
    }
    h
  }

  def mapReduce[K: Manifest, V: Manifest](left: Exp[HashMap[K, V]], right: Exp[HashMap[K, V]], reduce: Rep[(V, V)] => Rep[V]): Exp[HashMap[K, V]] = {
    val res = HashMap[K, V]()
    for (k <- left.keys) {
      res.update(k, if (right.contains(k)) reduce((left(k), right(k))) else left(k))
    }
    for (k <- right.keys) {
      if (!left.contains(k)) res.update(k, right(k))
    }
    res
  }

  def mapTransformValues[K: Manifest, V: Manifest, T: Manifest](in: Exp[HashMap[K, V]], f: Rep[V] => Rep[T]): Exp[HashMap[K, T]] = {
    val out = HashMap[K, T]()
    for (k <- in.keys) {
      out.update(k, f(in(k)))
    }
    out
  }

  def mapJoin[K: Manifest, V1: Manifest, V2: Manifest](left: Exp[HashMap[K, V1]], right: Exp[HashMap[K, V2]]): Exp[HashMap[K, (V1, V2)]] = {
    val h = HashMap[K, (V1, V2)]()
    for (k <- left.keys) {
      if (right.contains(k)) h.update(k, (left(k), right(k)))
    }
    h
  }

  def mapContains[K: Manifest, V: Manifest](map: Exp[HashMap[K, V]], key: Exp[K]): Exp[Boolean] = {
    map.contains(key)
  }

  def mapApply[K: Manifest, V: Manifest](map: Exp[HashMap[K, V]], key: Exp[K]): Exp[V] = {
    map(key)
  }

  def mapApplyIf[K: Manifest, V: Manifest, T: Manifest](map: Exp[HashMap[K, V]], key: Exp[K], exists: Exp[V] => Exp[T], otherwise: Exp[Unit] => Exp[T]): Exp[T] = {
    if (map.contains(key)) exists(map(key)) else otherwise()
  }

  def mapUpdate[K: Manifest, V: Manifest](map: Exp[HashMap[K, V]], key: Exp[K], value: Exp[V]): Exp[HashMap[K, V]] = {
    map.update(key, value)
    map
  }

  def mapSize[K: Manifest, V: Manifest](map: Exp[HashMap[K, V]]): Exp[Int] = {
    map.size
  }

  def mapToArray[K: Manifest, V: Manifest](map: Exp[HashMap[K, V]]): Exp[Array[(K, V)]] = {
    mapArray[K, (K, V)](map.keys.toArray, key => (key, map(key)))
  }

  def mapKeys[K: Manifest, V: Manifest](map: Exp[HashMap[K, V]]): Exp[Array[K]] = {
    map.keys.toArray
  }

  def mapValues[K: Manifest, V: Manifest](map: Exp[HashMap[K, V]]): Exp[Array[V]] = {
    map.values.toArray
  }

  def arrayGet[A: Manifest](a: Exp[Array[A]], i: Exp[Int]): Exp[A] = {
    a.at(i)
  }

  def arrayGather[A: Manifest](a: Exp[Array[A]], idxs: Exp[Array[Int]]): Exp[Array[A]] = {
    array(idxs.length)(i => a.at(idxs.at(i)))
  }

  def arrayLength[A: Manifest](a: Exp[Array[A]]): Exp[Int] = {
    a.length
  }

  def tuple[A: Manifest, B: Manifest](a: Exp[A], b: Exp[B]): Exp[(A, B)] = {
    Tuple2(a, b)
  }

  def first[A: Manifest, B: Manifest](tup: Exp[(A, B)]): Exp[A] = {
    tup._1
  }

  def second[A: Manifest, B: Manifest](tup: Exp[(A, B)]): Exp[B] = {
    tup._2
  }

  def opPlus[A: Numeric : Manifest](a: Exp[A], b: Exp[A]): Exp[A] = {
    a + b
  }

  def opMinus[A: Numeric : Manifest](a: Exp[A], b: Exp[A]): Exp[A] = {
    a - b
  }

  def opMult[A: Numeric : Manifest](a: Exp[A], b: Exp[A]): Exp[A] = {
    a * b
  }

  def opDiv[A: Numeric : Manifest](a: Exp[A], b: Exp[A]): Exp[A] = {
    a / b
  }

  def opMod(a: Exp[Int], b: Exp[Int]): Exp[Int] = a % b

  def opEq[A: Manifest](a: Exp[A], b: Exp[A]): Exp[Boolean] = {
    equals(a, b)
  }

  def Max[A: Manifest](left: Exp[A], right: Exp[A])(implicit ord: Ordering[A]) = {
    left.max(right)
  }

  def Min[A: Manifest](left: Exp[A], right: Exp[A])(implicit ord: Ordering[A]) = {
    left.min(right)
  }

  def stringConcat(a: Exp[String], b: Exp[String]): Exp[String] = {
    a + b
  }

  def stringContains(a: Exp[String], b: Exp[String]): Exp[Boolean] = {
    a.contains(b)
  }

  def stringStartsWith(a: Exp[String], b: Exp[String]): Exp[Boolean] = {
    a.startsWith(b)
  }

  def stringEndsWith(a: Exp[String], b: Exp[String]): Exp[Boolean] = {
    a.endsWith(b)
  }

  def stringMatches(a: Exp[String], b: Exp[String]): Exp[Boolean] = {
    string_matches(a, b)
  }

  def substring(str: Exp[String], start: Exp[Int], end: Exp[Int]) = {
    str.substring(start, end)
  }

  def charAt(str: Exp[String], index: Exp[Int]) = str.charAt(index)

  def stringToInt(str: Exp[String]) = str.toInt

  def stringToDouble(str: Exp[String]) = str.toDouble

  def mapArray[A: Manifest, B: Manifest](a: Exp[Array[A]], f: Rep[A] => Rep[B]): Exp[Array[B]] = {
    //    a.map(f)
    array(a.length)(i => f(a.at(i)))
  }

  def flatMapArray[A: Manifest, B: Manifest](arr: Exp[Array[A]], f: Rep[A] => Rep[Array[B]]): Exp[Array[B]] = {
    val buf = ArrayBuilder.make[B]
    for (x <- arr; y <- f(x)) {
      buf += y
    }
    buf.result
  }

  def findArray[A: Manifest](a: Exp[Array[A]], f: Rep[A] => Rep[Boolean]): Exp[Array[Int]] = {
    arrayIf(a.length) { i => (f(a.at(i)), i)}
  }

  def filterArray[A: Manifest](a: Exp[Array[A]], f: Rep[A] => Rep[Boolean]): Exp[Array[A]] = {
    arrayIf(a.length) { i => (f(a.at(i)), a.at(i))}
  }

  def countArray[A: Manifest](a: Exp[Array[A]], f: Rep[A] => Rep[Boolean]): Exp[Int] = {
    var count = 0
    for (x <- a) {
      if (f(x)) count += 1
    }
    count
  }

  def replicate[A: Manifest](length: Exp[Int], v: Exp[A]): Exp[Array[A]] = {
    array(length)(i => v)
  }

  def indexRangeD(length: Exp[Int]): Exp[Array[Int]] = {
    array(length)(i => i)
    array(length)(i => i)
  }

  def sum[A: Manifest](a: Exp[Array[A]]): Exp[A] = {
    sum(a.length) { i => a.at(i).AsInstanceOf[Double]}.AsInstanceOf[A]
  }

  def reduce[A: Manifest](a: Exp[Array[A]], zero: Exp[A], accumulate: Rep[(A, A)] => Rep[A]): Exp[A] = {
    var state = zero
    for (x <- a) {
      state = accumulate((state.AsInstanceOf[A], x))
    }
    state
  }

  def fold[A: Manifest, S: Manifest](a: Exp[Array[A]], init: Exp[S], func: Rep[(S, A)] => Rep[S]): Exp[S] = {
    var state = init
    for (x <- a) {
      state = func((state.AsInstanceOf[S], x))
    }
    state
  }

  def sumBy[A: Manifest, S: Manifest](a: Exp[Array[A]], func: Rep[A] => Rep[S])(implicit n: Numeric[S]): Exp[S] = {
    var sum = n.zero
    for (x <- a) {
      sum += func(x)
    }
    sum
  }


  def arrayMapReduce[T: Manifest, K: Manifest, V: Manifest](in: Exp[Array[T]], map: Rep[T] => Rep[(K, V)], reduce: Rep[(V, V)] => Rep[V]): Exp[HashMap[K, V]] = {
    val result = HashMap[K, V]()
    for (x <- in) {
      val pair = map(x)
      val key = pair._1
      val value = pair._2
      result.update(key, if (result.contains(key)) reduce((result(key), value)) else value)
    }
    result
  }

  def filterMapReduce[T: Manifest, K: Manifest, V: Manifest](in: Exp[Array[T]], filter: Rep[T] => Rep[Boolean], map: Rep[T] => Rep[(K, V)], reduce: Rep[(V, V)] => Rep[V]): Exp[HashMap[K, V]] = {
    val result = HashMap[K, V]()
    for (x <- in) {
      if (filter(x)) {
        val pair = map(x)
        val key = pair._1
        val value = pair._2
        result.update(key, if (result.contains(key)) reduce((result(key), value)) else value)
      }
    }
    result
  }

  def rangeMapReduce[K: Manifest, V: Manifest](n: Rep[Int], map: Rep[Int] => Rep[(K, V)], reduce: Rep[(V, V)] => Rep[V]): Exp[HashMap[K, V]] = {
    val result = HashMap[K, V]()
    for (i <- 0 until n) {
      val pair = map(i)
      val key = pair._1
      val value = pair._2
      result.update(key, if (result.contains(key)) reduce((result(key), value)) else value)
    }
    result
  }

  def rangeFilterMapReduce[T: Manifest, K: Manifest, V: Manifest](n: Rep[Int], map1: Rep[Int] => Rep[T], filter: Rep[T] => Rep[Boolean], map2: Rep[T] => Rep[(K, V)], reduce: Rep[(V, V)] => Rep[V]): Exp[HashMap[K, V]] = {
    val result = HashMap[K, V]()
    for (i <- 0 until n) {
      val x = map1(i)
      if (filter(x)) {
        val pair = map2(x)
        val key = pair._1
        val value = pair._2
        result.update(key, if (result.contains(key)) reduce((result(key), value)) else value)
      }
    }
    result
  }

  def lambda[A: Manifest, B: Manifest](f: Rep[A] => Rep[B]) = fun(f)

  def newArray[A: Manifest](length: Rep[Int]): Rep[Array[A]] = NewArray[A](length)

  def opZipWith[A: Manifest, B: Manifest, R: Manifest](f: (Rep[A], Rep[B]) => Rep[R], a: Exp[Array[A]], b: Exp[Array[B]]): Exp[Array[R]] = {
    array(a.length)(i => f(a.at(i), b.at(i)))
  }

  def opZip[A: Manifest, B: Manifest](a: Exp[Array[A]], b: Exp[Array[B]]): Exp[Array[(A, B)]] = {
    array[(A, B)](a.length)(i => (a.at(i), b.at(i)))
  }

  def arraySort[A: Manifest](a: Exp[Array[A]]): Exp[Array[A]] = {
    a.sort
  }

  //def arraySortBy[A: Manifest, B: Manifest](a: Exp[Array[A]], by: Rep[A] => Rep[B])(implicit o: Ordering[B]): Exp[Array[A]]
    //mapArray[(B,A),A](mapArray[A,(B,A)](a, x => (by(x), x)).sort, p => p._2)
    //a.map(x => (by(x), x)).sort.map(p => p._2)
    //val permutation = array(a.length)(i => (by(a.at(i)),i)).sort
    //array(a.length) { i => a.at(permutation.at(i)._2) }


  def arrayGroupBy[A: Manifest, K: Manifest](in: Exp[Array[A]], by: Rep[A] => Rep[K]): Exp[HashMap[K, scala.collection.mutable.ArrayBuilder[A]]] = {
    val result = HashMap[K, scala.collection.mutable.ArrayBuilder[A]]()
    for (x <- in) {
      val key = by(x)
      if (result.contains(key)) {
        result(key) += x
        ()
      } else {
        val buf = ArrayBuilder.make[A]
        buf += x
        result.update(key, buf)
      }
    }
    result
  }

  def opDotProductSV[A: Manifest](i1: Exp[Array[Int]], v1: Exp[Array[A]], i2: Exp[Array[Int]], v2: Exp[Array[A]]): Exp[A] = {
    array_dotProductSparse(i1, v1, i2, v2)
  }

  def parallelExecute[T: Manifest](nJobs: Exp[Int], f: Exp[Int] => Exp[T]): Exp[Array[T]] = {
    parallel_execute(nJobs, f)
  }

  def hashCode[T](obj: Exp[T]) : Exp[Int] = {
    hash_code(obj)
  }

  def arraySum[A: Manifest](xs: Exp[Array[A]])(implicit n : Numeric[A]): Exp[A] = {
    var sum = n.zero
    for (x <- xs) sum += x
    sum
  }
  def arrayMax[A: Manifest](xs: Exp[Array[A]])(implicit o : Ordering[A]): Exp[A] = {
    var max = xs.at(0) // we need Optional type to correctly implement min/max, but it is abselnt in CE
    for (x <- xs) if (x > max) max = x
    max
  }
  def arrayMin[A: Manifest](xs: Exp[Array[A]])(implicit o : Ordering[A]): Exp[A] = {
    var min = xs.at(0) // we need Optional type to correctly implement min/max, but it is abselnt in CE
    for (x <- xs) if (x < min) min = x
    min
  }

  def arrayAvg[A: Manifest](xs: Exp[Array[A]])(implicit n : Numeric[A]): Exp[Double] = {
    var sum = n.zero
    for (x <- xs) sum += x
    sum.AsInstanceOf[Double] / xs.length
  }

  def strideArray[A: Manifest](xs: Exp[Array[A]], start: Exp[Int], length: Exp[Int], stride: Exp[Int]) =
    array(length) { i =>
      xs.at(start + i * stride)
    }
  def updateArray[A: Manifest](xs: Exp[Array[A]], index: Exp[Int], value: Exp[A]) = {
    xs.update(index, value)
    xs
  }

  def ifThenElse[A:Manifest](cond: Exp[Boolean], iftrue: () => Exp[A], iffalse: () => Exp[A]) = {
    if (cond) iftrue() else iffalse()
  }

  //def printlnD(s: Exp[Any])  = println(s)
  def unitD[T:Manifest](x: T) = unit[T](x)
  /*
  def mkStringD[A:Manifest](a: Exp[DeliteArray[A]]) : Exp[String] = {
    a mkString unitD(" ")
  }  */
}

class CoreLmsBackend extends LmsBackend with LmsBackendFacade { self =>
  trait Codegen extends ScalaGenEffect with ScalaGenArrayOps with ScalaGenListOps with ScalaGenNumericOps
    with ScalaGenPrimitiveOps with ScalaGenEqual with ScalaGenBooleanOps with ScalaGenStruct with ScalaGenStringOps with ScalaGenEitherOps
    with ScalaGenTupleOps with ScalaGenFatArrayLoopsFusionOpt with ScalaGenIfThenElseFat with LoopFusionOpt
    with ScalaGenCastingOps with ScalaGenHashMapOps with ScalaGenIterableOps with ScalaGenOrderingOps with ScalaGenWhile
    with ScalaGenIfThenElse with ScalaGenVariables with ScalaGenArrayBuilderOps with ScalaGenExceptionOps with ScalaGenFunctions with ScalaGenRangeOps {

    val IR: self.type = self
    override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true

    private def isTuple(name: String) =
      name.startsWith("Tuple2") || name.startsWith("Tuple3") || name.startsWith("Tuple4") || name.startsWith("Tuple5")

    override def remap[A](m: Manifest[A]) =
      if (isTuple(m.runtimeClass.getSimpleName)) m.toString
      else super.remap(m)

    override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
      case Struct(ClassTag(name), elems) if isTuple(name) =>
        emitValDef(sym, "(" + elems.map(e => quote(e._2)).mkString(",") + ")")
      case _ => super.emitNode(sym, rhs)
    }
  }

  val codegen = new Codegen {}
}

class CommunityLmsBackend extends CoreLmsBackend with VectorOpsExp with SystemOpsExp { self =>
  override val codegen = new Codegen with ScalaGenVectorOps with ScalaGenSystemOps {
    override val IR: self.type = self
  }
}
