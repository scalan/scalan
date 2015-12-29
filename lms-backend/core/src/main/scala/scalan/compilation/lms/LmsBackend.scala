package scalan.compilation.lms

import java.io.File

import scala.collection.mutable
import scala.lms.common._
import scala.lms.internal.{Expressions, GenericNestedCodegen, Effects}
import scalan.compilation.language.Adjusted
import scalan.compilation.lms.arrays.{ArrayMutationExp, ArrayLoopsFatExp}
import scalan.compilation.lms.common._
import scalan.compilation.lms.graph.GraphCodegen
import scalan.compilation.lms.scalac.{ScalaMethodCallOpsExp, ScalaCoreCodegen}
import scalan.util.FileUtil
import java.util.HashMap


trait BaseCodegen[BackendType <: Expressions with Effects] extends GenericNestedCodegen { codegen =>
  val IR: BackendType
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Reify(s, u, effects) =>
      emitValDef(sym, quote(s))
    // FIXME: In LMS it has a comment "just ignore -- effects are accounted for in emitBlock", but
    // this leads to incorrect code generated in Scalan
    case _ => super.emitNode(sym, rhs)
  }

  def codePackage: Option[String] = None

  def createFile[T : Manifest, R : Manifest](f: Exp[T] => Exp[R], className: String, sourcesDir: File): File = {

    val sourceFile = new File(sourcesDir, s"$className.$kernelFileExt")
    FileUtil.withFile(sourceFile) { writer =>
      emitSource[T, R](f, className, writer)
      //          val s = lms.fresh[a](mA)
      //          val body = codegen.reifyBlock(facade.apply(s))(mB)
      //          codegen.emitSource(List(s), body, functionName, writer)(mB)
      //          val lms.TP(sym,_) = lms.globalDefs.last
      //          codegen.emitDepGraph( sym, new File( sourcesDir, functionName + "-LMS.dot" ).getAbsolutePath )
      emitDataStructures(writer)
    }
    sourceFile

  }

  // Below definitions make src"" extensible by allowing override of quoteOrRemap
  class CodegenHelper1(sc: StringContext) extends CodegenHelper(sc) {
    override def quoteOrRemap(arg: Any) = codegen.quoteOrRemap(arg)
  }

  override implicit def CodegenHelper(sc: StringContext) = new CodegenHelper1(sc)

  def quoteOrRemap(arg: Any): String = arg match {
    case xs: Seq[_] => xs.map(quoteOrRemap).mkString(",")
    case e: Exp[_] => quote(e)
    case m: Manifest[_] => remap(m)
    case s: String => s
    case _ => throw new RuntimeException(s"Could not quote or remap $arg")
  }
}


abstract class LmsBackend extends LmsBackendFacade { self =>

  val codegen: BaseCodegen[self.type]
  val graphCodegen: GraphCodegen[self.type] = new GraphCodegen(self)

}

abstract class LmsBackendFacade extends ObjectOpsExtExp with LiftVariables with LiftPrimitives with LiftNumeric with ArrayOpsExtExp with ListOpsExp
  with LstOpsExp with StringOpsExp with NumericOpsExp with RangeOpsExp with PrimitiveOpsExp with FunctionsExp with HashMapOpsExpExt
  with EqualExp with BooleanOpsExp with TupleOpsExp with ArrayLoopsFatExp with ArrayMutationExp with OrderingOpsExp
  with IfThenElseFatExp with VariablesExpOpt
  with ArrayOpsExp with IterableOpsExp with WhileExp with ArrayBuilderOpsExp with ExtNumOpsExp
// FIXME using CastingOpsExpOpt instead of CastingOpsExp generates wrong code, perhaps due to LMS bug
// Without it we get useless casts and more code than should be present
  with CastingOpsExp with EitherOpsExp with MathOpsExp with ExceptionOpsExp with SystemOpsExp
  with WhileExpExt with ListOpsExpExt with FunctionsExpExt
// FIXME using StructFatExpOptCommon instead of StructExpOptCommon leads to bad code generated in LmsMstPrimeItTests
// Not clear whether this is due to our or LMS error
// Replace StructExpOptCommon with StructExp if needed to verify codegen works correctly;
// otherwise they can get optimized out
  with MiscOpsExtExp with StructExpOptCommon with Effects {
  def toStringWithDefinition(x: Exp[_]) = s"$x: ${x.tp}" + (x match {
    case sym: Sym[_] =>
      findDefinition(sym) match {
        case Some(d) => " = " + infix_rhs(d)
        case _ => " (no definition)"
      }
    case _ => ""
  })

  def tuple2[A: Manifest, B: Manifest](a: Exp[A], b: Exp[B]): Exp[(A, B)] = make_tuple2(a -> b)

  def num_negate[T: Manifest](arg: Exp[T])(implicit n: Numeric[T]) = {
    n.zero - arg
  }

  def boolean_to_int(bool: Exp[Boolean]) = if (bool) 1 else 0

  // TODO implement for all types and contribute upstream
  def integral_mod[A: Integral: Manifest](arg1: Exp[A], arg2: Exp[A]) = manifest[A] match {
    case Manifest.Int =>
      int_mod(arg1.asInstanceOf[Exp[Int]], arg2.asInstanceOf[Exp[Int]])
    case mA =>
      throw new IllegalStateException(s"LMS only supports mod operation for Int, got $mA instead")
  }

  def semicolon[A, B: Manifest](left: Exp[A], right: Exp[B]) = {
    val l = left
    right
  }

  def semicolon_multi[B: Manifest](left: Seq[Exp[_]], right: Exp[B]) = {
    val l = left
    right
  }

  def loop_until[A: Manifest](init: Exp[A], step: Rep[A] => Rep[A], cond: Rep[A] => Rep[Boolean]): Exp[A] = {
    // TODO check correctness
    var state = init
    while (!cond(state)) state = step(state)
    state
  }

  //
  // ArrayBuffer
  //
  def arrayBufferUsingFunc[T: Manifest](count: Rep[Int], f: Exp[Int] => Exp[T]): Exp[mutable.ArrayBuilder[T]] = {
    val buf = ArrayBuilder.make[T]
    for (i <- 0 until count) {
      buf += f(i)
    }
    buf
  }

  def arrayBufferFromElem[T: Manifest](elem: Exp[T]): Exp[mutable.ArrayBuilder[T]] = {
    val buf = ArrayBuilder.make[T]
    buf += elem
    buf
  }

  def arrayBufferApply[T: Manifest](buf: Exp[mutable.ArrayBuilder[T]], i: Exp[Int]): Exp[T] = ???

  // TODO optimize
  def arrayBufferLength[T: Manifest](buf: Exp[mutable.ArrayBuilder[T]]): Exp[Int] = buf.result.length

  def arrayBufferMap[A: Manifest, B: Manifest](buf: Exp[mutable.ArrayBuilder[A]], f: Rep[A] => Rep[B]): Exp[mutable.ArrayBuilder[B]] = {
    val res = ArrayBuilder.make[B]
    val arr = buf.result
    val count = arr.length
    for (i <- 0 until count) {
      res += f(arr(i))
    }
    res
  }

  def arrayBufferUpdate[T: Manifest](buf: Exp[mutable.ArrayBuilder[T]], i: Exp[Int], v: Exp[T]): Exp[mutable.ArrayBuilder[T]] = ???

  def arrayBufferInsert[T: Manifest](buf: Exp[mutable.ArrayBuilder[T]], i: Exp[Int], v: Exp[T]): Exp[mutable.ArrayBuilder[T]] = ???

  def arrayBufferRemove[T: Manifest](buf: Exp[mutable.ArrayBuilder[T]], i: Exp[Int], n: Exp[Int]): Exp[mutable.ArrayBuilder[T]] = ???

  def arrayBufferAppend[T: Manifest](buf: Exp[mutable.ArrayBuilder[T]], v: Exp[T]): Exp[mutable.ArrayBuilder[T]] = {
    buf += v
    buf
  }

  def arrayBufferAppendArray[T: Manifest](buf: Exp[mutable.ArrayBuilder[T]], a: Exp[Array[T]]): Exp[mutable.ArrayBuilder[T]] = {
    for (x <- a) {
      buf += x
    }
    buf
  }

  def arrayBufferReset[T: Manifest](buf: Exp[mutable.ArrayBuilder[T]]): Exp[mutable.ArrayBuilder[T]] = {
    buf.clear()
    buf
  }

  //
  // Map
  //
  def map_usingFunc[K: Manifest, V: Manifest](count: Rep[Int], f: Exp[Int] => Exp[(K, V)]): Exp[HashMap[K, V]] = {
    val h = HashMap[K, V]()
    for (i <- 0 until count) {
      val pair = f(i)
      h.update(pair._1, pair._2)
    }
    h
  }

  def multiMap_append[K: Manifest, V: Manifest](map: Exp[HashMap[K, mutable.ArrayBuilder[V]]], key: Exp[K], value: Exp[V]): Exp[HashMap[K, mutable.ArrayBuilder[V]]] = {
    if (map.contains(key)) {
      map(key) += value
      map
    } else {
      map.update(key, arrayBufferFromElem(value))
      map
    }
  }

  def map_union[K: Manifest, V: Manifest](left: Exp[HashMap[K, V]], right: Exp[HashMap[K, V]]): Exp[HashMap[K, V]] = {
    for (k <- right.keys) {
      left.update(k, right(k))
    }
    left
  }

  def map_difference[K: Manifest, V: Manifest](left: Exp[HashMap[K, V]], right: Exp[HashMap[K, V]]): Exp[HashMap[K, V]] = {
    val h = HashMap[K, V]()
    for (k <- left.keys) {
      if (!right.contains(k)) h.update(k, left(k))
    }
    h
  }

  def map_reduce[K: Manifest, V: Manifest](left: Exp[HashMap[K, V]], right: Exp[HashMap[K, V]], reduce: Rep[(V, V)] => Rep[V]): Exp[HashMap[K, V]] = {
    val res = HashMap[K, V]()
    for (k <- left.keys) {
      res.update(k, if (right.contains(k)) reduce((left(k), right(k))) else left(k))
    }
    for (k <- right.keys) {
      if (!left.contains(k)) res.update(k, right(k))
    }
    res
  }

  def map_transformValues[K: Manifest, V: Manifest, T: Manifest](in: Exp[HashMap[K, V]], f: Rep[V] => Rep[T]): Exp[HashMap[K, T]] = {
    val out = HashMap[K, T]()
    for (k <- in.keys) {
      out.update(k, f(in(k)))
    }
    out
  }

  def map_join[K: Manifest, V1: Manifest, V2: Manifest](left: Exp[HashMap[K, V1]], right: Exp[HashMap[K, V2]]): Exp[HashMap[K, (V1, V2)]] = {
    val h = HashMap[K, (V1, V2)]()
    for (k <- left.keys) {
      if (right.contains(k)) h.update(k, (left(k), right(k)))
    }
    h
  }

//  def map_getOrElse[K: Manifest, V: Manifest](map: Exp[HashMap[K, V]], key: Exp[K], otherwise: Exp[Unit] => Exp[V]): Exp[V] = {
//    map.getOrElse(key, otherwise(()))
//  }

  def map_mapValueIfExists[K: Manifest, V: Manifest, T: Manifest](map: Exp[HashMap[K, V]], key: Exp[K], exists: Exp[V] => Exp[T], otherwise: Exp[Unit] => Exp[T]): Exp[T] = {
    if (map.contains(key)) exists(map(key)) else otherwise(())
  }

  def map_update[K: Manifest, V: Manifest](map: Exp[HashMap[K, V]], key: Exp[K], value: Exp[V]): Exp[HashMap[K, V]] = {
    map.update(key, value)
    map
  }

  def map_toArray[K: Manifest, V: Manifest](map: Exp[HashMap[K, V]]): Exp[Array[(K, V)]] = {
    arrayMap[K, (K, V)](map_keys(map), key => (key, map(key)))
  }

  def map_keys[K: Manifest, V: Manifest](map: Exp[HashMap[K, V]]): Exp[Array[K]] = {
    map.keys.toArray
  }

  def map_values[K: Manifest, V: Manifest](map: Exp[HashMap[K, V]]): Exp[Array[V]] = {
    map.values.toArray
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

  //def arraySortBy[A: Manifest, B: Manifest](a: Exp[Array[A]], by: Rep[A] => Rep[B])(implicit o: Ordering[B]): Exp[Array[A]]
    //mapArray[(B,A),A](mapArray[A,(B,A)](a, x => (by(x), x)).sort, p => p._2)
    //a.map(x => (by(x), x)).sort.map(p => p._2)
    //val permutation = array(a.length)(i => (by(a.at(i)),i)).sort
    //array(a.length) { i => a.at(permutation.at(i)._2) }


  def arrayGroupBy[A: Manifest, K: Manifest](in: Exp[Array[A]], by: Rep[A] => Rep[K]): Exp[HashMap[K, mutable.ArrayBuilder[A]]] = {
    val result = HashMap[K, mutable.ArrayBuilder[A]]()
    for (x <- in) {
      val key = by(x)
      if (result.contains(key)) {     // TODO optimize: make result(key) return empty builder for new keys
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

  def list_length[A:Manifest](l: Rep[List[A]]) = {
    list_toarray[A](l).length      // TODO optimize
  }

  def ifThenElse[A:Manifest](cond: Exp[Boolean], iftrue: () => Exp[A], iffalse: () => Exp[A]) = {
    if (cond) iftrue() else iffalse()
  }

  def reify[T:Manifest](x: Exp[T], u: Summary, effects: List[Exp[Any]]): Exp[T] = {
    toAtom(Reify(x, u, effects))
  }

  def unitD[T: Manifest](x: T) = unit[T](x)
}

abstract class CoreLmsBackend extends LmsBackend

class ScalaCoreLmsBackend extends CoreLmsBackend with ScalaMethodCallOpsExp { self =>
  override val codegen = new ScalaCoreCodegen[self.type](self)
}
