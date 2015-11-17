// copied from LMS to avoid depending on tests
// Apply any changes made there! To check:
// git log v0.9.0..HEAD test-src/epfl/test7-analysis/Arrays.scala
// Don't forget to update the command after applying changes
package scalan.compilation.lms.arrays

import scala.lms.common._
import scala.lms.internal.AbstractSubstTransformer
import scala.lms.util.OverloadHack
import scala.reflect.SourceContext

trait ArrayLoops extends ArrayOps with Loops with OverloadHack {
  def array[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[T]): Rep[Array[T]]
  def sum(shape: Rep[Int])(f: Rep[Int] => Rep[Double]): Rep[Double] // TODO: make reduce operation configurable!
  def sumInt(shape: Rep[Int])(f: Rep[Int] => Rep[Int]): Rep[Int] // TODO: make reduce operation configurable!
  def arrayIf[T:Manifest](shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[T])): Rep[Array[T]]
  def sumIf(shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[Double])): Rep[Double] // TODO: make reduce operation configurable!
  def sumIfInt(shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[Int])): Rep[Int] // TODO: make reduce operation configurable!
  def flatten[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[Array[T]]): Rep[Array[T]]

  def infix_at[T:Manifest](a: Rep[Array[T]], i: Rep[Int]): Rep[T]
  def infix_length[T:Manifest](a: Rep[Array[T]]): Rep[Int]
}


trait ArrayLoopsExp extends ArrayOpsExp with LoopsExp { //A.Filippov - add ArrayOpsExp

  case class ArrayElem[T](y: Block[T])(implicit val m: Manifest[T]) extends Def[Array[T]]
  case class ReduceElem(y: Block[Double]) extends Def[Double]
  case class ReduceIntElem(y: Block[Int]) extends Def[Int]

  case class ArrayIfElem[T](c: Exp[Boolean], y: Block[T])(implicit val m: Manifest[T]) extends Def[Array[T]]
  case class ReduceIfElem(c: Exp[Boolean], y: Block[Double]) extends Def[Double]
  case class ReduceIfIntElem(c: Exp[Boolean], y: Block[Int]) extends Def[Int]

  case class FlattenElem[T](y: Block[Array[T]]) extends Def[Array[T]]
  case class FlattenIfElem[T](c: Exp[Boolean], y: Block[Array[T]]) extends Def[Array[T]]

  case class ArrayIndex[T](a: Rep[Array[T]], i: Rep[Int]) extends Def[T]
  // case class ArrayLength[T](a: Rep[Array[T]]) extends Def[Int] // A.Filippov - this class is defined in ArrayOpsExp

  def array[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[T]): Rep[Array[T]] = {
    val x = fresh[Int]
    val y = reifyEffects(f(x))
    simpleLoop(shape, x, ArrayElem(y))
  }

  def arrayIf[T:Manifest](shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[T])): Rep[Array[T]] = {
    val x = fresh[Int]
    var c: Rep[Boolean] = null
    val y = reifyEffects { val p = f(x); c = p._1; p._2 }
    simpleLoop(shape, x, ArrayIfElem(c,y)) // TODO: simplify for const true/false
  }

  def flatten[T:Manifest](shape: Rep[Int])(f: Rep[Int] => Rep[Array[T]]): Rep[Array[T]] = {
    val x = fresh[Int]
    val y = reifyEffects(f(x))
    simpleLoop(shape, x, FlattenElem(y))
  }

  def flattenIf[T:Manifest](shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[Array[T]])): Rep[Array[T]] = {
    val x = fresh[Int]
    var c: Rep[Boolean] = null
    val y = reifyEffects { val p = f(x); c = p._1; p._2 }
    simpleLoop(shape, x, FlattenIfElem(c,y)) // TODO: simplify for const true/false
  }

  def sum(shape: Rep[Int])(f: Rep[Int] => Rep[Double]): Rep[Double] = {
    val x = fresh[Int]
    val y = reifyEffects(f(x))
    simpleLoop(shape, x, ReduceElem(y))
  }

  def sumInt(shape: Rep[Int])(f: Rep[Int] => Rep[Int]): Rep[Int] = {
    val x = fresh[Int]
    val y = reifyEffects(f(x))
    simpleLoop(shape, x, ReduceIntElem(y))
  }

  def sumIf(shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[Double])): Rep[Double] = {
    val x = fresh[Int]
    //val (c,y) = f(x)
    var c: Rep[Boolean] = null
    val y = reifyEffects { val p = f(x); c = p._1; p._2 }
    simpleLoop(shape, x, ReduceIfElem(c,y)) // TODO: simplify for const true/false
  }

  def sumIfInt(shape: Rep[Int])(f: Rep[Int] => (Rep[Boolean],Rep[Int])): Rep[Int] = {
    val x = fresh[Int]
    //val (c,y) = f(x)
    var c: Rep[Boolean] = null
    val y = reifyEffects { val p = f(x); c = p._1; p._2 }
    simpleLoop(shape, x, ReduceIfIntElem(c,y)) // TODO: simplify for const true/false
  }


  def infix_at[T:Manifest](a: Rep[Array[T]], i: Rep[Int]): Rep[T] = ArrayIndex(a, i)

  def infix_length[T:Manifest](a: Rep[Array[T]]): Rep[Int] = a match {
    case Def(SimpleLoop(s, x, ArrayElem(y))) => s
    case _ => ArrayLength(a)
  }


  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ArrayElem(y) => effectSyms(y)
    case ReduceElem(y) => effectSyms(y)
    case ReduceIntElem(y) => effectSyms(y)
    case FlattenElem(y) => effectSyms(y)
    case _ => super.boundSyms(e)
  }


  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(ArrayIndex(l,r), u, es) => reflectMirrored(Reflect(ArrayIndex(f(l),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case SimpleLoop(s,i, ArrayElem(y)) if f.hasContext =>
      array(f(s)) { j => f.asInstanceOf[AbstractSubstTransformer{val IR:ArrayLoopsExp.this.type}].withSubstScope(i -> j) { f.reflectBlock(y) } }
    case ArrayIndex(a,i) => infix_at(f(a), f(i))(mtype(manifest[A]))
    case ArrayLength(a) => infix_length(f(a))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  override def mirrorFatDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case ArrayElem(y) => ArrayElem(f(y))
    case ReduceElem(y) => ReduceElem(f(y))
    case ReduceIntElem(y) => ReduceIntElem(f(y))
    case ArrayIfElem(c,y) => ArrayIfElem(f(c),f(y))
    case ReduceIfElem(c,y) => ReduceIfElem(f(c),f(y))
    case ReduceIfIntElem(c,y) => ReduceIfIntElem(f(c),f(y))
    case FlattenElem(y) => FlattenElem(f(y))
    case FlattenIfElem(c,y) => FlattenIfElem(f(c),f(y))
    case _ => super.mirrorFatDef(e,f)
  }).asInstanceOf[Def[A]]


}

trait ArrayLoopsFatExp extends ArrayLoopsExp with LoopsFatExp




trait ScalaGenArrayLoops extends ScalaGenLoops {
  val IR: ArrayLoopsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case SimpleLoop(s,x,ArrayElem(y)) =>
      stream.println("val " + quote(sym) + " = LoopArray("+quote(s)+") { " + quote(x) + " => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")
    case SimpleLoop(s,x,ReduceElem(y)) =>
      stream.println("val " + quote(sym) + " = LoopReduce("+quote(s)+") { " + quote(x) + " => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")
    case SimpleLoop(s,x,ReduceIntElem(y)) =>
      stream.println("val " + quote(sym) + " = LoopReduce("+quote(s)+") { " + quote(x) + " => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")
    // TODO: conditional variants ...
    case SimpleLoop(s,x,FlattenElem(y)) =>
      stream.println("val " + quote(sym) + " = LoopFlatten("+quote(s)+") { " + quote(x) + " => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")
    case ArrayIndex(a,i) =>
      emitValDef(sym, quote(a) + ".apply(" + quote(i) + ")")
    case ArrayLength(a) =>
      emitValDef(sym, quote(a) + ".length")
    case _ => super.emitNode(sym, rhs)
  }
}

trait ScalaGenArrayLoopsFat extends ScalaGenArrayLoops with ScalaGenLoopsFat {
  val IR: ArrayLoopsFatExp
  import IR._

  override def emitFatNode(sym: List[Sym[Any]], rhs: FatDef) = rhs match {
    case SimpleFatLoop(s,x,rhs) => {
      for ((l,r) <- sym zip rhs) {
        r match {
          case ArrayElem(y) =>
            stream.println("var " + quote(l) + " = new Array[" + remap(getBlockResultFull(y).tp) + "]("+quote(s)+")")
          // TODO generalize over Monoid
          case ReduceElem(y) =>
            stream.println("var " + quote(l) + ": " + remap(getBlockResult(y).tp) + " = 0")
          case ReduceIntElem(y) =>
            stream.println("var " + quote(l) + ": " + remap(getBlockResult(y).tp) + " = 0")

          case ArrayIfElem(c,y) =>
            stream.println("var " + quote(l) + "_buf = scala.collection.mutable.ArrayBuilder.make[" + remap(getBlockResult(y).tp) + "]")
          case ReduceIfElem(c,y) =>
            stream.println("var " + quote(l) + ": " + remap(getBlockResult(y).tp) + " = 0")
          case ReduceIfIntElem(c,y) =>
            stream.println("var " + quote(l) + ": " + remap(getBlockResult(y).tp) + " = 0")
          case FlattenElem(y) =>
            val mR = getBlockResult(y).tp
            assert(mR.runtimeClass.isArray)
            stream.println("var " + quote(l) + "_buf = scala.collection.mutable.ArrayBuilder.make[" + remap(mR.typeArguments(0)) + "]")
          case FlattenIfElem(c,y) =>
            val mR = getBlockResult(y).tp
            stream.println("var " + quote(l) + "_buf = scala.collection.mutable.ArrayBuilder.make[" + remap(mR.typeArguments(0)) + "]")
        }
      }
      val ii = x // was: x(i)
      stream.println("var " + quote(ii) + " = 0")
      stream.println("while ("+quote(ii)+" < "+quote(s)+") {")
      emitFatBlock(syms(rhs).map(Block(_))) // TODO: check this
      for ((l,r) <- sym zip rhs) {
        r match {
          case ArrayElem(y) =>
            stream.println(quote(l) + "("+quote(ii)+") = " + quote(getBlockResult(y)))
          case ReduceElem(y) =>
            stream.println(quote(l) + " += " + quote(getBlockResult(y)))
          case ReduceIntElem(y) =>
            stream.println(quote(l) + " += " + quote(getBlockResult(y)))
          case ArrayIfElem(c,y) =>
            stream.println("if ("+quote(/*getBlockResult*/(c))+") " + quote(l) + "_buf += " + quote(getBlockResult(y)))
          case ReduceIfElem(c,y) =>
            stream.println("if ("+quote(/*getBlockResult*/(c))+") " + quote(l) + " += " + quote(getBlockResult(y)))
          case ReduceIfIntElem(c,y) =>
            stream.println("if ("+quote(/*getBlockResult*/(c))+") " + quote(l) + " += " + quote(getBlockResult(y)))
          case FlattenElem(y) =>
            stream.println(quote(l) + "_buf ++= " + quote(getBlockResult(y)))
          case FlattenIfElem(c,y) =>
            stream.println("if ("+quote(/*getBlockResult*/(c))+") " + quote(l) + "_buf ++= " + quote(getBlockResult(y)))
        }
      }
      stream.println(quote(ii)+" += 1")
      stream.println("}")
      for ((l,r) <- sym zip rhs) {
        r match {
          case ArrayIfElem(_, _) =>
            stream.println("val " + quote(l) + " = " + quote(l) + "_buf.result")
          case FlattenElem(_) =>
            stream.println("val " + quote(l) + " = " + quote(l) + "_buf.result")
          case FlattenIfElem(_, _) =>
            stream.println("val " + quote(l) + " = " + quote(l) + "_buf.result")
          case _ =>
        }
      }
    }
    case _ => super.emitFatNode(sym, rhs)
  }
}








trait Arrays extends Base with OverloadHack {
  def zeroes(n: Rep[Int]): Rep[Array[Int]]
  def infix_update(a: Rep[Array[Int]], x: Rep[Int], v: Rep[Int]): Rep[Array[Int]]
  def infix_+(a: Rep[Array[Int]], b: Rep[Array[Int]])(implicit o: Overloaded1): Rep[Array[Int]]
}

trait ArraysExp extends Arrays with EffectExp {
  case class ArrayZero(n: Rep[Int]) extends Def[Array[Int]]
  case class ArrayUpdate(a: Rep[Array[Int]], x: Rep[Int], v: Rep[Int]) extends Def[Array[Int]]
  case class ArrayPlus(a: Rep[Array[Int]], b: Rep[Array[Int]]) extends Def[Array[Int]]
  def zeroes(n: Rep[Int]) = ArrayZero(n)
  def infix_update(a: Rep[Array[Int]], x: Rep[Int], v: Rep[Int]) = ArrayUpdate(a,x,v)
  def infix_+(a: Rep[Array[Int]], b: Rep[Array[Int]])(implicit o: Overloaded1) = ArrayPlus(a,b)
}

trait ScalaGenArrays extends ScalaGenEffect {
  val IR: ArraysExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayZero(n) =>
      emitValDef(sym, "new Array[Int](" + quote(n) + ")")
    case ArrayUpdate(a,x,v) =>
      emitValDef(sym, quote(a) +".clone()")
      stream.println(quote(sym) + "(" + quote(x) + ") = " + quote(v))
    case ArrayPlus(a,b) =>
      emitValDef(sym, "new Array[Int](" + quote(a) + ".length)")
      stream.println("arrayPlus("+ quote(sym) + "," + quote(a) + "," + quote(b) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}
