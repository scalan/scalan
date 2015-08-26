package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.ScalaNestedCodegen
import scalan.compilation.lms.cxx.sharedptr.CxxShptrCodegen

trait LstOps extends Base {

  def list_replicate[A: Manifest](len: Rep[Int], x: Rep[A])(implicit pos: SourceContext): Rep[List[A]]
  def list_rangeFrom0(length: Rep[Int]): Rep[List[Int]]

}

trait LstOpsExp extends LstOps with BaseExp with EffectExp with TupleOps {

  case class ListRangeFrom0Lms(length: Rep[Int]) extends Def[List[Int]]

  def list_rangeFrom0(length: Rep[Int]): Exp[List[Int]] = {
    ListRangeFrom0Lms(length)
  }

  case class ListReplicate[A: Manifest](len: Rep[Int], x: Rep[A]) extends Def[List[A]]

  def list_replicate[A: Manifest](len: Rep[Int], x: Rep[A])(implicit pos: SourceContext): Rep[List[A]] =
    ListReplicate(len, x)

  def list_reduce[A: Manifest](xs: Exp[List[A]], zero: Exp[A], accumulate: Exp[(A, A)] => Exp[A]): Exp[A] = {
    var state = zero
    list_foreach(xs) { x =>
      state = accumulate((state, x))
    }
    state
  }

  def list_foreach[A: Manifest](xs: Exp[List[A]])(block: Rep[A] => Rep[Unit])(implicit pos: SourceContext): Exp[Unit] = {
    val x = fresh[A]
    val b = reifyEffects(block(x))
    reflectEffect(ListForeach(xs, x, b), summarizeEffects(b).star)
  }
  case class ListForeach[A](a: Exp[List[A]], x: Sym[A], block: Block[Unit]) extends Def[Unit]

  // TODO try to figure out how to implement correctly to be usable from CoreBridge
//  def sumList[A](xs: Exp[List[A]])(implicit m: Manifest[A]/*, n: Numeric[A]*/): Exp[A] = {
//    var state: Exp[Double] = 0.0 // unit(n.zero)
//    for (x <- xs) {
//      state = state + x.AsInstanceOf[Double]
//    }
//    state.AsInstanceOf[A]
//  }

  override def mirror[A: Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(ListForeach(a,x,b), u, es) =>
      reflectMirrored(Reflect(ListForeach(f(a),f(x).asInstanceOf[Sym[A]],f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]

  override def syms(e: Any): List[Sym[Any]] = e match {
    case ListForeach(a, x, body) => syms(a):::syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ListForeach(a, x, body) => x :: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ListForeach(a, x, body) => freqNormal(a):::freqHot(body)
    case _ => super.symsFreq(e)
  }

}

trait ScalaGenLstOps extends ScalaNestedCodegen {
  val IR: LstOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ListReplicate(len, x) => emitValDef(sym, src"List.fill($len)($x)")
    case ListRangeFrom0Lms(length) => emitValDef(sym, src"List((for(i <- 0 until ${quote(length)}) yield i ) : _*)")
    case ListForeach(a,x,block) =>
      gen"""val $sym = $a.foreach{ $x =>
           |${nestedBlock(block)}
           |$block
           |}"""
    case _ => super.emitNode(sym, rhs)
  }
}

trait CxxShptrGenLstOps extends CxxShptrCodegen {
  val IR: LstOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ListReplicate(len, x) =>
      emitConstruct(sym, src"$len", src"$x")
//    case ListRangeFrom0Lms(length) => emitValDef(sym, src"List((for(i <- 0 to ${quote(length)}) yield i ) : _*)")
    case _ => super.emitNode(sym, rhs)
  }
}
