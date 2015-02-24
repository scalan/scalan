package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericNestedCodegen

trait ArrayOpsExt extends Base {

  def foldArray[A: Manifest, B: Manifest](a: Rep[Array[A]], init: Rep[B], f: Rep[(B, A)] => Rep[B]): Rep[B]
  def array_new[A: Manifest](len: Rep[Int]): Rep[Array[A]]
  def array_append[A: Manifest](xs: Rep[Array[A]], value: Rep[A]): Rep[Array[A]]
}

trait ArrayOpsExtExp extends BaseExp with EffectExp with ArrayOpsExp with ArrayBuilderOpsExp {

  case class FoldArray[A, B: Manifest](a: Exp[Array[A]], x: Sym[(B, A)], init: Exp[B], b: Block[B]) extends Def[B] {
    val m = manifest[B]
  }

  def foldArray[A, B: Manifest](a: Exp[Array[A]], x: Sym[(B, A)], init: Exp[B], b: Block[B]): Exp[B] = {
    FoldArray(a, x, init, b)
  }

  def foldArray[A: Manifest, B: Manifest](a: Rep[Array[A]], init: Rep[B], f: Rep[(B, A)] => Rep[B]) = {
    val x = fresh[(B, A)]
    val b = reifyEffects[B](f(x))
    reflectEffect(FoldArray(a, x, init, b), summarizeEffects(b).star)
  }

  def array_new[A: Manifest](len: Rep[Int]): Rep[Array[A]] = ArrayNew[A](len)

  def array_append[A: Manifest](xs: Rep[Array[A]], value: Rep[A]): Rep[Array[A]] = {
    val bu = ArrayBuilder.make[A]
    for(a <- xs ) {bu += a}
    bu.result
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case FoldArray(a, x, init, b) => foldArray(f(a), x, f(init), f(b))(mtype(manifest[A]))
    case Reflect(FoldArray(a, x, init, b), u, es) =>
      reflectMirrored(Reflect(FoldArray(f(a), x, f(init), f(b)), mapOver(f, u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case FoldArray(a, x, init, b) if addControlDeps => syms(a) ::: syms(init) ::: syms(b)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case FoldArray(a, x, init, b) => x :: effectSyms(b)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case FoldArray(a, x, init, b) => freqNormal(init) ::: freqNormal(a) ::: freqHot(b)
    case _ => super.symsFreq(e)
  }
}

trait ScalaGenArrayOpsExt extends GenericNestedCodegen with ScalaGenBase {
  val IR: ArrayOpsExtExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case FoldArray(a, x, init, b) =>
      gen"""val $sym = $a.fold($init){(a, b) =>
           |val $x = (a, b)
           |${nestedBlock(b)}
           |$b
           |}"""
    case _ => super.emitNode(sym, rhs)
  }
}
