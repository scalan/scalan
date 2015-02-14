package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericNestedCodegen

trait LoopOpsExt extends Base {

  def continueUntil[A: Manifest](s1: Rep[A], stepF: Rep[A] => Rep[A], matchF: Rep[A] => Rep[Boolean]): Rep[A]

}

trait LoopOpsExtExp extends LoopOpsExt with BaseExp with EffectExp {

  case class LoopUntilLms[A: Manifest](s1: Rep[A], stepX: Sym[A], stepF: Block[A], matchX: Sym[A], matchF: Block[Boolean]) extends Def[A] {
    val m = manifest[A]
  }

  def continueUntil[A: Manifest](s1: Rep[A], stepX: Sym[A], stepF: Block[A], matchX: Sym[A], matchF: Block[Boolean]): Exp[A] = {
    LoopUntilLms(s1, stepX, stepF, matchX, matchF)
  }

  def continueUntil[A: Manifest](s1: Rep[A], stepF: Rep[A] => Exp[A], matchF: Rep[A] => Rep[Boolean]): Exp[A] = {
    val x1 = fresh[A]
    val stepBlock = reifyEffects[A](stepF(x1))
    val x2 = fresh[A]
    val isMatchBlock = reifyEffects[Boolean](matchF(x2))
    reflectEffect(LoopUntilLms(s1, x1, stepBlock, x2, isMatchBlock), summarizeEffects(stepBlock).star)
  }

  override def mirror[A: Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case LoopUntilLms(s1, stepX, stepF, matchX, matchF) => continueUntil(f(s1), stepX, f(stepF), matchX, f(matchF))(mtype(manifest[A]))
    case Reflect(LoopUntilLms(s1, stepX, stepF, matchX, matchF), u, es) =>
      reflectMirrored(Reflect(LoopUntilLms(f(s1), stepX, f(stepF), matchX, f(matchF)), mapOver(f, u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e, f)
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case LoopUntilLms(s1, stepX, stepF, matchX, matchF) if addControlDeps => syms(s1) ::: syms(stepF) ::: syms(matchF)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case LoopUntilLms(s1, stepX, stepF, matchX, matchF) => stepX :: matchX :: effectSyms(stepF) ::: effectSyms(matchF)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case LoopUntilLms(s1, stepX, stepF, matchX, matchF) => freqNormal(s1) ::: freqHot(stepF) ::: freqHot(matchF)
    case _ => super.symsFreq(e)
  }
}

trait ScalaGenLoopOpsExt extends GenericNestedCodegen {
  val IR: LoopOpsExtExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case LoopUntilLms(s1, stepX, stepF, matchX, matchF) =>
      gen"""
          |def $sym = {
          |val $matchX = $s1
          |${nestedBlock(matchF)}
          |$matchF
          |}
          |while(!$sym) {
          |val $stepX = $s1
          |${nestedBlock(stepF)}
          |$s1 = $stepF
          |}"""
    case _ => super.emitNode(sym, rhs)
  }
}
