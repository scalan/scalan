package scalan.compilation.lms.scalac

import scala.lms.common._
import scala.lms.internal.Effects
import scala.reflect.SourceContext

trait ScalaMethodCallOps extends Base with Effects {
  def scalaMethodCall[A: Manifest](caller: Rep[_], effects: Summary, methodName: String, typeArgs: List[Manifest[_]], args: Rep[_]*): Rep[A]
}

trait ScalaMethodCallOpsExp extends ScalaMethodCallOps with BaseExp with EffectExp {

  case class ScalaMethodCall[A: Manifest](caller: Rep[_], methodName: String, typeArgs: List[Manifest[_]], args: List[Rep[_]]) extends Def[A] {
    val m = manifest[A]
  }

  def scalaMethodCall[A: Manifest](caller: Rep[_], effects: Summary, methodName: String, typeArgs: List[Manifest[_]], args: Rep[_]*): Exp[A] = {
    reflectEffect(ScalaMethodCall[A](caller, methodName, typeArgs, args.toList), effects)
  }

  override def mirror[A: Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case ScalaMethodCall(caller, methodName, typeArgs, args) => ScalaMethodCall[A](f(caller), methodName, typeArgs, args.map(f(_)))
    case Reflect(ScalaMethodCall(caller, methodName, typeArgs, args), u, es) =>
      reflectMirrored(Reflect(ScalaMethodCall[A](f(caller), methodName, typeArgs, args.map(f(_))), mapOver(f, u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e, f)
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case ScalaMethodCall(caller, methodName, typeArgs, args) if addControlDeps => syms(caller) ::: syms(args)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ScalaMethodCall(caller, methodName, typeArgs, args) => effectSyms(caller) ::: effectSyms(args)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ScalaMethodCall(caller, methodName, typeArgs, args) => freqNormal(caller) ::: freqHot(args)
    case _ => super.symsFreq(e)
  }
}

trait ScalaGenMethodCallOps extends ScalaGenBase {
  val IR: ScalaMethodCallOpsExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ScalaMethodCall(caller, methodName, typeArgs, args) =>
      val argString = if (args.isEmpty) "" else src"($args)"
      val rhs1 = src"$caller.$methodName$argString"
      emitTypedValDef(sym, rhs1)
    case _ => super.emitNode(sym, rhs)
  }
}
