package scalan.compilation.lms.common

import scala.reflect.SourceContext
import scala.virtualization.lms.common._

trait MethodCallOps extends Base {
  import scala.reflect.runtime.universe.WeakTypeTag

  object EffectId extends Enumeration {
    type EFFECT = Value
    val PURE, SIMPLE, GLOBAL, ALLOC, CONTROL = Value
  }

  import EffectId.EFFECT

  def scalaMethod[A: Manifest](caller: Rep[_], effectName: EFFECT, methodName: String, types: List[WeakTypeTag[_]], args: Rep[_]*): Rep[A]
}

trait MethodCallOpsExp extends MethodCallOps with BaseExp with EffectExp {

  import EffectId._
  import scala.reflect.runtime.universe.WeakTypeTag

  case class ScalaMethod[A: Manifest](caller: Sym[Any], methodName: String, types: List[WeakTypeTag[_]], args: List[Rep[_]]) extends Def[A] {
    val m = manifest[A]
  }

  def scalaMethod[A: Manifest](caller: Rep[_], effectId: EFFECT, methodName: String, types: List[WeakTypeTag[_]], args: Rep[_]*): Exp[A] = {
    val effect = effectId match {
      case PURE => Pure
    }
    reflectEffect(ScalaMethod[A](caller.asInstanceOf[Sym[Any]], methodName, types, args.toList), effect)
  }

  override def mirror[A: Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case ScalaMethod(caller, methodName, types, args) => ScalaMethod[A](f(caller).asInstanceOf[Sym[Any]], methodName, types, args map (f(_)))
    case Reflect(ScalaMethod(caller, methodName, types, args), u, es) =>
      reflectMirrored(Reflect(ScalaMethod[A](f(caller).asInstanceOf[Sym[Any]], methodName, types, args map (f(_))), mapOver(f, u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e, f)
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case ScalaMethod(caller, methodName, types, args) if addControlDeps => syms(caller) ::: syms(args)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ScalaMethod(caller, methodName, types, args) => effectSyms(caller) ::: effectSyms(args)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ScalaMethod(caller, methodName, types, args) => freqNormal(caller) ::: freqHot(args)
    case _ => super.symsFreq(e)
  }
}

trait ScalaGenMethodCallOps extends ScalaGenBase {
  val IR: MethodCallOpsExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ScalaMethod(caller, methodName, types, args) =>
      stream.print( src"val $sym:${sym.tp} = " )
      caller match {
        case c: Rep[_] => stream.print(quote(caller))
        case _ =>
      }
      caller match {
        case c: Rep[_] => methodName match {
          case c: String if c.isEmpty =>
          case _ => stream.print(".")
        }
        case _ =>
      }
      methodName match {
        case c: String => stream.print(methodName)
        case _ =>
      }
      args.isEmpty match {
        case true =>
        case _ => stream.print(s"(${args map quote mkString ","})")
      }
      stream.println()
    case _ => super.emitNode(sym, rhs)
  }
}
