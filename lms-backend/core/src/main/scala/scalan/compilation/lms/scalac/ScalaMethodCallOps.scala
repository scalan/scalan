package scalan.compilation.lms.scalac

import scala.lms.common._
import scala.lms.internal.{Expressions, Effects}
import scala.reflect.SourceContext
import scalan.compilation.lms.{GenMethodCallOps, MethodCallOpsExp}

trait ScalaMethodCallOps extends Base with Effects {
  sealed trait Receiver
  case class Static(name: String) extends Receiver
  case class Obj(obj: Rep[_]) extends Receiver

  def scalaMethodCall[A: Manifest](receiver: Receiver, effects: Summary, methodName: String, typeArgs: List[Manifest[_]], args: Any*): Rep[A]

  def scalaNewObj[A: Manifest](args: Any*): Rep[A]
}

trait ScalaMethodCallOpsExp extends ScalaMethodCallOps with MethodCallOpsExp {

  case class ScalaMethodCall[A: Manifest](receiver: Receiver, methodName: String, typeArgs: List[Manifest[_]], args: List[Any]) extends Def[A] {
    val m = manifest[A]
  }

  case class ScalaNewObj[A](m: Manifest[A], args: List[Any]) extends Def[A]

  def scalaMethodCall[A: Manifest](receiver: Receiver, effects: Summary, methodName: String, typeArgs: List[Manifest[_]], args: Any*): Exp[A] = {
    reflectEffect(ScalaMethodCall[A](receiver, methodName, typeArgs, args.toList), effects)
  }

  def scalaNewObj[A: Manifest](args: Any*): Rep[A] =
    reflectEffect(ScalaNewObj(manifest[A], args.toList), Alloc)

  def transformReceiver(f: Transformer, receiver: Receiver) = receiver match {
    case Static(_) => receiver
    case Obj(x) => Obj(f(x))
  }

  override def mirror[A: Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case ScalaMethodCall(receiver, methodName, typeArgs, args) => ScalaMethodCall[A](transformReceiver(f, receiver), methodName, typeArgs, transformAny(f, args))
    case ScalaNewObj(m, args) => ScalaNewObj[A](mtype(m), transformAny(f, args))
    case Reflect(ScalaMethodCall(receiver, methodName, typeArgs, args), u, es) =>
      reflectMirrored(Reflect(ScalaMethodCall[A](transformReceiver(f, receiver), methodName, typeArgs, transformAny(f, args)), mapOver(f, u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(ScalaNewObj(m, args), u, es) =>
      reflectMirrored(Reflect(ScalaNewObj[A](mtype(m), transformAny(f, args)), mapOver(f, u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e, f)
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case ScalaMethodCall(receiver, methodName, typeArgs, args) if addControlDeps => syms(receiver) ::: syms(args)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ScalaMethodCall(receiver, methodName, typeArgs, args) => effectSyms(receiver) ::: effectSyms(args)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ScalaMethodCall(receiver, methodName, typeArgs, args) => freqNormal(receiver) ::: freqHot(args)
    case _ => super.symsFreq(e)
  }
}

trait ScalaGenMethodCallOps[BackendType <: Expressions with Effects with ScalaMethodCallOpsExp] extends ScalaGenBase with GenMethodCallOps[BackendType] {
  import IR._

  override def quoteOrRemap(arg: Any) = arg match {
    case Static(name) => name
    case Obj(obj) => quoteOrRemap(obj)
    case _ => super.quoteOrRemap(arg)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ScalaMethodCall(receiver, methodName, typeArgs, args) =>
      val argString = if (args.isEmpty) "" else src"($args)"
      val rhs1 = src"$receiver.$methodName$argString"
      emitTypedValDef(sym, rhs1)
    case ScalaNewObj(m, args) =>
      val argString = if (args.isEmpty) "" else src"($args)"
      val rhs1 = src"new $m$argString"
      emitTypedValDef(sym, rhs1)
    case _ => super.emitNode(sym, rhs)
  }
}
