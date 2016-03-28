package scalan.compilation.lms.common

import scala.lms.common._
import scala.lms.internal.Effects
import scala.reflect.SourceContext

trait ObjectOrientedOps extends Base with Effects {
  def methodCall[A: Manifest](caller: Rep[_], effects: Summary, methodName: String, typeArgs: List[Manifest[_]], args: Rep[_]*): Rep[A]

  def newObj[A: Manifest](className: String, args: Seq[Any], newKeyWord: Boolean): Rep[A]
}

trait ObjectOrientedOpsExp extends ObjectOrientedOps with BaseExp with EffectExp {

  case class MethodCall[A: Manifest](caller: Rep[_], methodName: String, typeArgs: List[Manifest[_]], args: List[Rep[_]]) extends Def[A] {
    val m = manifest[A]
  }

  def methodCall[A: Manifest](caller: Rep[_], effects: Summary, methodName: String, typeArgs: List[Manifest[_]], args: Rep[_]*): Exp[A] = {
    reflectEffect(MethodCall[A](caller, methodName, typeArgs, args.toList), effects)
  }

  case class NewObj[A: Manifest](className: String, args: Seq[Any], newKeyWord: Boolean) extends Def[A] {
    val m = manifest[A]
  }

  def newObj[A: Manifest](className: String, args: Seq[Any], newKeyWord: Boolean): Exp[A] = {
    NewObj[A](className, args, newKeyWord)
  }

  override def mirror[A: Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case MethodCall(caller, methodName, typeArgs, args) => MethodCall[A](f(caller), methodName, typeArgs, args.map(f(_)))
    case Reflect(MethodCall(caller, methodName, typeArgs, args), u, es) =>
      reflectMirrored(Reflect(MethodCall[A](f(caller), methodName, typeArgs, args.map(f(_))), mapOver(f, u), f(es)))(mtype(manifest[A]), pos)
    case NewObj(className, args, newKeyWord) =>
      val newArgs = args.map {
        case arg: Exp[_] => f(arg)
        case arg => arg
      }
      newObj(className, newArgs, newKeyWord)(mtype(manifest[A])) // TODO check: Why not use the manifest from the NewObj
    case _ => super.mirror(e, f)
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case MethodCall(caller, methodName, typeArgs, args) if addControlDeps => syms(caller) ::: syms(args)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case MethodCall(caller, methodName, typeArgs, args) => effectSyms(caller) ::: effectSyms(args)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case MethodCall(caller, methodName, typeArgs, args) => freqNormal(caller) ::: freqHot(args)
    case _ => super.symsFreq(e)
  }

}

trait ScalaGenObjectOrientedOps extends ScalaGenBase {
  val IR: ObjectOrientedOpsExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MethodCall(caller, methodName, typeArgs, args) =>
      // TODO caller may be equal to null, see CoreBridgeScala; fix this (also in CxxShptrGenObjectOpsExp)
      val callerString = if (caller != null) src"$caller." else ""
      val argString = if (args.isEmpty) "" else src"($args)"
      val rhs1 = callerString + methodName + argString
      // Typed to allow type parameter inference
      emitTypedValDef(sym, rhs1)
    case NewObj(className, args, newKeyWord) =>
      val newStr = newKeyWord match {
        case true => "new "
        case false => ""
      }
      val argsStr = args.map {
        case e: Exp[_] => quote(e)
        // TODO handle standard implicit arguments (Numeric/Ordering/Manifest/etc.)
        case arg => arg
      }.mkString(",")
      emitValDef(sym, src"$newStr$className($argsStr)")
    case _ => super.emitNode(sym, rhs)
  }
}
