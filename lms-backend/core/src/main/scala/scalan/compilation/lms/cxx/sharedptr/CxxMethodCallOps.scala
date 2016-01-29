package scalan.compilation.lms.cxx.sharedptr

import scala.lms.common._
import scala.lms.internal.{NestedBlockTraversal, Expressions, Effects}
import scala.reflect.SourceContext
import scalan.compilation.language.Adjusted
import scalan.compilation.language.CxxMapping.{CxxMethod, CxxType, CxxLibrary}
import scalan.compilation.lms.{GenMethodCallOps, MethodCallOpsExp}

trait CxxMethodCallOps extends Base with Effects {
  // see http://en.cppreference.com/w/cpp/language/template_parameters
  sealed trait TemplateArg
  object NullPtrArg extends TemplateArg
  case class TypeArg(m: Manifest[_]) extends TemplateArg
  case class IntegralArg[A: Integral](value: A) extends TemplateArg
  case class PointerArg[A](target: Rep[A]) extends TemplateArg
  // LValueRefArg, MemberPointerArg, EnumerationArg not needed yet

  def cxxMethodCall[A: Manifest](caller: Rep[_], effects: Summary, methodName: String, templateArgs: Seq[Adjusted[TemplateArg]], args: Any*): Rep[A]

  def cxxNewObj[A: Manifest](templateArgs: Seq[Adjusted[TemplateArg]], args: Any*): Rep[A]
}

trait CxxMethodCallOpsExp extends CxxMethodCallOps with MethodCallOpsExp {
  case class CxxMethodCall[A](caller: Rep[_], methodName: String, templateArgs: List[Adjusted[TemplateArg]], args: List[Any])(implicit val m: Manifest[A]) extends Def[A]

  case class CxxNewObj[A](m: Manifest[A], templateArgs: List[Adjusted[TemplateArg]], args: List[Any]) extends Def[A]

  def cxxMethodCall[A: Manifest](caller: Rep[_], effects: Summary, methodName: String, templateArgs: Seq[Adjusted[TemplateArg]], args: Any*): Exp[A] = {
    reflectEffect(CxxMethodCall[A](caller, methodName, templateArgs.toList, args.toList), effects)
  }

  def cxxNewObj[A: Manifest](templateArgs: Seq[Adjusted[TemplateArg]], args: Any*): Rep[A] =
    reflectEffect(CxxNewObj(manifest[A], templateArgs.toList, args.toList), Alloc)

  override def mirror[A: Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case CxxMethodCall(caller, methodName, templateArgs, args) => CxxMethodCall[A](f(caller), methodName, transformTemplateArgs(f, templateArgs), transformAny(f, args))
    case CxxNewObj(m, templateArgs, args) => CxxNewObj[A](mtype(m), transformTemplateArgs(f, templateArgs), transformAny(f, args))
    case Reflect(CxxMethodCall(caller, methodName, templateArgs, args), u, es) =>
      reflectMirrored(Reflect(CxxMethodCall[A](f(caller), methodName, transformTemplateArgs(f, templateArgs), transformAny(f, args)), mapOver(f, u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(CxxNewObj(m, templateArgs, args), u, es) =>
      reflectMirrored(Reflect(CxxNewObj[A](mtype(m), transformTemplateArgs(f, templateArgs), transformAny(f, args)), mapOver(f, u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e, f)
  }

  def transformTemplateArg(f: Transformer, a: TemplateArg): TemplateArg = a match {
    case PointerArg(target) => PointerArg(f(target))
    case _ => a
  }
  def transformTemplateArg(f: Transformer, a: Adjusted[TemplateArg]): Adjusted[TemplateArg] =
    a.map(transformTemplateArg(f, _))

  def transformTemplateArgs(f: Transformer, as: List[TemplateArg]): List[TemplateArg] = as.map(transformTemplateArg(f, _))
  def transformTemplateArgs(f: Transformer, as: List[Adjusted[TemplateArg]])(implicit d: DummyImplicit): List[Adjusted[TemplateArg]] = as.map(transformTemplateArg(f, _))

  override def transformAny(f: Transformer, x: Any) = x match {
    case ta: TemplateArg => transformTemplateArg(f, ta)
    case _ => super.transformAny(f, x)
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    // case CxxMethodCall(caller, methodName, templateArgs, args) if addControlDeps => syms(caller) ::: syms(args)
    case Adjusted(value, _) => syms(value)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case CxxMethodCall(caller, methodName, templateArgs, args) => effectSyms(caller) ::: effectSyms(templateArgs) ::: effectSyms(args)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case CxxMethodCall(caller, methodName, templateArgs, args) => freqNormal(caller) ::: freqHot(templateArgs) ::: freqHot(args)
    case _ => super.symsFreq(e)
  }
}

trait CxxShptrGenMethodCallOps[BackendType <: Expressions with Effects with CxxMethodCallOpsExp] extends CxxShptrCodegen with GenMethodCallOps[BackendType, CxxLibrary, CxxType, CxxMethod] { self =>
  import IR._

  def quoteTemplateArg(x: TemplateArg): String = x match {
    case NullPtrArg => "nullptr"
    case TypeArg(m) => remap(m)
    case IntegralArg(value) => value.toString
    // TODO need to make sure target is generated as a global or member. Do it in emitDataStructures?
    // But forward declaration is needed as well.
    case PointerArg(target) => src"&$target"
  }
  def quoteTemplateArg(x: Adjusted[TemplateArg]): String = x match {
    case Adjusted(value, _) => quoteTemplateArg(value)
  }

  private def addTemplateArgsToGlobals(templateArgs: List[Adjusted[IR.TemplateArg]]) = templateArgs.foreach {
    case Adjusted(IR.PointerArg(target), _) => globals += target
    case _ =>
  }

  override def preprocess[A: Manifest](args: List[Sym[_]], body: Block[A], className: String): Unit = {
    super.preprocess(args, body, className)
    val traversal = new NestedBlockTraversal {
      override val IR: self.IR.type = self.IR
      import IR._

      override def traverseStm(stm: Stm) = stm match {
        case TP(lhs, rhs) =>
          // TODO what other types need to be tested? Ideally, this should happen on call to remap,
          // but that happens too late
          mappings.mappedType(lhs.tp) match {
            case Some((lib, _)) =>
              lib.headerName.foreach(headerFiles += _)
            case _ =>
          }
          rhs match {
            case mc: CxxMethodCall[_] =>
              addTemplateArgsToGlobals(mc.templateArgs)
            case no: CxxNewObj[_] =>
              addTemplateArgsToGlobals(no.templateArgs)
            case _ =>
          }
        case _ =>
      }
    }

    traversal.traverseBlock(body)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case CxxMethodCall(caller, methodName, templateArgs, args) =>
      val templateArgString =
        if (templateArgs.isEmpty) "" else s"<${templateArgs.map(quoteTemplateArg).mkString(",")}>"
      val rhs1 = src"$caller->$methodName$templateArgString($args)"
      emitValDef(sym, rhs1)
    case CxxNewObj(m, templateArgs, args) =>
      val templateArgString =
        if (templateArgs.isEmpty) "" else s"<${templateArgs.map(quoteTemplateArg).mkString(",")}>"
      val rhs1 = src"new ${remapWithoutTemplateArgs(m)}$templateArgString($args)"
      emitValDef(sym, rhs1)
    case _ => super.emitNode(sym, rhs)
  }

  override def remap[A](m: Manifest[A]) = mappings.mappedType(m) match {
    case Some((lib, tpe)) =>
      // FIXME this is too late but we don't know what types will need to be remapped in preprocess
      // see the comment there
      // lib.headerName.foreach(headerFiles += _)
      val templateArgs = tpe.templateArgOrder.map { case Adjusted(i, adjOpt) =>
        val typeArg = m.typeArguments(i)
        adjust(adjOpt, typeArg)
      }
      // pointer or reference?
      src"${lib.namespace.fold("")(_ + "::")}${tpe.mappedName}<$templateArgs>*"
    case None => super.remap(m)
  }
}

