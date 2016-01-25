package scalan.compilation.lms

import java.lang.reflect.Method

import scala.lms.common.{EffectExp, BaseExp}
import scala.lms.internal.{Effects, Expressions}
import scalan.compilation.language._
import scalan.compilation.lms.common.TransformingExt
import scalan.util.ScalaNameUtil

trait MethodCallBridge[LibraryT, TypeT <: TypeRep[MethodT], MethodT <: MethodRep] extends LmsBridge with MethodMappingDSL {
  override val lms: LmsBackend with MethodCallOpsExp
  import scalan._

  def mappedMethod(method: Method): Option[(LibraryT, TypeT, MethodT)] =
    methodReplaceConf.map { mapping =>
      mapping.getMethod(method)
    }.collectFirst { case Some(x) => x }

  def mappedType(m: Manifest[_]): Option[(LibraryT, TypeT)] = {
    methodReplaceConf.map { mapping =>
      mapping.getType(m)
    }.collectFirst { case Some(x) => x }
  }

  val languageId: LanguageId

  lazy val methodReplaceConf = mappingDSLs.getOrElse(languageId, Nil).asInstanceOf[Seq[Mapping[LibraryT, TypeT, MethodT]]]

  def adjustArgs(m: LmsMirror, args: Seq[Any], shuffle: Seq[Adjusted[Int]]) = shuffle.map { adjIndex =>
    val arg = args(adjIndex.value)
    val lmsArg = mapParam(m, arg, false)
    adjIndex.adjustment match {
      case None => lmsArg
      case Some(adjustment) => lms.adjust(adjustment, lmsArg)
    }
  }

  def transformMethodCall[T](m: LmsMirror, receiver: Exp[_], method: Method, args: List[AnyRef], returnType: Elem[T]): lms.Exp[_] =
    !!!(s"Don't know how to transform method call: $method")

  def newObj[A](m: Manifest[A], args: Seq[Any]): lms.Exp[A] = {
    val name = mappedType(m) match {
      case Some((_, typeT)) => typeT.mappedName
      case None => ScalaNameUtil.cleanNestedClassName(m.runtimeClass.getName)
    }
    lms.newObj[A](name, args, true)(m)
  }

  override protected def transformDef[T](m: LmsMirror, g: AstGraph, sym: Exp[T], d: Def[T]) = d match {
    case mc@MethodCall(receiver, method, args, _) =>
      val exp = (isWrapperElem(receiver.elem) && isWrappedValueAccessor(method)) match {
        case true  => m.symMirror[T](receiver)
        case false => transformMethodCall[T](m, receiver, method, args, mc.selfType.asInstanceOf[Elem[T]])
      }

      m.addSym(sym, exp)

    case lr@NewObject(eA, args, _) =>
      elemToManifest(eA) match {
        case mA: Manifest[a] =>
          // TODO handle case when some of params are functions
          val lmsArgs = args.map(mapParam(m, _, false))
          val exp = newObj[a](mA, lmsArgs)
          m.addSym(sym, exp)
      }

    case _ => super.transformDef(m, g, sym, d)
  }
}

trait MethodCallOpsExp extends BaseExp with EffectExp with TransformingExt {
  /** Applies the given adjustment to the given value. By default just passes through so it can be used in codegen.
    * Override to transform Exps/Manifests/etc. */
  def adjust(adjustment: Adjustment, value: Any): Any = adjustment(value)
}

trait GenMethodCallOps[BackendType <: Expressions with Effects with MethodCallOpsExp, LibraryT, TypeT <: TypeRep[MethodT], MethodT <: MethodRep] extends BaseCodegen[BackendType] {
  def mappings: MethodCallBridge[LibraryT, TypeT, MethodT]

  def adjust(adjustment: Adjustment, value: Any): Any = IR.adjust(adjustment, value)
  def adjust(adjustmentOpt: Option[Adjustment], value: Any): Any = adjustmentOpt match {
    case None => value
    case Some(adjustment) => adjust(adjustment, value)
  }

  override def quoteOrRemap(arg: Any) = arg match {
    case Adjusted(value, None) => quoteOrRemap(value)
    case Adjusted(value, Some(adj)) => quoteOrRemap(adjust(adj, value))
    case _ => super.quoteOrRemap(arg)
  }
}