package scalan.compilation.lms

import java.lang.reflect.Method

import scalan.compilation.language._

trait MethodCallBridge[LibraryT, TypeT <: TypeRep[MethodT], MethodT <: MethodRep] extends LmsBridge with MethodMappingDSL {
  import scalan._

  def mappedMethod(method: Method): Option[(LibraryT, TypeT, MethodT)] =
    methodReplaceConf.map { mapping =>
      mapping.getMethod(method)
    }.collectFirst { case Some(x) => x}

  def mappedType(m: Manifest[_]): Option[(LibraryT, TypeT)] = {
    methodReplaceConf.map { mapping =>
      mapping.getType(m).map(typeT => (mapping.library, typeT))
    }.collectFirst { case Some(x) => x}
  }

  val languageId: LanguageId

  lazy val methodReplaceConf = mappingDSLs(languageId).asInstanceOf[Seq[Mapping[LibraryT, TypeT, MethodT]]]

  def transformMethodCall[T](m: LmsMirror, receiver: Exp[_], method: Method, args: List[AnyRef], returnType: Elem[T]): lms.Exp[_] =
    !!!(s"Don't know how to transform method call: $method")

  def newObj[A](m: Manifest[A], args: Seq[Any]): lms.Exp[A] = {
    val name = mappedType(m).getOrElse(m.runtimeClass.getName)
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
