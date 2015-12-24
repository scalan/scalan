package scalan.compilation.lms

import java.lang.reflect.Method

import scalan.compilation.language.{CoreMethodMappingDSL, LanguageId}

trait ObjectOrientedBridge extends LmsBridge with CoreMethodMappingDSL {
  import scalan._

  def mappedFunc(method: Method): Option[MappingTags#Fun] =
    methodReplaceConf.map(_.get(method.getDeclaringClass.getName, method.getName)).reverse.find(_.isDefined).getOrElse(None)

  def mappedClassName(c: Class[_]): Option[String] = {
    var fun: Option[String] = None
    methodReplaceConf.foreach(languageBackend => {
      val f = languageBackend.classMap.get(c)
      f match {
        case Some(func) => fun = Some(func.name)
        case _ =>
      }
    })
    fun
  }

  val languageId: LanguageId

  def methodReplaceConf = mappingDSLs(languageId)

  def transformMethodCall[T](m: LmsMirror, receiver: Exp[_], method: Method, args: List[AnyRef], returnType: Elem[T]): lms.Exp[_] = {
    elemToManifest(returnType) match {
      case mA: Manifest[a] =>
        mappedFunc(method) match {
          case Some(func) =>
            mappedMethodCall(m, receiver, func, args, mA)
          case None =>
            val lmsReceiver = m.symMirrorUntyped(receiver)
            val typeArgs = args.collect { case elem: Elem[_] => elemToManifest(elem) }
            val lmsArgs = args.collect { case v: Exp[_] => m.symMirrorUntyped(v) }
            lms.methodCall[a](lmsReceiver, lms.Pure, method.getName, typeArgs, lmsArgs: _*)(mA)
        }
    }
  }

  def mappedMethodCall[A](m: LmsMirror, receiver: Exp[_], func: MappingTags#Fun, args: List[AnyRef], mReturn: Manifest[A]): lms.Exp[_]

  def newObj[A: Manifest](args: Seq[Any], newKeyWord: Boolean): lms.Exp[A] = {
    val aClass = manifest[A].runtimeClass
    val name = mappedClassName(aClass).getOrElse(aClass.getName)
    lms.newObj[A](name, args, newKeyWord)
  }

  override protected def transformDef[T](m: LmsMirror, g: AstGraph, sym: Exp[T], d: Def[T]) = d match {
    case mc@MethodCall(receiver, method, args, _) =>
      val exp = (isWrapperElem(receiver.elem) && isValueAccessor(method)) match {
        case true  => m.symMirror[T](receiver)
        case false => transformMethodCall[T](m, receiver, method, args, mc.selfType.asInstanceOf[Elem[T]])
      }

      m.addSym(sym, exp)

    case lr@NewObject(eA, args, _) =>
      elemToManifest(eA) match {
        case mA: Manifest[a] =>
          // TODO handle case when some of params are functions
          val lmsArgs = args.map(mapParam(m, _, false))
          val exp = newObj[a](lmsArgs, true)(mA)
          m.addSym(sym, exp)
      }

    case _ => super.transformDef(m, g, sym, d)
  }
}
