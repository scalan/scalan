package scalan.compilation.lms.cxx.sharedptr

import java.lang.reflect.Method

import scalan.compilation.language.{Adjusted, CXX}
import scalan.compilation.language.CxxMapping.{CxxMethod, CxxType, CxxLibrary}
import scalan.compilation.lms.{MethodCallBridge, CoreLmsBackend, CoreBridge}

trait CoreBridgeCxx extends CoreBridge with MethodCallBridge[CxxLibrary, CxxType, CxxMethod] {
  override val lms: CoreLmsBackend with CxxMethodCallOpsExp
  import scalan._

  val languageId = CXX

  def argToTemplateArg(arg: Any): lms.TemplateArg = arg match {
    case m: Manifest[_] => lms.TypeArg(m)
    case i: Int => lms.IntegralArg(i)
    case e: lms.Exp[_] => lms.PointerArg(e)
  }

  override def transformMethodCall[T](m: LmsMirror, receiver: Exp[_], method: Method, args: List[AnyRef], returnType: Elem[T]): lms.Exp[_] = {
    mappedMethod(method) match {
      case Some((libraryT, typeT, methodT)) =>
        elemToManifest(returnType) match {
          case mA: Manifest[a] =>
            val lmsReceiver = m.symMirrorUntyped(receiver)
            val lmsArgs = adjustArgs(m, args, methodT.argOrder)
            val templateArgs = adjustArgs(m, args, methodT.templateArgOrder).map(_.map(argToTemplateArg))/*.map {
              case a: Adjusted[_] => a.map(argToTemplateArg)
              case a => Adjusted(argToTemplateArg(a))
            }*/
            lms.cxxMethodCall[a](lmsReceiver, lms.Pure, methodT.mappedName, templateArgs, lmsArgs: _*)(mA.asInstanceOf[Manifest[a]])
        }
      case None =>
        val obj = m.symMirrorUntyped(receiver)
        elemToManifest(returnType) match {
          case mA: Manifest[a] => lms.cxxMethodCall[a](obj, lms.Pure, method.getName,
            args.collect {
              case elem: Elem[_] => Adjusted(lms.TypeArg(elemToManifest(elem)))
            },
            /* filter out implicit ClassTag params */
            args.collect { case v: Exp[_] => Adjusted(m.symMirrorUntyped(v)) }: _*)(mA.asInstanceOf[Manifest[a]])
        }
    }
  }
}
