package scalan.compilation.lms.cxx.sharedptr

import java.lang.reflect.Method

import scalan.compilation.language.CXX
import scalan.compilation.language.CxxMapping.{CxxMethod, CxxType, CxxLibrary}
import scalan.compilation.lms.{MethodCallBridge, CoreLmsBackend, CoreBridge}

trait CoreBridgeCxx extends CoreBridge with MethodCallBridge[CxxLibrary, CxxType, CxxMethod] {
  override val lms: CoreLmsBackend with CxxMethodCallOpsExp
  import scalan._

  val languageId = CXX

  override def transformMethodCall[T](m: LmsMirror, receiver: Exp[_], method: Method, args: List[AnyRef], returnType: Elem[T]): lms.Exp[_] = {
    mappedMethod(method) match {
      case Some((libraryT, typeT, methodT)) => func.lib match {
        case e: CppMappingDSL#CppLib =>
//          val param = func.wrapper match {
//            case true => Seq(m.symMirrorUntyped(receiver))
//            case false => Seq.empty[lms.Exp[_]]
//          }
          elemToManifest(returnType) match {
            case (mA: Manifest[a]) =>
              val lmsArgs = /*param ++ */args.collect { case v: Exp[_] => m.symMirrorUntyped(v) }
              lms.cxxMethodCall[a](null, lms.Pure, func.name, List.empty, lmsArgs: _*)(mA.asInstanceOf[Manifest[a]])
          }
//        case e: CppMappingDSL#EmbeddedObject if e.name == "lms" =>
//          val obj = m.symMirrorUntyped(receiver)
//          val name = func.name
//          val lmsMethod = lmsMemberByName(name).asMethod
//          lmsMirror.reflectMethod(lmsMethod).apply(obj, elemToManifest(receiver.elem)).asInstanceOf[lms.Exp[_]]
      }
      case None =>
        val obj = m.symMirrorUntyped(receiver)
        elemToManifest(returnType) match {
          case mA: Manifest[a] => lms.cxxMethodCall[a](obj, lms.Pure, method.getName,
            args.collect {
              case elem: Elem[_] => lms.TypeArg(elemToManifest(elem))
            },
            /* filter out implicit ClassTag params */
            args.collect { case v: Exp[_] => m.symMirrorUntyped(v) }: _*)(mA.asInstanceOf[Manifest[a]])
        }
    }
  }
}
