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
      case Some((libraryT, typeT, methodT)) =>
        elemToManifest(returnType) match {
          case mA: Manifest[a] =>
            val lmsReceiver = m.symMirrorUntyped(receiver)
            val lmsArgs = adjustArgs(m, args, methodT.argOrder)
            // TODO For now assume inference is enough here
            val typeArgs = Nil
            lms.cxxMethodCall[a](lmsReceiver, lms.Pure, methodT.mappedName, typeArgs, lmsArgs: _*)(mA.asInstanceOf[Manifest[a]])
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
