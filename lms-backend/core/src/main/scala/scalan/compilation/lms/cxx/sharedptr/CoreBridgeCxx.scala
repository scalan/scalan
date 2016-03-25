package scalan.compilation.lms.cxx.sharedptr

import scalan.compilation.language.CPP
import scalan.compilation.lms.{ObjectOrientedBridge, CoreBridge}

trait CoreBridgeCxx extends CoreBridge with ObjectOrientedBridge {
  import scalan._

  val languageId = CPP

  override def mappedMethodCall[A](m: LmsMirror, receiver: Exp[_], func: MappingTags#Fun, args: List[AnyRef], mReturn: Manifest[A]): lms.Exp[_] = {
    func match {
      case func: CppMappingDSL#CppFunc => func.lib match {
        case e: CppMappingDSL#CppLib =>
//          val param = func.wrapper match {
//            case true => Seq(m.symMirrorUntyped(receiver))
//            case false => Seq.empty[lms.Exp[_]]
//          }
          val lmsArgs = /*param ++ */args.collect { case v: Exp[_] => m.symMirrorUntyped(v) }
          lms.methodCall[A](null, lms.Pure, func.name, List.empty, lmsArgs: _*)(mReturn)
//        case e: CppMappingDSL#EmbeddedObject if e.name == "lms" =>
//          val obj = m.symMirrorUntyped(receiver)
//          val name = func.name
//          val lmsMethod = lmsMemberByName(name).asMethod
//          lmsMirror.reflectMethod(lmsMethod).apply(obj, elemToManifest(receiver.elem)).asInstanceOf[lms.Exp[_]]
      }
      case nonCxxFunc =>
        !!!(s"$nonCxxFunc is not a CppMappingDSL#CppFunc")
    }
  }
}
