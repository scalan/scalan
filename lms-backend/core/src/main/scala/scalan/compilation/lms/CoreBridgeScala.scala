package scalan.compilation.lms

import java.lang.reflect.Method

import scalan.compilation.language.ScalaInterpreter

trait CoreBridgeScala extends CoreBridge with ScalaInterpreter {
  override val lms: ScalaCoreLmsBackend
  import scalan._

  override def transformMethodCall[T](m: LmsMirror, receiver: Exp[_], method: Method, args: List[AnyRef], returnType: Elem[T]): lms.Exp[_] = {
    mappedFunc(method) match {
      case Some(func: ScalaMappingDSL#ScalaFunc) => func.lib match {
        case e: ScalaMappingDSL#ScalaLib =>
          val param = func.wrapper match {
            case true => Seq(m.symMirrorUntyped(receiver))
            case false => Seq.empty[lms.Exp[_]]
          }
          val methodName: String = func.name match {
            case "" => ""
            case _ =>
              e.pack match {
                case "" => func.name
                case p => p + "." + func.name
              }
          }
          elemToManifest(returnType) match {
            case (mA: Manifest[a]) =>
              val lmsArgs = param ++ args.collect { case v: Exp[_] => m.symMirrorUntyped(v) }
              lms.scalaMethodCall[a](null, lms.Pure, methodName, List.empty, lmsArgs: _*)(mA.asInstanceOf[Manifest[a]])
          }
        case e: ScalaMappingDSL#EmbeddedObject if e.name == "lms" =>
          val obj = m.symMirrorUntyped(receiver)
          val name = func.name
          val lmsMethod = lmsMemberByName(name).asMethod
          lmsMirror.reflectMethod(lmsMethod).apply(obj, elemToManifest(receiver.elem)).asInstanceOf[lms.Exp[_]]
      }
      case Some(nonScalaFunc) =>
        !!!(s"$nonScalaFunc is not a ScalaMappingDSL#ScalaFunc")
      case None =>
        val obj = m.symMirrorUntyped(receiver)
        elemToManifest(returnType) match {
          case (mA: Manifest[a]) => lms.scalaMethodCall[a](obj, lms.Pure, method.getName,
            args.collect {
              case elem: Elem[_] => elemToManifest(elem)
            },
            /* filter out implicit ClassTag params */
            args.collect { case v: Exp[_] => m.symMirrorUntyped(v) }: _*)(mA.asInstanceOf[Manifest[a]])
        }
    }
  }
}
