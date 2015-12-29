package scalan.compilation.lms

import java.lang.reflect.Method

import scalan.compilation.language.SCALA
import scalan.compilation.language.ScalaMapping.{ScalaMethod, ScalaType, ScalaLibrary}

trait CoreBridgeScala extends CoreBridge with MethodCallBridge[ScalaLibrary, ScalaType, ScalaMethod] {
  override val lms: ScalaCoreLmsBackend
  import scalan._

  val languageId = SCALA

  override def transformMethodCall[T](m: LmsMirror, receiver: Exp[_], method: Method, args: List[AnyRef], returnType: Elem[T]): lms.Exp[_] = {
    mappedMethod(method) match {
      case Some((libraryT, typeT, methodT)) => libraryT match {
        case e: ScalaMappingDSL#ScalaLib =>
          val param = methodT.wrapper match {
            case true => Seq(m.symMirrorUntyped(receiver))
            case false => Seq.empty[lms.Exp[_]]
          }
          val methodName: String = methodT.name match {
            case "" => ""
            case _ =>
              e.pack match {
                case "" => methodT.name
                case p => p + "." + methodT.name
              }
          }
          elemToManifest(returnType) match {
            case (mA: Manifest[a]) =>
              val lmsArgs = param ++ args.collect { case v: Exp[_] => m.symMirrorUntyped(v) }
              lms.scalaMethodCall[a](null, lms.Pure, methodName, List.empty, lmsArgs: _*)(mA.asInstanceOf[Manifest[a]])
          }
        case e: ScalaMappingDSL#EmbeddedObject if e.name == "lms" =>
          val obj = m.symMirrorUntyped(receiver)
          val name = methodT.name
          val lmsMethod = lmsMemberByName(name).asMethod
          // FIXME why only elemToManifest(receiver.elem)?
          lmsMirror.reflectMethod(lmsMethod).apply(obj, elemToManifest(receiver.elem)).asInstanceOf[lms.Exp[_]]
      }
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
