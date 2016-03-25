package scalan.compilation.lms

import scalan.compilation.language.SCALA

trait CoreBridgeScala extends CoreBridge with ObjectOrientedBridge {
  import scalan._

  val languageId = SCALA

  override def mappedMethodCall[A](m: LmsMirror, receiver: Exp[_], func: MappingTags#Fun, args: List[AnyRef], mReturn: Manifest[A]): lms.Exp[_] = {
    func match {
      case func: ScalaMappingDSL#ScalaFunc => func.lib match {
        case e: ScalaMappingDSL#ScalaLib =>
          val param = func.wrapper match {
            case true =>
              val lmsReceiver = m.symMirrorUntyped(receiver)
              Seq(lmsReceiver)
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
          val lmsArgs = param ++ args.collect { case v: Exp[_] => m.symMirrorUntyped(v) }
          lms.methodCall[A](null, lms.Pure, methodName, List.empty, lmsArgs: _*)(mReturn)
        case e: ScalaMappingDSL#EmbeddedObject if e.name == "lms" =>
          val name = func.name
          val lmsReceiver = m.symMirrorUntyped(receiver)
          val lmsMethod = lmsMemberByName(name).asMethod
          lmsMirror.reflectMethod(lmsMethod).apply(lmsReceiver, lmsReceiver.tp).asInstanceOf[lms.Exp[_]]
      }
      case nonScalaFunc =>
        !!!(s"$nonScalaFunc is not a ScalaMappingDSL#ScalaFunc")
    }
  }
}
