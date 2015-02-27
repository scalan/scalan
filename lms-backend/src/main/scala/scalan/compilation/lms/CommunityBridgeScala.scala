package scalan.compilation.lms

import java.lang.reflect.Method

import scalan.{ScalanCommunityDslExp, CommunityMethodMapping}

trait CommunityBridgeScala extends CommunityBridge with CommunityMethodMapping { self: ScalanCommunityDslExp =>
  override def transformMethodCall[T](symMirr: SymMirror, receiver: Exp[_], method: Method, args: List[AnyRef]): lms.Exp[_] = {
    import lms.EffectId._

    val obj = symMirr(receiver.asInstanceOf[Exp[_]])

    getFunc(method) match {
      case Some(conf: ScalaLanguage#ScalaFunc) => conf.lib match {
        case e: ScalaLanguage#ScalaLib =>
          //              if (!e.jar.isEmpty) extensionsJars += e.jar
          Manifest.classType(method.getDeclaringClass) match {
            case (mA: Manifest[a]) => lms.scalaMethod[a](null, PURE, e.pack + "." + conf.funcName.name, List.empty, List(obj) ++ args.map(v => symMirr(v.asInstanceOf[Exp[_]])): _*)(mA)
          }
        case e: ScalaLanguage#EmbeddedObject if e.name == "lms" =>
          val name = conf.funcName.name
          import scala.reflect.runtime.universe._
          val m = runtimeMirror(obj.getClass.getClassLoader).reflect(lms)
          val lmsMethod = m.symbol.typeSignature.member(newTermName(name))
          m.reflectMethod(lmsMethod.asMethod).apply(obj, createManifest(receiver.asInstanceOf[Exp[_]].elem)).asInstanceOf[lms.Exp[_]]
      }
      case None =>
        Manifest.classType(method.getDeclaringClass) match {
          case (mA: Manifest[a]) => lms.scalaMethod[a](obj, PURE, method.getName,
            args.filter(_.isInstanceOf[Element[_]]).map(_.asInstanceOf[Element[_]].tag),
            args
              /* filter out implicit ClassTag params */ .filter(_.isInstanceOf[Exp[_]])
              .map(v => symMirr(v.asInstanceOf[Exp[_]])): _*)(mA)
        }
    }
  }
}
