package scalan.compilation.lms

import java.lang.reflect.Method

import scalan.compilation.language.ScalaInterpreter
import scalan.{ScalanCommunityDslExp, CommunityMethodMappingDSL}

trait CommunityBridgeScala extends CommunityBridge with CommunityMethodMappingDSL with ScalaInterpreter { self: ScalanCommunityDslExp =>

  override def transformMethodCall[T](m: LmsMirror, receiver: Exp[_], method: Method, args: List[AnyRef]): lms.Exp[_] = {
    import lms.EffectId._

    mappedFunc(method) match {
      case Some(conf: ScalaMappingDSL#ScalaFunc) => conf.lib match {
        case e: ScalaMappingDSL#ScalaLib =>
          val param = conf.wrapper match {
            case true => Seq(m.symMirrorUntyped(receiver))
            case false => Seq.empty[lms.Exp[_]]
          }
          val methodName = conf.funcName.name match {
            case n: String if n.isEmpty => n
            case _ => e.pack + "." + conf.funcName.name
          }
          Manifest.classType(method.getDeclaringClass) match {
            case (mA: Manifest[a]) =>
              val lmsArgs = param ++ args.collect { case v: Exp[_] => m.symMirrorUntyped(v) }
              lms.scalaMethod[a](null, PURE, methodName, List.empty, lmsArgs: _*)(mA.asInstanceOf[Manifest[a]])
          }
        case e: ScalaMappingDSL#EmbeddedObject if e.name == "lms" =>
          val obj = m.symMirrorUntyped(receiver)
          val name = conf.funcName.name
          import scala.reflect.runtime.universe._
          val instanceMirror = runtimeMirror(obj.getClass.getClassLoader).reflect(lms)
          val lmsMethod = instanceMirror.symbol.typeSignature.member(newTermName(name))
          instanceMirror.reflectMethod(lmsMethod.asMethod).apply(obj, createManifest(receiver.elem)).asInstanceOf[lms.Exp[_]]
      }
      case Some(nonScalaFunc) =>
        !!!(s"$nonScalaFunc is not a ScalaMappingDSL#ScalaFunc")
      case None =>
        val obj = m.symMirrorUntyped(receiver)
        Manifest.classType(method.getDeclaringClass) match {
          case (mA: Manifest[a]) => lms.scalaMethod[a](obj, PURE, method.getName,
            args.collect { case elem: Element[_] => elem.tag },
            /* filter out implicit ClassTag params */
            args.collect { case v: Exp[_] => m.symMirrorUntyped(v) }: _*)(mA.asInstanceOf[Manifest[a]])
        }
    }
  }

  override def transformDef[T](m: LmsMirror, g: AstGraph, sym: Exp[T], d: Def[T]) = d match {
    case u: scalan.collections.impl.CollectionsExp#ExpCollectionOverArray[_] =>
      val exp = Manifest.classType(u.getClass) match {
        case (mA: Manifest[a]) =>
          lms.newObj[a]("scalan.imp.ArrayImp", Seq(m.symMirrorUntyped(u.arr.asInstanceOf[Exp[_]])), true)(mA)
      }
      m.addSym(sym, exp)
    case _ => super.transformDef(m, g, sym, d)
  }
}
