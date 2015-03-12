package scalan.compilation.lms

import java.lang.reflect.Method

import scalan.compilation.language.ScalaInterpreter
import scalan.{ScalanCommunityDslExp, CommunityMethodMapping}

trait CommunityBridgeScala extends CommunityBridge with CommunityMethodMapping with ScalaInterpreter { self: ScalanCommunityDslExp =>

  override def defTransformer[T](m: LmsMirror, g: AstGraph, e: TableEntry[T]) =
    communityTransformer(m, g, e) orElse super.defTransformer(m, g, e)

  def communityTransformer[T](m: LmsMirror, fromGraph: AstGraph, tp: TableEntry[T]): DefTransformer = {
    val (exps, symMirr, funcMirr) = m
    val sym = tp.sym
    val tt: DefTransformer = {
      case ba: scalan.parrays.impl.PArraysExp#ExpBaseArray[_] =>
        val exp = Manifest.classType(ba.getClass) match {
          case (mA: Manifest[a]) => lms.newObj[a]("scalan.imp.ArrayImp", Seq(symMirr(ba.arr.asInstanceOf[Exp[_]])), true)(mA)
        }
        (exps ++ List(exp), symMirr + ((sym, exp)), funcMirr)
        }
    tt
  }

  override def transformMethodCall[T](symMirr: SymMirror, receiver: Exp[_], method: Method, args: List[AnyRef]): lms.Exp[_] = {
    import lms.EffectId._

    mappedFunc(method) match {
      case Some(conf: ScalaLanguage#ScalaFunc) => conf.lib match {
        case e: ScalaLanguage#ScalaLib =>
          val param = conf.wrapper match {
            case true => Seq(symMirr(receiver.asInstanceOf[Exp[_]]))
            case false => Seq.empty[lms.Exp[_]]
          }
          val methodName = conf.funcName.name match {
            case n : String if n.isEmpty => n
            case _ => e.pack + "." + conf.funcName.name
          }
          Manifest.classType(method.getDeclaringClass) match {
            case (mA: Manifest[a]) => lms.scalaMethod[a](null, PURE, methodName, List.empty,
              param ++ args.filter(_.isInstanceOf[Exp[_]]).map(v => symMirr(v.asInstanceOf[Exp[_]])): _*)(mA)
          }
        case e: ScalaLanguage#EmbeddedObject if e.name == "lms" =>
          val obj = symMirr(receiver.asInstanceOf[Exp[_]])
          val name = conf.funcName.name
          import scala.reflect.runtime.universe._
          val m = runtimeMirror(obj.getClass.getClassLoader).reflect(lms)
          val lmsMethod = m.symbol.typeSignature.member(newTermName(name))
          m.reflectMethod(lmsMethod.asMethod).apply(obj, createManifest(receiver.asInstanceOf[Exp[_]].elem)).asInstanceOf[lms.Exp[_]]
      }
      case None =>
        val obj = symMirr(receiver.asInstanceOf[Exp[_]])
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
