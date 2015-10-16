package scalan

import java.lang.reflect.Method

import scalan.common.Default
import scala.reflect.runtime.universe._
import scalan.compilation.{GraphVizConfig, GraphVizExport}

/**
 * Created by slesarenko on 17/01/15.
 */

trait TypeWrappers extends Base { self: Scalan =>

  trait TypeWrapper[TBase, TWrapper] extends Reifiable[TWrapper] {
    def wrappedValue: Rep[TBase]
  }

  class BaseTypeElem[TBase, TWrapper <: TypeWrapper[TBase, TWrapper]]
    (extE: =>Elem[TWrapper])(implicit tag: WeakTypeTag[TBase], z: Default[TBase])
    extends BaseElem[TBase] { self =>
    lazy val wrapperElem = extE
    override protected def getDefaultRep = {
      val wrapperDefaultValue = wrapperElem.defaultRepValue
      unwrapTypeWrapperRep(wrapperDefaultValue)
    }
    override protected def getName = s"${super.getName}{base type, wrapper: ${wrapperElem.name}}"
  }

  class BaseTypeElem1[A, CBase[_], TWrapper <: TypeWrapper[CBase[A], TWrapper]]
    (extE: =>Elem[TWrapper])(implicit val eItem: Elem[A], val cont: Cont[CBase], z: Default[CBase[A]])
    extends BaseTypeElem[CBase[A], TWrapper](extE)(cont.tag(eItem.tag), z)

  protected def unwrapTypeWrapperRep[TBase, TWrapper](x: Rep[TypeWrapper[TBase, TWrapper]]): Rep[TBase]

  abstract class WrapperElem[TBase, TWrapper](val baseElem: Elem[TBase]) extends EntityElem[TWrapper] {
    def eTo: Elem[_]
  }

  abstract class WrapperElem1[A, Abs, CBase[_], CW[_]](eA: Elem[A], contBase: Cont[CBase], contW: Cont[CW])
    extends EntityElem1[A, Abs, CW](eA, contW) {
    def baseElem: Elem[CBase[A]] = contBase.lift(eA)
    def eTo: Elem[_]
  }

  trait ExCompanion0[TBase] {
    //def defaultVal: Default[TBase]
  }

  trait ExCompanion1[TBase[_]] {
    //def defaultVal[A]: Default[TBase[A]]
  }

  final val ContainerLength = "ContainerLength"
  final val ContainerApply = "ContainerApply"

  def isValueAccessor(m: Method) = m.getName == "wrappedValue"

  def isWrapperElem(el: Elem[_]) = el match {
    case el: WrapperElem[_,_] => true
    case el: WrapperElem1[_,_,_,_] => true
    case _ => false
  }
}

trait TypeWrappersSeq extends TypeWrappers { scalan: ScalanSeq =>
  protected def unwrapTypeWrapperRep[TBase, TWrapper](x: Rep[TypeWrapper[TBase, TWrapper]]): Rep[TBase] =
    x.wrappedValue
 }

trait TypeWrappersExp extends TypeWrappers with GraphVizExport { scalan: ScalanExp =>
  protected def unwrapTypeWrapperRep[TBase, TWrapper](x: Rep[TypeWrapper[TBase, TWrapper]]): Rep[TBase] =
    x.asInstanceOf[Rep[TBase]]

  override protected def nodeColor(sym: Exp[_])(implicit config: GraphVizConfig) = sym.elem match {
    case _: BaseTypeElem[_, _] => "blue"
    case _ => super.nodeColor(sym)
  }

  def unwrapSyms(syms: List[AnyRef]): List[AnyRef] = {
    syms.map {
      case obj if !obj.isInstanceOf[Rep[_]] => obj
      case HasViews(s, iso) => s
      case s => s
    }
  }

  def unwrapMethodCall[T](mc: MethodCall, unwrappedReceiver: Rep[_], eUnwrappedRes: Elem[T]): Rep[T] = {
    val eUnwrappedReceiver = unwrappedReceiver.elem
    val newArgs = unwrapSyms(mc.args)
    val argClasses = newArgs.map {
      case a: Rep[a] => a.elem.runtimeClass
      case a => a.getClass
    }

    // keep unwrapped method because real method may have different signature
    // moreover it is not necessary exists
    // this should be handled in Backend using config
    val newCall = mkMethodCall(unwrappedReceiver, mc.method, newArgs, true, eUnwrappedRes)
    newCall.asRep[T]
  }
  def unwrapNewObj[T](clazz: Class[T], args: List[AnyRef], neverInvoke: Boolean, eUnwrappedRes: Elem[T]): Rep[T] = {
    val newArgs = unwrapSyms(args)
    val newObj = new NewObject[T](clazz, newArgs, neverInvoke)(eUnwrappedRes)
    newObj
  }
}