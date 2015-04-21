package scalan

import java.lang.reflect.Method

import scalan.common.Default
import scala.reflect.runtime.universe._
import scalan.compilation.{GraphVizConfig, GraphVizExport}

/**
 * Created by slesarenko on 17/01/15.
 */

trait TypeWrappers extends Base { self: Scalan =>

  trait TypeWrapper[TBase, TExt] extends Reifiable[TExt] {
    def wrappedValueOfBaseType: Rep[TBase]
  }

  class BaseElemEx[TBase, TExt](extE: =>Elem[TExt])
                               (implicit override val tag: WeakTypeTag[TBase], z: Default[TBase])
    extends BaseElem[TBase] { self =>
    def getWrapperElem = extE
    override protected def getName = s"BT[${super.getName},${getWrapperElem.name}]"
  }

  class BaseElemEx1[A, TExt, CBase[_]]
    (extE: =>Elem[TExt])(implicit val eItem: Elem[A], val cont: Cont[CBase], z: Default[CBase[A]])
    extends BaseElemEx[CBase[A], TExt](extE)(cont.tag(eItem.tag), z) {
  }

  abstract class WrapperElem[TBase, TExt](implicit val baseElem: Elem[TBase]) extends EntityElem[TExt] {
  }

  abstract class WrapperElem1[A, To, CBase[_], CW[_]](implicit eA: Elem[A], val contBase: Cont[CBase], contW: Cont[CW])
    extends EntityElem1[A, To, CW](eA, contW) {
    def baseElem: Elem[CBase[A]] = contBase.lift(eA)
  }

  trait ExCompanion0[TBase] {
    //def defaultVal: Default[TBase]
  }

  trait ExCompanion1[TBase[_]] {
    //def defaultVal[A]: Default[TBase[A]]
  }

  final val ContainerLength = "ContainerLength"
  final val ContainerApply = "ContainerApply"

  def unwrapElem(e: Elem[_]): Elem[_] = e match {
    case ce: ConcreteElem[_,_] =>
      val iso = getIsoByElem(e)
      iso.eFrom match {
        case eBase: BaseElemEx[_,_] => ???
      }
    case ee: EntityElem[_] => ???
  }

}
 trait TypeWrappersSeq extends TypeWrappers { scalan: ScalanSeq =>
   class SeqBaseElemEx[TBase, TExt](extE: =>Elem[TExt])
                                (implicit override val tag: WeakTypeTag[TBase], z: Default[TBase])
     extends BaseElemEx[TBase, TExt](extE) {
     override protected def getDefaultRep = {
       val defaultOfWrapper = getWrapperElem.defaultRepValue.asInstanceOf[TypeWrapper[TBase, TExt]]
       defaultOfWrapper.wrappedValueOfBaseType
     }
   }
   class SeqBaseElemEx1[A, TExt, CBase[_]]
       (extE: =>Elem[TExt])
       (implicit override val eItem: Elem[A],
                 override val cont: Cont[CBase],
                              z: Default[CBase[A]])
     extends BaseElemEx1[A, TExt, CBase](extE) {
     override protected def getDefaultRep = {
       val defaultOfWrapper = getWrapperElem.defaultRepValue.asInstanceOf[TypeWrapper[CBase[A], TExt]]
       defaultOfWrapper.wrappedValueOfBaseType
     }
   }
 }

trait TypeWrappersExp extends TypeWrappers with GraphVizExport { scalan: ScalanExp =>
  class ExpBaseElemEx[TBase, TExt](extE: =>Elem[TExt])
                                  (implicit override val tag: WeakTypeTag[TBase], z: Default[TBase])
    extends BaseElemEx[TBase, TExt](extE) {
    override protected def getDefaultRep = getWrapperElem.defaultRepValue.asInstanceOf[Rep[TBase]]
  }
  class ExpBaseElemEx1[A, TExt, CBase[_]]
    (extE: =>Elem[TExt])
    (implicit override val eItem: Elem[A],
               override val cont: Cont[CBase],
                               z: Default[CBase[A]])
    extends BaseElemEx1[A, TExt, CBase](extE)
  {
    override protected def getDefaultRep = getWrapperElem.defaultRepValue.asInstanceOf[Rep[CBase[A]]]
  }

  override protected def nodeColor(sym: Exp[_])(implicit config: GraphVizConfig) = sym.elem match {
    case _: BaseElemEx[_, _] => "blue"
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
      case a: Rep[a] => a.elem.classTag.runtimeClass
      case a => a.getClass
    }

    val newMethod = eUnwrappedReceiver.getMethod(mc.method.getName, argClasses: _*)
    val newCall = mkMethodCall(unwrappedReceiver, newMethod, newArgs, true, eUnwrappedRes)
    newCall.asRep[T]
  }
}