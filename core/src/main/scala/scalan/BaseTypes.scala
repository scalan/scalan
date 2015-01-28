package scalan

import scalan.common.Default
import scala.reflect.runtime.universe._
import scalan.compilation.GraphVizExport

/**
 * Created by slesarenko on 17/01/15.
 */

trait BaseTypes extends Base { self: Scalan =>

  trait BaseTypeEx[TBase, TExt] extends Reifiable[TExt] {
    def wrappedValueOfBaseType: Rep[TBase]
  }

  class BaseElemEx[TBase, TExt](extE: =>Elem[TExt])
                               (implicit override val tag: WeakTypeTag[TBase], z: Default[TBase])
    extends BaseElem[TBase] { self =>
    def getWrapperElem = extE
    override def getName = s"BT[${super.getName},${getWrapperElem.name}]"
    override def getDefaultRep = getWrapperElem.getDefaultRep.asInstanceOf[Default[Rep[TBase]]]
  }

  trait ExCompanion0[TBase] {
    //def defaultVal: Default[TBase]
  }

  trait ExCompanion1[TBase[_]] {
    //def defaultVal[A]: Default[TBase[A]]
  }
}
 trait BaseTypesSeq extends BaseTypes { scalan: ScalanSeq =>
   class SeqBaseElemEx[TBase, TExt](extE: =>Elem[TExt])
                                (implicit override val tag: WeakTypeTag[TBase], z: Default[TBase])
     extends BaseElemEx[TBase, TExt](extE) {
     override def getDefaultRep = {
       val defaultOfWrapper = getWrapperElem.getDefaultRep.asInstanceOf[Default[BaseTypeEx[TBase, TExt]]]
       Default.defaultVal(defaultOfWrapper.value.wrappedValueOfBaseType)
     }
   }
 }

trait BaseTypesExp extends BaseTypes with GraphVizExport { scalan: ScalanExp =>
  class ExpBaseElemEx[TBase, TExt](extE: =>Elem[TExt])
                                  (implicit override val tag: WeakTypeTag[TBase], z: Default[TBase])
    extends BaseElemEx[TBase, TExt](extE)
  {

  }

  override protected def nodeColor(sym: Exp[_]) = sym.elem match {
    case _: BaseElemEx[_, _] => "blue"
    case _ => super.nodeColor(sym)
  }
}