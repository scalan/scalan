package scalan

import scalan.common.Default
import scala.reflect.runtime.universe._

/**
 * Created by slesarenko on 17/01/15.
 */

trait BaseTypes extends Base { self: Scalan =>

  trait BaseTypeEx[TBase, TExt] extends Reifiable[TExt] {
    def value: Rep[TBase]
  }

  class BaseElemEx[TBase, TExt](extE: =>Elem[TExt])
                               (implicit override val tag: TypeTag[TBase], z: Default[TBase])
    extends BaseElem[TBase] {
    def extElem = extE
  }

  trait ExCompanion0[TBase] {
    def defaultVal: Default[TBase]
  }
}
