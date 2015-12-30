package scalan.primitives

import scalan._

trait StructItems extends ViewsDsl with Entities  { self: StructsDsl with Scalan =>

  trait StructItem[Val] extends Def[StructItem[Val]] {
    def eVal: Elem[Val]
    def key: Rep[StructKey]
    def value: Rep[Val]
  }

  abstract class StructItemBase[Val]
        (val key: Rep[StructKey], val value: Rep[Val])
        (implicit val eVal: Elem[Val])
    extends StructItem[Val]

}
