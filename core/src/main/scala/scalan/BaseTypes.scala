package scalan

/**
 * Created by slesarenko on 17/01/15.
 */

trait BaseTypes { self: Scalan =>

  trait BaseTypeEx[TBase, TExt] extends Reifiable[TExt] {
    def value: Rep[TBase]
  }

}
