package scalan

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

package object frontend {

  /**
   * Wrap `block` in Rep
   */
  def rep[T](block: T): T = macro RepMacro.repImpl[T]

  object RepMacro {
    def repImpl[T](c: blackbox.Context)(block: c.Expr[T]): c.Expr[T] = {
      import c._

      block
    }
  }
}
