package scalan

import ch.epfl.yinyang.YYTransformer
import ch.epfl.yinyang.typetransformers.GenericTypeTransformer

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

package object frontend {

  /**
   * Wrap `block` in Rep
   */
  def rep[T](block: T): T = macro RepMacro.repImpl[T]

  def repYY[T](block: T): T = macro RepMacro.repYYImpl[T]

  object RepMacro {

    def repImpl[T](c: blackbox.Context)(block: c.Expr[T]): c.Expr[T] =
      SSTransformer(c)(new RepTransformer(c), 4).apply(block)

    def repYYImpl[T](c: blackbox.Context)(block: c.Expr[T]): c.Expr[T] =
      YYTransformer[c.type, T](c)(
        "scala.Option",
        new GenericTypeTransformer[c.type](c) {
          override val IRType = "R"
        },
        postProcessing = None,
        preProcessing = None,
        config = Map(
          "shallow" -> true,
          "virtualizeFunctions" -> false,
          "virtualizeVal" -> false,
          "debug" -> 0,
          "featureAnalysing" -> false,
          "ascriptionTransforming" -> false))(block)
  }
}
