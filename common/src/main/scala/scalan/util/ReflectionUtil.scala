package scalan.util

import scala.reflect.runtime.universe._

object ReflectionUtil {
  def typeSymbol[A: TypeTag] = typeOf[A].typeSymbol

  def annotation[T: TypeTag](symbol: Symbol) = symbol.annotations.find {
    _.tpe =:= typeOf[T]
  }
}
