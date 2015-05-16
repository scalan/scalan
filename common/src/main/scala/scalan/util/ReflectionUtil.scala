package scalan.util

import scala.reflect.{ReflectionUtil0, runtime}
import scala.reflect.runtime.universe._

object ReflectionUtil {
  def typeSymbol[A: TypeTag] = typeOf[A].typeSymbol

  def annotation[T: TypeTag](symbol: Symbol) = symbol.annotations.find {
    _.tree.tpe =:= typeOf[T]
  }

  def methodToJava(sym: MethodSymbol) = ReflectionUtil0.methodToJava(sym)
}
