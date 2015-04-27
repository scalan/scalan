package scala.reflect

import runtime.universe._
/**
 * Methods which need to access private[scala.reflect] API
 */
object ReflectionUtil0 {
  private val universe = runtime.universe.asInstanceOf[runtime.JavaMirrors]
  private val mirror = universe.runtimeMirror(getClass.getClassLoader)

  def methodToJava(sym: MethodSymbol) = mirror.methodToJava(sym.asInstanceOf[universe.MethodSymbol])
}
