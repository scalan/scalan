package scalan.util

import scala.reflect.{ReflectionUtil0, runtime}
import scala.reflect.runtime.universe._

object ReflectionUtil {
  def typeSymbol[A: TypeTag] = typeOf[A].typeSymbol

  def annotation[T: TypeTag](symbol: Symbol) = symbol.annotations.find {
    _.tree.tpe =:= typeOf[T]
  }

  def methodToJava(sym: MethodSymbol) = ReflectionUtil0.methodToJava(sym)

  def primaryConstructor(tpe: Type) = {
    val constructorSymbol = tpe.decl(termNames.CONSTRUCTOR)
    constructorSymbol match {
      case ctorTermSymbol: TermSymbol =>
        if (ctorTermSymbol.isOverloaded) {
          val constructors = ctorTermSymbol.alternatives
          constructors.collectFirst {
            case c: MethodSymbol if c.isPrimaryConstructor => c
          }
        } else {
          Some(ctorTermSymbol.asMethod)
        }
      case NoSymbol => None
    }
  }

  def classToSymbol(clazz: Class[_]) =
    runtimeMirror(clazz.getClassLoader).classSymbol(clazz)

  def simplifyType(tpe: Type) = {
    val tpe1 = tpe match {
      case NullaryMethodType(returnTpe) => returnTpe
      case _ => tpe
    }
    tpe1.dealias
  }
}
