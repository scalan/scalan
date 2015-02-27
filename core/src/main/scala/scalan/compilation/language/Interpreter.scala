package scalan.compilation.language

import java.lang.reflect.{Method => JMethod}

import _root_.scala.reflect.runtime.universe._

trait Interpreter {
  this: MethodMapping =>

  def getFunc(method: JMethod): Option[LanguageConf#Fun] = {
    methodReplaceConf.get(method.getDeclaringClass.getName, method.getName) 
  }

  def getType[T](c: Class[T]) = runtimeMirror(c.getClassLoader).classSymbol(c).toType
}
