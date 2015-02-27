package scalan.compilation.language

import java.lang.reflect.{Method => JMethod}

trait Interpreter {
  this: MethodMapping =>

  def mappedFunc(method: JMethod): Option[LanguageConf#Fun] = {
    methodReplaceConf.get(method.getDeclaringClass.getName, method.getName) 
  }

  def mappedClassName(c: Class[_]): Option[String] = ???
}
