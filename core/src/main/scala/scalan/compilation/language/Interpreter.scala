package scalan.compilation.language

import java.lang.reflect.{Method => JMethod}

trait Interpreter {
  this: MethodMappingDSL =>

  def mappedFunc(method: JMethod): Option[MappingTags#Fun] = {
    var fun: Option[MappingTags#Fun] = None
    methodReplaceConf.foreach(languageBackend => {
      val f = languageBackend.get(method.getDeclaringClass.getName, method.getName)
      f match {
        case None =>
        case _ => fun = f
      }
    })
    fun
  }

  def mappedClassName(c: Class[_]): Option[String] = sys.error("Don't know how to transform method call")
}
