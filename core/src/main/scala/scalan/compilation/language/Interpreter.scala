package scalan.compilation.language

import java.lang.reflect.{Method => JMethod}

trait Interpreter {
  this: MethodMappingDSL =>

  def mappedFunc(method: JMethod): Option[MappingTags#Fun] =
    methodReplaceConf.map(l => l.get(method.getDeclaringClass.getName, method.getName)).reverse.find(_ != None).getOrElse(None)

  def mappedClassName(c: Class[_]): Option[String] = {
    var fun: Option[String] = None
    methodReplaceConf.foreach(languageBackend => {
      val f = languageBackend.classMap.get(c)
      f match {
        case Some(func) => fun = Some(func.name)
        case _ =>
      }
    })
    fun
  }
}
