package scalan.compilation.language

import java.lang.reflect.{Method => JMethod}

trait Interpreter {
  this: MethodMappingDSL =>

  def mappedFunc(method: JMethod): Option[MappingTags#Fun] = methodReplaceConf.map(l => l.get(method.getDeclaringClass.getName, method.getName)).reverse.find(_ != None).getOrElse(None)

  def mappedClassName(c: Class[_]): Option[String] = //sys.error("Don't know how to transform method call")
    sys.error("Don't know how to transform method call; " + c.toString())
}
