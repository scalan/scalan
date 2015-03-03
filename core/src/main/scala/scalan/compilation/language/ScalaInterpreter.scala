package scalan.compilation.language

trait ScalaInterpreter extends Interpreter {
  this: MethodMapping =>

  override def mappedClassName(c: Class[_]): Option[String] = {
    methodReplaceConf.classMap.get(c) match {
      case Some(func) => Some(func.asInstanceOf[MethodMapping#ScalaLanguage#ScalaFunc].funcName.name)
      case _ => None
    }
  }
}
