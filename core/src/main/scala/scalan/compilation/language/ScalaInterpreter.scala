package scalan.compilation.language

trait ScalaInterpreter extends Interpreter {
  this: MethodMapping =>

  override def mappedClassName(c: Class[_]): Option[String] = {
    var fun: Option[String] = None
    methodReplaceConf.foreach(languageBackend => {
      val f = languageBackend.classMap.get(c)
      f match {
        case Some(func) => fun = Some(func.asInstanceOf[MethodMapping#ScalaLanguage#ScalaFunc].funcName.name)
        case _ =>
      }
    })
    fun
  }
}
