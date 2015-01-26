package scalan.common

trait Config {
  val verbosity = System.getProperty("scalan.verbosity","0").toInt
  val sourceinfo = System.getProperty("scalan.sourceinfo","0").toInt
  val addControlDeps = System.getProperty("scalan.controldeps","true").toBoolean
}
