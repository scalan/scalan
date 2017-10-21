package scalan

import scalan.meta.Name

case class ModuleInfo(packageName: String, moduleName: String) {
  val name = Name(packageName, moduleName)
  def getKey = name.mkFullName
  def sourceFileName = packageName.split('.').mkString("/") + s"/$moduleName.scala"
}

