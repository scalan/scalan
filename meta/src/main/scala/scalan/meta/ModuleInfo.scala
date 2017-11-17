package scalan

import scalan.meta.SName

case class ModuleInfo(packageName: String, moduleName: String) {
  val name = SName(packageName, moduleName)
  def getKey = name.mkFullName
  def sourceFileName = packageName.split('.').mkString("/") + s"/$moduleName.scala"
}

