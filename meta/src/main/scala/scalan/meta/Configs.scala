package scalan.meta

import java.io.File
import scalan.util.FileUtil
import scala.collection.mutable.{Map => MMap}

sealed trait Conf {
  def name: String
}

class ConfMap[C <: Conf](val underlying: MMap[String, C]) {
  def this() = this(MMap())
  def add(conf: C): this.type = {
    underlying += (conf.name -> conf)
    return this
  }
}

case class TargetModuleConf(
      name: String,
      targetFolder: String,
      sourceModules: ConfMap[SourceModuleConf]
) extends Conf

case class SourceModuleConf(
      name: String,
      units: MMap[String, UnitConfig]
) extends Conf

case class UnitConfig(
      name: String,
      srcPath: String, // the base path to where root package is located
      resourcePath: String,
      entityFile: String, // the package path and file name
      baseContextTrait: String = "scalan.Scalan",
      extraImports: List[String] = List("scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}", "scalan.meta.ScalanAst._"),
      isVirtualized: Boolean = true,
      isStdEnabled: Boolean = true) extends Conf {
  def getFile: File = FileUtil.file(srcPath, entityFile)
}

