package scalan.meta

import java.io.File
import scalan.util.FileUtil
import scalan.util.CollectionUtil._
import scala.collection.mutable.{Map => MMap}

sealed trait Conf {
  def name: String
}

class ConfMap[C <: Conf] private(val table: MMap[String, C]) extends (String => C) {
  private def this() = this(MMap())

  def add(conf: C): this.type = {
    table += (conf.name -> conf)
    return this
  }

  def keySet = table.keySet

  def values = table.values

  def contains(name: String) = table.contains(name)

  def apply(name: String): C = table.getOrElse(name, sys.error(s"Cannot find config $name in collection $table"))

  def get(name: String) = table.get(name)

  def find(p: C => Boolean): Option[C] = table.find { case (_, c) => p(c) }.map(_._2)
}

object ConfMap {
  def apply[C <: Conf](): ConfMap[C] = new ConfMap[C]()

  def apply[C <: Conf](c: C, cs: C*): ConfMap[C] = {
    val res = ConfMap[C].add(c)
    cs.foldLeft(res) { (res, c) => res.add(c) }
  }
}

abstract class ModuleConf extends Conf {
}

object ModuleConf {
  val SourcesDir = "src/main/scala"
  val ResourcesDir = "src/main/resources"
}

case class TargetModuleConf(
    name: String,
    sourceModules: ConfMap[SourceModuleConf]
) extends ModuleConf

case class SourceModuleConf(
    name: String,
    units: ConfMap[UnitConfig] = ConfMap()
) extends ModuleConf {
  def getResourceHome = s"$name/${ModuleConf.ResourcesDir}"
  def getWrappersHome = getResourceHome + "/wrappers"

  private def unitConfigTemplate(name: String, entityFile: String) =
    UnitConfig(
      name = name, srcPath = "", resourcePath = "", entityFile = entityFile,
      baseContextTrait = "scalan.Scalan", // used like this: trait ${module.name}Defs extends ${config.baseContextTrait.opt(t => s"$t with ")}${module.name} {
      extraImports = List(
        "scala.reflect.runtime.universe._",
        "scala.reflect._"
      ),
      isVirtualized = false, isStdEnabled = false
    )

  def mkUnit(unitName: String, unitFile: String) =
    unitConfigTemplate(unitName, unitFile).copy(
      srcPath = s"$name/${ModuleConf.SourcesDir}",
      resourcePath = s"$name/${ModuleConf.ResourcesDir}"
    )

  def hasUnit(unitName: String) = units.contains(unitName)

  def addUnit(unitName: String, unitFile: String): this.type = {
    units.add(mkUnit(unitName, unitFile))
    this
  }

  def listWrapperFiles: Array[File] = {
    import FileUtil._
    file(getWrappersHome).traverseDepthFirst(f => listDirectories(f).toList).toArray
  }
}

case class UnitConfig(
    name: String,
    srcPath: String, // the base path to where root package is located (example: <module>/<ModuleConf.SourceDir>)
    resourcePath: String, // example: <module>/<ModuleConf.ResourcesDir>
    entityFile: String, // the package path and file name (example: scalan/collection/Col.scala)
    baseContextTrait: String = "scalan.Scalan",
    extraImports: List[String] = List("scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}", "scalan.meta.ScalanAst._"),
    isVirtualized: Boolean = true,
    isStdEnabled: Boolean = true) extends Conf {
  def getFile: File = FileUtil.file(srcPath, entityFile)
  def getResourceFile: File = FileUtil.file(resourcePath, entityFile)
}

object UnitConfig {
  def getImplFile(file: File, suffix: String, extension: String) = {
    val fileName = file.getName.split('.')(0)
    val folder = file.getParentFile
    val implFile = FileUtil.file(folder, "impl", s"$fileName$suffix.$extension")
    implFile
  }
}