package scalan.meta

import java.io.File

import scalan.util.{FileUtil, GraphUtil}
import scalan.util.CollectionUtil._
import scalan.util.StringUtil._
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

  def apply[C <: Conf](cs: C*): ConfMap[C] = {
    val res = ConfMap[C]()
    cs.foldLeft(res) { (res, c) => res.add(c) }
  }
}

abstract class ModuleConf extends Conf {
  def units: ConfMap[UnitConfig]

  def dependencies: ConfMap[ModuleConf]

  def dependsOnModules(): Set[ModuleConf] =
    GraphUtil.depthFirstSetFrom(Set(this.dependencies.values.toSeq: _*))(m => m.dependencies.values)

  def getResourceHome = s"$name/${ModuleConf.ResourcesDir }"

  def getWrappersHome = getResourceHome + "/wrappers"

  private def unitConfigTemplate(name: String, entityFile: String) =
    UnitConfig(
      name = name, srcPath = "", resourcePath = "", entityFile = entityFile,
      baseContextTrait = "scalan.Scalan", // used like this: trait ${module.name}Defs extends ${config.baseContextTrait.opt(t => s"$t with ")}${module.name} {
      extraImports = List(
        "scala.reflect.runtime.universe._",
        "scala.reflect._"
      ),
      isVirtualized = false
    )

  def mkUnit(unitName: String, unitFile: String, isVirtualized: Boolean) =
    unitConfigTemplate(unitName, unitFile).copy(
      srcPath = s"$name/${ModuleConf.SourcesDir }",
      resourcePath = s"$name/${ModuleConf.ResourcesDir }",
      isVirtualized = isVirtualized
    )
}

object ModuleConf {
  val SourcesDir = "src/main/scala"
  val TestsDir = "src/test/scala"
  val ResourcesDir = "src/main/resources"
  val ResourceFileExtension = ".scalan"
}

class TargetModuleConf(
    val name: String,
    val sourceModules: ConfMap[SourceModuleConf]
) extends ModuleConf {
  override def units: ConfMap[UnitConfig] = ConfMap()

  override def dependencies: ConfMap[ModuleConf] = ConfMap(sourceModules.values.map(v => v: ModuleConf).toSeq: _*)
}

class SourceModuleConf(
    val name: String,
    val units: ConfMap[UnitConfig] = ConfMap(),
    val deps: ConfMap[SourceModuleConf] = ConfMap()
) extends ModuleConf {
  override def dependencies: ConfMap[ModuleConf] = deps.asInstanceOf[ConfMap[ModuleConf]]

  def hasUnit(unitName: String) = units.contains(unitName)

  def addUnit(unitName: String, unitFile: String): this.type = {
    units.add(mkUnit(unitName, unitFile, isVirtualized = false))
    this
  }

  def dependsOn(us: SourceModuleConf*): this.type = {
    for ( u <- us ) deps.add(u)
    this
  }

  def listWrapperFiles: Array[File] = {
    import FileUtil._
    listFilesRecursive(file(getWrappersHome))
  }
}

case class UnitConfig(
    name: String,
    srcPath: String, // the base path to where root package is located (example: <module>/<ModuleConf.SourceDir>)
    resourcePath: String, // example: <module>/<ModuleConf.ResourcesDir>
    entityFile: String, // the package path and file name (example: scalan/collection/Col.scala)
    baseContextTrait: String = "scalan.Scalan",
    extraImports: List[String] = List("scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}", "scalan.meta.ScalanAst._"),
    isVirtualized: Boolean = true) extends Conf {
  def getFile: File = FileUtil.file(srcPath, entityFile)
  def getResourceFile: File = {
    val entityResource = entityFile.replaceSuffix(".scala", ModuleConf.ResourceFileExtension)
    FileUtil.file(resourcePath, entityResource)
  }
}

object UnitConfig {
  def getImplFile(file: File, suffix: String, extension: String) = {
    val fileName = file.getName.split('.')(0)
    val folder = file.getParentFile
    val implFile = FileUtil.file(folder, "impl", s"$fileName$suffix.$extension")
    implFile
  }
}