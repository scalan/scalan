package scalan.meta

import java.io.File
import scalan.util.FileUtil
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

  def contains(name: String) = table.contains(name)

  def apply(name: String): C = table.getOrElse(name, sys.error(s"Cannot find config $name in collection $table"))

  def find(p: C => Boolean): Option[C] = table.find { case (_, c) => p(c) }.map(_._2)
}

object ConfMap {
  def apply[C <: Conf]() = new ConfMap[C]()

  def apply[C <: Conf](c: C, cs: C*) = {
    val res = new ConfMap[C]()

    cs.foldLeft(res) { (res, c) => res.add(c) }
  }
}

abstract class ModuleConf extends Conf {
}

case class TargetModuleConf(
    name: String,
    targetFolder: String,
    sourceModules: ConfMap[SourceModuleConf]
) extends ModuleConf

case class SourceModuleConf(
    name: String,
    units: ConfMap[UnitConfig] = ConfMap()
) extends ModuleConf {
  def getResourceHome = name + "/src/main/resources"

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

  private def mkUnit(unitName: String, unitFile: String) =
    unitConfigTemplate(unitName, unitFile).copy(
      srcPath = s"$name/src/main/scala",
      resourcePath = s"$name/src/main/resources"
    )

  def hasUnit(unitName: String) = units.contains(unitName)

  def addUnit(unitName: String, unitFile: String): this.type = {
    units.add(mkUnit(unitName, unitFile))

    this
  }
}

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

