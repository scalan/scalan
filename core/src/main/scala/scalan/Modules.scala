package scalan

import scalan.meta.ScalanAst.{SModuleDef, STraitOrClassDef}
import scala.collection.mutable
import scala.reflect.internal.util.BatchSourceFile
import scalan.meta.{Parsers, BoilerplateToolRun, Name}
import scalan.util.{ReflectionUtil, FileUtil}

trait Modules extends Base { self: Scalan =>
  private[scalan] lazy val parsers = {
    val parsers = new Parsers(BoilerplateToolRun.allConfigs)
    parsers.context.loadModulesFromResources()
    parsers
  }

  private[this] lazy val modules = parsers.context.modules

  def getModules: mutable.Map[String, SModuleDef] = modules

  def allEntities = getModules.values.flatMap(_.allEntities)

  def registerModule(moduleInfo: ModuleInfo) = {
    val pack = moduleInfo.packageName
    val name = moduleInfo.moduleName
    if (!parsers.context.hasModule(pack, name)) {
      val m = parsers.loadModuleDefFromResource(moduleInfo.sourceFileName)
      parsers.context.addModule(m)
      println(s"WARNING: module $pack.$name added by registerModule")
    }
  }

  def entityDef(e: EntityElem[_]): STraitOrClassDef = {
    val elemClassSymbol = ReflectionUtil.classToSymbol(e.getClass)
    val moduleName = elemClassSymbol.owner.name.toString.stripSuffix("Defs")
    val packageName = e.getClass.getPackage.getName.stripSuffix(".impl")
    val key = Name.fullNameString(packageName, moduleName)
    val module = modules.getOrElse(key, !!!(s"Module $key not found"))
    val entityName = elemClassSymbol.name.toString.stripSuffix("Elem")
    module.allEntities.find(_.name == entityName).getOrElse {
      !!!(s"Entity $entityName not found in module $moduleName")
    }
  }

}
