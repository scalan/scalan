package scalan

import scalan.meta.ScalanAst.{SModuleDef, STraitOrClassDef}
import scala.collection.mutable
import scala.reflect.internal.util.BatchSourceFile
import scalan.meta.{BoilerplateToolRun, Parsers}
import scalan.util.{ReflectionUtil, FileUtil}

abstract class ModuleInfo(val name: String, val sourceFileName: String) {
}

trait Modules extends Base { self: Scalan =>
  private[this] lazy val modules = mutable.Map.empty[String, SModuleDef]
  private[this] lazy val parsers = {
    val parsers = new Parsers(BoilerplateToolRun.allConfigs)
    parsers
  }

  def getModules = modules

  def allEntities = modules.values.flatMap(_.allEntities)

  def registerModule(moduleInfo: ModuleInfo) = {
    val m = parsers.loadModuleDefFromResource(moduleInfo.sourceFileName)
    if (modules.contains(m.name))
      !!!(s"Module ${m.name} already registered")
    else {
      modules += (m.name -> m)
    }
  }

  def entityDef(e: EntityElem[_]): STraitOrClassDef = {
    val elemClassSymbol = ReflectionUtil.classToSymbol(e.getClass)
    val moduleName = elemClassSymbol.owner.name.toString.stripSuffix("Defs")
    val module = modules.getOrElse(moduleName, !!!(s"Module $moduleName not found"))
    val entityName = elemClassSymbol.name.toString.stripSuffix("Elem")
    module.allEntities.find(_.name == entityName).getOrElse {
      !!!(s"Entity $entityName not found in module $moduleName")
    }
  }

}
