package scalan.plugin

import scala.tools.nsc.plugins.PluginComponent
import scalan.meta.SourceModuleConf

/**
  * Created by slesarenko on 01/04/17.
  */
abstract class ScalanizerComponent(val plugin: ScalanizerPlugin) extends PluginComponent {
  val global: plugin.global.type = plugin.global
  val scalanizer: plugin.scalanizer.type = plugin.scalanizer
  import global._

  def getSourceModule = scalanizer.snConfig.sourceModules.get(plugin.sourceModuleName).getOrElse {
    global.abort(
      s"""Source module ${plugin.sourceModuleName } is not found in ${scalanizer.snConfig}.
        |Declared modules ${scalanizer.snConfig.sourceModules.keySet}.
       """.stripMargin
    )
  }

  def findUnitModule(unitName: String) = {
    scalanizer.snConfig.sourceModules.find(mc => mc.hasUnit(unitName))
  }

  def isModuleUnit(unitName: String) = findUnitModule(unitName).isDefined

  def getModulePackage(unit: global.CompilationUnit) = {
    val packageName = unit.body match {
      case pd: PackageDef =>
        val packageName = pd.pid.toString
        packageName
    }
    packageName
  }

  def withUnitModule(unit: CompilationUnit)(action: (SourceModuleConf, String) => Unit) = {
    val unitName = unit.source.file.name
    findUnitModule(unitName) match {
      case Some(sm) => try {
        action(sm, unitName)
      } catch {
        case e: Exception =>
          print(s"Error: failed to parse $unitName from ${unit.source.file } due to " + e.printStackTrace())
      }
      case None =>
    }
  }
}
