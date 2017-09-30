package scalan.plugin

import scala.tools.nsc.plugins.PluginComponent

/**
  * Created by slesarenko on 01/04/17.
  */
abstract class ScalanizerComponent(val plugin: ScalanizerPlugin) extends PluginComponent {
  val global: plugin.global.type = plugin.global
  val scalanizer: plugin.scalanizer.type = plugin.scalanizer
  import global._

  def isModuleUnit(unitName: String) = {
    scalanizer.snConfig.unitConfigs.exists(_.name == unitName)
  }

  def getModulePackage(unit: global.CompilationUnit) = {
    val packageName = unit.body match {
      case pd: PackageDef =>
        val packageName = pd.pid.toString
        packageName
    }
    packageName
  }
}
