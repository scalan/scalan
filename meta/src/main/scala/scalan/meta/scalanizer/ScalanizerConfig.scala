package scalan.meta.scalanizer

import scalan.meta._
import scalan.meta.ScalanAst.{WrapperConfig, NonWrapper}

trait ScalanizerConfig {
  /** Modules that contain units to be virtualized by scalan-meta. */
  val sourceModules: ConfMap[SourceModuleConf]
  /** Modules that assemble virtualized units from source modules into virtualized cakes */
  val targetModules: ConfMap[TargetModuleConf]
  //  /** The folder of the module where the generated code will be stored.
  //    * This folder is relative to the project root folder. */
  //  def targetModuleFolder: String
  /** The flag indicates that the plugin has to generate additional information and to store it
    * the debug folder and outputs to the console. */
  def debug: Boolean

  def withDebug(d: Boolean): ScalanizerConfig

  /** Config for Scalan Codegen. */
  def unitConfigs: List[UnitConfig]

  def getUnitConfig(unitName: String): UnitConfig

  def getModule(moduleName: String): ModuleConf

  def wrapperConfigs: Map[String, WrapperConfig]

  def nonWrappers: Map[String, NonWrapper]
}
