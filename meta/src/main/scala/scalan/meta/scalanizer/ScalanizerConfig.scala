package scalan.meta.scalanizer

import scalan.meta.MetaConfig
import scalan.meta.ScalanAst.{WrapperConfig, NonWrapper}

trait ScalanizerConfig {

  /** The folder of the module where the generated code will be stored.
    * This folder is relative to the project root folder. */
  def targetModuleFolder: String

  /** The flag indicates that the plugin has to generate additional information and to store it
    * the debug folder and outputs to the console. */
  def debug: Boolean
  def withDebug(d: Boolean): ScalanizerConfig

  /** Config for Scalan Codegen. */
  def unitConfigs: List[MetaConfig]
  def getUnitConfig(unitName: String): MetaConfig

  /** Config for Scalan Wrapper Codegen */
  def wrappersMetaConfig: MetaConfig

  def wrapperConfigs: Map[String, WrapperConfig]

  def nonWrappers: Map[String, NonWrapper]
}
