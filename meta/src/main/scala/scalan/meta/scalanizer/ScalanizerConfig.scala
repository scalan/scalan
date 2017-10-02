package scalan.meta.scalanizer

import scalan.meta.CodegenConfig
import scalan.meta.ScalanAst.{WrapperConfig, NonWrapper}

trait ScalanizerConfig {
  /** The folder of the module where the generated code will be stored.
    * This folder is relative to the project root folder. */
  def targetModuleFolder: String

  /** The flag indicates that generated code (virtualized code, boilerplate and type wrappers)
    * should be stored on the file system. */
  def save: Boolean
  def withSave(s: Boolean): ScalanizerConfig

  /** Reload virtualized code from the file system. */
  def read: Boolean
  def withRead(r: Boolean): ScalanizerConfig

  /** The flag indicates that the plugin has to generate additional information and to store it
    * the debug folder and outputs to the console. */
  def debug: Boolean
  def withDebug(d: Boolean): ScalanizerConfig

  /** The flag indicates that Meta AST of entities should be serialized and assigned to some variable
    * inside virtualized code. */
  def saveMetaAst: Boolean
  def withSaveMetaAst(b: Boolean): ScalanizerConfig

  /** Config for Scalan Codegen. */
  def unitConfigs: List[CodegenConfig]
  def getUnitConfig(unitName: String): CodegenConfig

  /** Config for Scalan Wrapper Codegen */
  def wrappersCodegenConfig: CodegenConfig

  def wrapperConfigs: Map[String, WrapperConfig]

  def nonWrappers: Map[String, NonWrapper]
}
