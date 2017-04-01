package scalan.meta.scalanizer

import scalan.meta.CodegenConfig

trait ScalanizerConfig {
  /** The folder where the app is located and where the generated code will be stored. */
  def home: String

  /** The flag indicates that generated code (virtualized code, boilerplate and type wrappers)
    * should be stored on the file system. */
  def save: Boolean

  /** Reload virtualized code from the file system. */
  def read: Boolean

  /** The flag indicates that the plugin has to generate additional information and to store it
    * the debug folder and outputs to the console. */
  def debug: Boolean

  /** The flag indicates that Meta AST of entities should be serialized and assigned to some variable
    * inside virtualized code. */
  def saveMetaAst: Boolean

  /** Mapping of entities and their concrete classes.
    * For example "Vec" -> Set("DenseVec", "SparseVec") */
  def concreteClassesOfEntity: Map[String, Set[String]]

  /** The types that shouldn't be Rep[].
    * For example List("Elem", "Cont", "ClassTag") */
  def typeClasses: List[String]

  /** Config for Scalan Codegen. */
  def codegenConfig: CodegenConfig
}
