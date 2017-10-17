package scalan.plugin

import java.lang.annotation.Annotation

import scalan.{FunctorType, ContainerType}
import scalan.meta.CodegenConfig
import scalan.meta.ScalanAst.{WrapperConfig, NonWrapper}
import scalan.meta.scalanizer.ScalanizerConfig

class ScalanizerPluginConfig extends ScalanizerConfig {
  val targetModuleFolder = "library"

  /** The flag indicates that generated code (virtualized code, boilerplate and type wrappers)
    * should be stored on the file system. */
  var save: Boolean           = true
  def withSave(s: Boolean): ScalanizerConfig = {save = s; this}

  /** Reload virtualized code from the file system. */
  var read: Boolean           = false
  def withRead(r: Boolean): ScalanizerConfig = { read = r; this }

  /** The flag indicates that the plugin has to generate additional information and to store it
    * the debug folder and outputs to the console. */
  var debug: Boolean          = true
  def withDebug(d: Boolean): ScalanizerConfig = { debug = d; this }

  /** The flag indicates that Meta AST of entities should be serialized and assigned to some variable
    * inside virtualized code. */
  var saveMetaAst: Boolean    = true
  def withSaveMetaAst(b: Boolean): ScalanizerConfig = { saveMetaAst = b; this }

  private def unitConfig(name: String, entityFile: String) =
    CodegenConfig(
      name = name, entityFile = entityFile,
      srcPath = "librarydef/src/main/scala",
      resourcePath = "librarydef/src/main/resources",
      baseContextTrait = "scalan.Scalan", // used like this: trait ${module.name}Defs extends ${config.baseContextTrait.opt(t => s"$t with ")}${module.name} {
      extraImports = List(
        "scala.reflect.runtime.universe._",
        "scala.reflect._"
      ),
      isVirtualized = false,
      isStdEnabled = false
    )

  /** A list of scalan modules that should be virtualized by scalan-meta. */
  val unitConfigs = List(
    unitConfig("Cols.scala", "scalanizer/collections/Cols.scala")
  )
  def getUnitConfig(unitName: String) = unitConfigs.find(_.name == unitName).getOrElse{
    sys.error(s"Cannot fing UnitConfig for '$unitName'")
  }

  val wrappersCodegenConfig = CodegenConfig(
    name = "Wrappers Config",
    srcPath = "librarydef/src/main/scala",
    resourcePath = "librarydef/src/main/resources",
    entityFile = "<shouldn't be used>",    // NOTE: there is no any wrapper source files
    extraImports = List(
      "scala.reflect.runtime.universe._",
      "scala.reflect._"
    ),
    isVirtualized = false,
    isStdEnabled = false
  )
  val wrapperConfigs = List[WrapperConfig](
    WrapperConfig(
      name = "Array",
      annotations = List(classOf[ContainerType], classOf[FunctorType]).map(_.getSimpleName)
    )
  ).map(w => (w.name, w)).toMap

  val nonWrappers = List[NonWrapper](
    NonWrapper(name = "Predef"),
    NonWrapper(name = "<byname>"),
    NonWrapper(name = "ArrayOps"),
    NonWrapper(name = "WrappedArray"),
    NonWrapper(name = "CanBuildFrom")
  ).map(w => (w.name, w)).toMap
}
