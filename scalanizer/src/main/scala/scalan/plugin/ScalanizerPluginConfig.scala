package scalan.plugin

import java.lang.annotation.Annotation

import scalan.{FunctorType, ContainerType}
import scalan.meta.CodegenConfig
import scalan.meta.ScalanAst.{WrapperConfig, NonWrapper}
import scalan.meta.scalanizer.ScalanizerConfig

class ScalanizerPluginConfig extends ScalanizerConfig {
  /** The folder where the app is located and where the generated code will be stored. */
  val home = "/Users/slesarenko/Projects/github/scalan/morphic/library"

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

  /** Mapping of entities and their concrete classes. */
  val concreteClassesOfEntity = Map[String, Set[String]](
//    "Num" -> Set("DoubleNum"),
//    "NumMonoid" -> Set("PlusMonoid"),
    "Col" -> Set("ColOverArray")
//    "Vec" -> Set("DenseVec"),
//    "Matr" -> Set("DenseMatr"),
//    "MatrOp" -> Set("BaseMatrOp"),
//    "LinearAlgebraOp" -> Set("LA")
  )
  /** The types that shouldn't be Rep[]. */
  val typeClasses             = List("Elem", "Cont", "ClassTag")

  /** Config for scalan-meta. */
  val codegenConfig = CodegenConfig(name = "Scalan Plugin",
    srcPath = "/",
    entityFiles = List[String](
//      "Nums.scala"
//      ,"NumMonoids.scala"
      "Cols.scala"
//      ,"Vecs.scala"
//      ,"Matrs.scala"
//      ,"MatrOps.scala"
//      ,"LinearAlgebraOps.scala"
    ),
    extraImports = List(
      "scala.reflect.runtime.universe._",
      "scala.reflect._"
    ),
    isVirtualized = false,
    isStdEnabled = false
  )
  val wrappersCodegenConfig = CodegenConfig(
    name = "Wrappers Config",
    srcPath = "/",
    entityFiles = List[String](),
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
