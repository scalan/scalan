package scalan.plugin

import java.lang.annotation.Annotation

import scalan.{FunctorType, ContainerType}
import scalan.meta._
import scalan.meta.ScalanAst.{WrapperConfig, NonWrapper}
import scalan.meta.scalanizer.ScalanizerConfig

class ScalanizerPluginConfig extends ScalanizerConfig {
  val apiModule = SourceModuleConf("library-api")
      .addUnit("Cols.scala", "scalan/collection/Cols.scala")

  val implModule = SourceModuleConf("library-impl")
      .addUnit("ColsOverArrays.scala", "scalan/collection/ColsOverArrays.scala")

  /** Modules that contain units to be virtualized by scalan-meta. */
  val sourceModules: ConfMap[SourceModuleConf] = ConfMap(apiModule, implModule)

  /** Modules that assemble virtualized units from source modules into virtualized cakes */
  val targetModules: ConfMap[TargetModuleConf] = ConfMap()
      .add(TargetModuleConf("library",
        sourceModules = ConfMap()
            .add(apiModule)
            .add(implModule)))

  def getModule(moduleName: String): ModuleConf = {
    val source = sourceModules.get(moduleName)
    val target = targetModules.get(moduleName)
    if (source.isDefined && target.isDefined)
        sys.error(s"ScalanPlugin configuration error: Module $moduleName found in both source modules and target modules")
    else if (source.isEmpty && target.isEmpty)
      sys.error(s"ScalanPlugin configuration error: Module $moduleName not found in both source and target lists")
    else
      source.getOrElse(target.get)
  }

  /** The flag indicates that the plugin has to generate additional information and to store it
    * the debug folder and outputs to the console. */
  var debug: Boolean = true

  def withDebug(d: Boolean): ScalanizerConfig = {debug = d; this }

  val unitConfigs: List[UnitConfig] = (for {
    (_, mc) <- sourceModules.table
    (_, u) <- mc.units.table
  } yield u).toList

  def getUnitConfig(unitName: String) = unitConfigs.find(_.name == unitName).getOrElse {
    sys.error(s"Cannot fing UnitConfig for '$unitName'")
  }

  val wrapperConfigs: Map[String, WrapperConfig] = List(
    WrapperConfig(
      name = "Array",
      annotations = List(classOf[ContainerType], classOf[FunctorType]).map(_.getSimpleName)
    )
  ).map(w => (w.name, w)).toMap

  val nonWrappers: Map[String, NonWrapper] = List[NonWrapper](
    NonWrapper(name = "Predef"),
    NonWrapper(name = "<byname>"),
    NonWrapper(name = "ArrayOps"),
    NonWrapper(name = "WrappedArray"),
    NonWrapper(name = "CanBuildFrom")
  ).map(w => (w.name, w)).toMap
}
