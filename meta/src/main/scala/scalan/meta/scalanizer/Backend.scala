package scalan.meta.scalanizer

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.Global
import scalan.meta.ScalanAst._
import scalan.meta.ScalanAstTransformers._
import scalan.meta.{ScalanCodegen, ModuleFileGenerator}
import scalan.util.ScalaNameUtil.PackageAndName

trait Backend[+G <: Global] extends ScalanizerBase[G] {
  import global._

  /** Generate boilerplate text for virtualized user-defined module */
  def genUDModuleBoilerplateText(unitName: String, module: SUnitDef): String = {
    val entityGen = new ModuleFileGenerator(
      ScalanCodegen,
      module.copy(
        origModuleTrait = Some(createModuleTrait(module.name)),
        okEmitOrigModuleTrait = true
      )(context),
      snConfig.getUnitConfig(unitName))
    entityGen.emitImplFile
  }

}
