package scalan.plugin

import java.io.File

import scala.reflect.io.{PlainFile, Path}
import scalan.meta.scalanizer.Scalanizer
import scala.tools.nsc.Global
import scalan.meta.ScalanAst._
import scalan.meta.ScalanAstExtensions._
import scalan.meta._
import scalan.util.FileUtil
import scala.collection.mutable.{Map => MMap}

class TargetModulePipeline[+G <: Global](s: Scalanizer[G]) extends ScalanizerPipeline[G](s) {
  import scalanizer._
  import scalanizer.global._

  val name = "target-assembler"
  val runAfter = List("parser")
  val wrappers = MMap[SName, SUnitDef]()

  override def isEnabled: Boolean = {
    val moduleName = s.moduleName
    s.snConfig.targetModules.get(moduleName).isDefined
  }

  def copyFile(sourceFile: File, targetFile: File): Boolean = {
    val isNewFile = !targetFile.exists
    scalanizer.inform(s"Copying from $sourceFile to $targetFile ...")
    FileUtil.copy(sourceFile, targetFile)
    isNewFile
  }

  def copyScalanizedUnit(unit: UnitConfig, target: TargetModuleConf): Unit = {
    val sourceFile = unit.getResourceFile
    val targetFile = FileUtil.file(target.name, ModuleConf.SourcesDir, unit.entityFile)

    val sourceImpl = UnitConfig.getImplFile(sourceFile, "Impl", "scala")
    val targetImpl = UnitConfig.getImplFile(targetFile, "Impl", "scala")

    val isNewFile = copyFile(sourceFile, targetFile)
    val isNewImpl = copyFile(sourceImpl, targetImpl)
    if (isNewFile)
      global.currentRun.compileLate(new PlainFile(Path(targetFile)))
    if (isNewImpl)
      global.currentRun.compileLate(new PlainFile(Path(targetImpl)))
  }

  def mergeWrapperUnit(unit: SUnitDef) = {
    val wName = SName(unit.packageName, unit.name)
    scalanizer.inform(s"Merging wrapper $wName")
    wrappers.get(wName) match {
      case Some(existingUnit) =>
        scalanizer.inform(s"Cannot merge $wName")
        val merger = new SUnitMerger(existingUnit)(scalanizer.context)
        val newUnit =merger.merge(unit)
        wrappers(wName) = newUnit
      case None =>
        wrappers(wName) = unit
    }
  }

  val steps: List[PipelineStep] = List(
    RunStep("assembler") { _ =>
      scalanizer.inform(s"Processing target module ${scalanizer.moduleName }")
      // merge all partial wrappers from source modules
      val target = snConfig.targetModules(moduleName)
      for (source <- target.sourceModules.values) {
        for (wFile <- source.listWrapperFiles) {
          val unit = parseEntityModule(wFile)(new ParseCtx(isVirtualized = true)(context))
          mergeWrapperUnit(unit)
        }
      }
      // gen boilerplate and save for all merged wrappers
      for (w <- wrappers.values) {
        val wPackage = genWrapperPackage(w, isVirtualized = true)
        val sourceRoot = s"${target.name }/${ModuleConf.SourcesDir }"
        saveCode(sourceRoot,
          w.packageName, w.name,
          showCode(wPackage))
        val boilerplateText = genWrapperBoilerplateText(target, w, isVirtualized = true)
        saveCode(sourceRoot, w.packageName + ".impl", w.name + "Impl", boilerplateText)
      }
      // generate WrappersModule cake

      // copy scalanized units from source modules
      for (source <- target.sourceModules.values) {
        for (unit <- source.units.values) {
          copyScalanizedUnit(unit, target)
        }
      }
      ()
    }
  )
}
