package scalan.plugin

import java.io.File
import scala.reflect.io.{PlainFile, Path}
import scalan.meta.scalanizer.Scalanizer
import scala.tools.nsc.Global
import scalan.meta.ScalanAst._
import scalan.meta.ScalanAstExtensions._
import scalan.meta.{TargetModuleConf, UnitConfig}
import scalan.util.FileUtil

class TargetModulePipeline[+G <: Global](s: Scalanizer[G]) extends ScalanizerPipeline[G](s) {
  import scalanizer._
  import scalanizer.global._

  val name = "target-assembler"
  val runAfter = List("parser")

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
    val targetFile = FileUtil.file(target.name, "src/main/scala", unit.entityFile)
    val isNewFile = copyFile(sourceFile, targetFile)
    val sourceImpl = UnitConfig.getImplFile(sourceFile, "Impl", "scala")
    val targetImpl = UnitConfig.getImplFile(targetFile, "Impl", "scala")
    val isNewImpl = copyFile(sourceImpl, targetImpl)
    if (isNewFile)
      global.currentRun.compileLate(new PlainFile(Path(targetFile)))
    if (isNewImpl)
      global.currentRun.compileLate(new PlainFile(Path(targetImpl)))
  }

  val steps: List[PipelineStep] = List(
    RunStep("assembler") { _ =>
      scalanizer.inform(s"Processing target module ${scalanizer.moduleName }")
      val target = snConfig.targetModules(moduleName)
      for (source <- target.sourceModules.values) {
        for (unit <- source.units.values) {
          copyScalanizedUnit(unit, target)
        }
      }
      ()
    }
  )
}
