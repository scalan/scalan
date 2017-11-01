package scalan.plugin

import scala.reflect.io.Path
import scalan.meta.scalanizer.Scalanizer
import scala.tools.nsc.Global
import scalan.meta.ScalanAst._
import scalan.meta.ScalanAstExtensions._

class TargetModulePipeline[+G <: Global](s: Scalanizer[G]) extends ScalanizerPipeline[G](s) {
  import scalanizer._
  import scalanizer.global._

  val name = "target-assembler"
  val runAfter = List("typer")
  //  val virtPipeline = new ModuleVirtualizationPipeline()(context)
  val steps: List[PipelineStep] = List(
    RunStep("assembler") {_ =>
      scalanizer.inform(s"Processing target module ${scalanizer.targetModuleName}")
      ////      val targetFile = "library-api/src/main/scala/scalan/Test.scala"
      ////      val sourceFile = "library-impl/src/main/resources/scalan/Test.scala"
      ////      if (!Path(targetFile).exists) {
      ////        FileUtil.copy(new File(sourceFile), new File(targetFile))
      ////        global.currentRun.compileLate(new PlainFile(Path(sourceFile)))
      ////      }
      ()
    }
  )
}
