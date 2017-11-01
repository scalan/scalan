package scalan.plugin

import scala.reflect.internal.Phase
import scala.tools.nsc.Global
import scala.tools.nsc.plugins.PluginComponent
import scalan.meta.SourceModuleConf

abstract class ScalanizerComponent(
    val phaseName: String,
    val runsAfter: List[String],
    val global: Global) extends PluginComponent {
  //    val scalanizer: plugin.scalanizer.type = plugin.scalanizer
  //  import global._
  override def description: String = s"Scalanizer component at phase $phaseName runs after $runsAfter"
}

object ScalanizerComponent {
  def forEachUnit[P <: ScalanizerPlugin](p: P)
      (runsAfter: List[String], step: p.scalanizer.ForEachUnitStep) =
    new ScalanizerComponent(step.name, runsAfter, p.scalanizer.global) {
      def newPhase(prev: Phase) = new StdPhase(prev) {
        def apply(unit: global.CompilationUnit): Unit = {
          val context = new p.scalanizer.UnitContext(unit.asInstanceOf[p.scalanizer.global.CompilationUnit])
          step.action(context)
        }
      }
    }

  def forRun[P <: ScalanizerPlugin](p: P)
      (runsAfter: List[String], step: p.scalanizer.RunStep) =
    new ScalanizerComponent(step.name, runsAfter, p.scalanizer.global) {
      def newPhase(prev: Phase) = new StdPhase(prev) {
        override def run(): Unit = {
          step.action(())
        }
        def apply(unit: global.CompilationUnit): Unit = ()
      }
    }
}

