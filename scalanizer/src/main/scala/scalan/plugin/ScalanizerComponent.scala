package scalan.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.PluginComponent

abstract class ScalanizerComponent(
    val phaseName: String,
    val runsAfter: List[String],
    val global: Global,
    val pipeline: ScalanizerPipeline[Global]) extends PluginComponent {
  override def enabled: Boolean = true
  override def description: String = s"Scalanizer component at phase $phaseName runs after $runsAfter"
}

