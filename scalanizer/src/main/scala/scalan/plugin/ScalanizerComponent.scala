package scalan.plugin

import scala.tools.nsc.plugins.PluginComponent
import scalan.meta.scalanizer.{ScalanizerConfig, ScalanizerBase, ScalanizerState, Scalanizer}
import scala.tools.nsc.Global

/**
  * Created by slesarenko on 01/04/17.
  */
abstract class ScalanizerComponent(val plugin: ScalanizerPlugin) extends PluginComponent {
  val global: plugin.global.type = plugin.global
  val scalanizer: plugin.scalanizer.type = plugin.scalanizer
}
