package scalan.plugin

import scala.tools.nsc.Global
import scalan.meta.ScalanAst.{WrapperDescr, SModuleDef}
import scalan.meta.scalanizer.{Scalanizer, ScalanizerState}

/** The object contains the current state and temporary data of the plugin. */
class ScalanizerPluginState[G <: Global](val scalanizer: Scalanizer[G]) extends ScalanizerState[G] {

}
