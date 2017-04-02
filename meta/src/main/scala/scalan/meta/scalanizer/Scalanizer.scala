package scalan.meta.scalanizer

import scala.tools.nsc.Global

/** Scalanizer is a component which can be used from different contexts to generated
  * boilerplate code such as wrappers, Impl files etc.
  * Scalanizer object is created for a set of Scalan modules of scalan-meta (SModuleDef) */
trait Scalanizer[G <: Global]
  extends ScalanizerBase[G]
  with Enricher[G]
  with Backend[G]
  with HotSpots[G] {
}
