package scalan.plugin

import scala.tools.nsc._

object VirtFrontend {
  val name = "scalanizer-virt-frontend"
}

/** Prepare for virtualization of modules.
  * At this phase all original modules are parsed into SModuleDef and saved in Scalanizer state.
  * This provides, together with already captured wrappers, a full set of definitions
  * that can be used to resolve type references on types and symbols.
  */
class VirtFrontend(override val plugin: ScalanizerPlugin) extends ScalanizerComponent(plugin) {
  import scalanizer._
  import scalanizer.global._

  val phaseName: String = VirtFrontend.name

  override def description: String = "Preparing for generating of Scala AST for virtualized cake."

  override val runsAfter = List(WrapBackend.name)

  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit): Unit = {
      val unitName = unit.source.file.name
      if (isModuleUnit(unitName)) {
        val moduleDef = moduleDefFromTree(unitName, unit.body, false)
        snState.addModule(unitName, moduleDef)
      }
    }
  }

}
