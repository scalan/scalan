package scalan.plugin

import scala.tools.nsc._

object Debug {
  val name = "scalan-debug"
}

/** The component outputs the tree of compilation unit. */
//class Debug(override val plugin: ScalanizerPlugin) extends ScalanizerComponent(plugin) {
//  import global._
//  import plugin.scalanizer._
//
//  val phaseName: String = Debug.name
//  override def description: String = "Print AST of compilation units"
//
//  val runsAfter = List(FinalComponent.name)
//
//  def newPhase(prev: Phase) = new StdPhase(prev) {
//    def apply(unit: CompilationUnit) {
//    }
//  }
//}
