package scalan.plugin

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.tools.nsc._
import scalan.meta.EntityManagement
import scalan.meta.ScalanAst._
import scalan.meta.ScalanAstExtensions._
import scalan.meta.ScalanAstTransformers._
import scalan.util.CollectionUtil.TraversableOps

object WrapFrontend {
  val name = "scalanizer-wrapfrontend"
}

/** The component builds wrappers. */
//class WrapFrontend(override val plugin: ScalanizerPlugin) extends ScalanizerComponent(plugin) {
//  import scalanizer._
//  import scalanizer.global._
//
//  val phaseName: String = WrapFrontend.name
//
//  override def description: String = "Building wrappers for external types"
//
//  val runsAfter = List("typer")
//
//  /** The phase creates wrappers for the type that are out of virtualization scope. */
//  def newPhase(prev: Phase) = new StdPhase(prev) {
//    def apply(unit: CompilationUnit) {
////      val unitName = unit.source.file.name
////      if (isModuleUnit(unitName)) {
////        //        /* Collect all methods with the HotSpot annotation. */
////        //        val hotSpotFilter = new FilterTreeTraverser(isHotSpotTree)
////        //        hotSpotFilter.traverse(unit.body)
////        //        /* Traversing through the hot spots and building of type wrappers. */
////        //        hotSpotFilter.hits foreach { hotSpot =>
////        //          new ForeachTreeTraverser(catchWrapperUsage).traverse(hotSpot)
////        //        }
////        scalanizer.inform(s"Catching wrappers in ${unit.source.file}")
////        new ForeachTreeTraverser(catchWrapperUsage).traverse(unit.body)
////      }
//    }
//  }
//
//
//}