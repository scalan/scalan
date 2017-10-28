package scalan.plugin

import scalan.meta.scalanizer.Scalanizer
import scala.tools.nsc.Global

//abstract class ScalanPluginPipeline[G <: Global](val scalanizer: Scalanizer[G]) {
//
//}
//
//class ScalanizerPipeline[G <: Global](s: Scalanizer[G]) extends ScalanPluginPipeline[G](s) {
//  import scalanizer._
//  import scalanizer.global._
//
//  sealed trait Phase
//  class ForEachUnit(action: (ScalanizerComponent, CompilationUnit) => Unit) extends Phase
//  class Run(action: Unit => Unit) extends Phase
//
//  def isModuleUnit(unitName: String) = {
//    scalanizer.snConfig.unitConfigs.exists(_.name == unitName)
//  }
//
//  def getModulePackage(unit: global.CompilationUnit) = {
//    val packageName = unit.body match {
//      case pd: PackageDef =>
//        val packageName = pd.pid.toString
//        packageName
//    }
//    packageName
//  }
//
//  val phases = List(
//    ForEachUnit {
//      def apply(unit: CompilationUnit) {
//        val unitName = unit.source.file.name
//        if (isModuleUnit(unitName)) {
//          //        /* Collect all methods with the HotSpot annotation. */
//          //        val hotSpotFilter = new FilterTreeTraverser(isHotSpotTree)
//          //        hotSpotFilter.traverse(unit.body)
//          //        /* Traversing through the hot spots and building of type wrappers. */
//          //        hotSpotFilter.hits foreach { hotSpot =>
//          //          new ForeachTreeTraverser(catchWrapperUsage).traverse(hotSpot)
//          //        }
//          new ForeachTreeTraverser(catchWrapperUsage).traverse(unit.body)
//        }
//      }
//    }
//  )
//
//}
