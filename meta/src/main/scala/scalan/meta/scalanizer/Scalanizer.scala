package scalan.meta.scalanizer

import scala.tools.nsc.Global
import scalan.meta.SourceModuleConf

/** Scalanizer is a component which can be used from different contexts to generated
  * boilerplate code such as wrappers, Impl files etc.
  * Scalanizer object is created for a set of Scalan modules of scalan-meta (SModuleDef) */
trait Scalanizer[+G <: Global]
    extends ScalanizerBase[G]
        with Enricher[G]
        with Backend[G]
        //  with HotSpots[G]
{

  class UnitContext(val unit: global.CompilationUnit)

  sealed trait PipelineStep {
    def name: String
  }

  case class ForEachUnitStep(name: String)(val action: UnitContext => Unit) extends PipelineStep

  case class RunStep(name: String)(val action: Unit => Unit) extends PipelineStep

  def sourceModuleName: String

  def getSourceModule = snConfig.sourceModules.get(sourceModuleName).getOrElse {
    global.abort(
      s"""Source module $sourceModuleName is not found in config instance of ${snConfig.getClass.getName}.
        |Declared modules ${snConfig.sourceModules.keySet}.
       """.stripMargin
    )
  }

  def findUnitModule(unitName: String) = {
    snConfig.sourceModules.find(mc => mc.hasUnit(unitName))
  }

  def isModuleUnit(unitName: String) = findUnitModule(unitName).isDefined

  def getModulePackage(unit: global.CompilationUnit) = {
    val packageName = unit.body match {
      case pd: global.PackageDef =>
        val packageName = pd.pid.toString
        packageName
    }
    packageName
  }

  def withUnitModule(unit: global.CompilationUnit)(action: (SourceModuleConf, String) => Unit) = {
    val unitName = unit.source.file.name
    findUnitModule(unitName) match {
      case Some(sm) => try {
        action(sm, unitName)
      } catch {
        case e: Exception =>
          print(s"Error: failed to parse $unitName from ${unit.source.file} due to " + e.printStackTrace())
      }
      case None =>
    }
  }
}
