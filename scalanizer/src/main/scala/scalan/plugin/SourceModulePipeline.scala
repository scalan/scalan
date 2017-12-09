package scalan.plugin

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.io.Path
import scalan.meta.scalanizer.Scalanizer
import scala.tools.nsc.Global
import scalan.meta.ModuleVirtualizationPipeline
import scalan.meta.ScalanAst._
import scalan.meta.ScalanAstExtensions._
import scalan.util.CollectionUtil._
import scalan.meta.ScalanAstTransformers.isIgnoredExternalType


class SourceModulePipeline[+G <: Global](s: Scalanizer[G]) extends ScalanizerPipeline[G](s) {
  import scalanizer._
  import scalanizer.global._
  val name = "scalanizer"
  val runAfter = List("typer")
  val virtPipeline = new ModuleVirtualizationPipeline()(context)

  override def isEnabled: Boolean = {
    val moduleName = s.moduleName
    s.snConfig.sourceModules.get(moduleName).isDefined
  }

  val steps: List[PipelineStep] = List(
    RunStep("dependencies") { step =>
      val module = scalanizer.getSourceModule
      // add virtualized units from dependencies
      for (depModule <- module.dependsOnModules()) {
        for (unitConf <- depModule.units.values) {
          val unit = parseEntityModule(unitConf.getResourceFile)(new ParseCtx(isVirtualized = true)(context))
          scalanizer.inform(s"Step(${step.name}): Adding dependency ${unit.fullName} parsed from ${unitConf.getResourceFile}")
          snState.addUnit(unit)
        }
      }
      // add not yet virtualized units from the current module
      // because we are running after typer, all the names has been resolved by the compiler
      // we need to ensure visibility of all the names by scalanizer as well
      for (unitConf <- module.units.values) {
        val unit = parseEntityModule(unitConf.getFile)(new ParseCtx(isVirtualized = false)(context))
        scalanizer.inform(s"Step(${step.name}): Adding unit ${unit.fullName} form module '${s.moduleName}' (parsed from ${unitConf.getFile})")
        snState.addUnit(unit)
      }
    },
    ForEachUnitStep("wrapfrontend") { context => import context._;
      val unitName = unit.source.file.name
      if (isModuleUnit(unitName)) {
        new ForeachTreeTraverser(catchWrapperUsage).traverse(unit.body)
      }
    },
    RunStep("enricher") { _ =>
      import virtPipeline._
      import moduleBuilder._
      implicit val context = virtPipeline.context

      snState.transformWrappers { case (name, wrapperDescr) =>
        /** Transformations of Wrappers by adding of Elem, Cont and other things. */
        val pipeline = scala.Function.chain(Seq(
          preventNameConflict _,
          addBaseToAncestors _,
          addDefAncestorToAllEntities _,
          updateSelf _,
          checkEntityCompanion _,
          constr2apply _,
          cleanUpClassTags _,
          preventNameConflict _,
          genEntityImplicits _,
          genMethodsImplicits _,
          replaceExternalTypeByWrapper _,
          /** Currently, inheritance of type wrappers is not supported.
            * Print warnings and remove ancestors. */
          filterAncestors _
        ))
        val enrichedModule = pipeline(wrapperDescr.module)

        wrapperDescr.copy(module = enrichedModule)
      }
      ()
    },
    RunStep("wrapbackend") { _ =>
      snState.forEachWrapper { case (_, WrapperDescr(u, _, config)) =>
        val wUnit = u.copy(imports = u.imports :+ SImportStat("scala.wrappers.WrappersModule"))(scalanizer.context)
        val moduleConf = getSourceModule

        /** Build source code of the wrapper unit and store it in a file */
        val wUnitWithoutImpl = wUnit.copy(classes = Nil)(context)
        val optImplicits = optimizeModuleImplicits(wUnitWithoutImpl)
        val wrapperPackage = genPackageDef(optImplicits, isVirtualized = false)(scalanizer.context)
        saveWrapperCode(moduleConf,
          optImplicits.packageName,
          optImplicits.name,
          showCode(wrapperPackage))
      }
    },
    ForEachUnitStep("virtfrontend") { context => import context._;
      withUnitModule(unit) { (module, unitFileName) =>
        // this unit has been added in 'dependencies' step
        val existingUnit = context.getUnit

        // now it can be replaced with the body which has passed namer and typer
        implicit val ctx = new ParseCtx(isVirtualized = false)(scalanizer.context)
        val unitDef = unitDefFromTree(unitFileName, unit.body)

        scalanizer.inform(
            s"Step(virtfrontend): Updating source unit ${existingUnit.fullName} " +
            s"with version from CompilationUnit(${unit.source.file})")
        snState.addUnit(unitDef)
      }
    },
    ForEachUnitStep("virtfinal") { context => import context._
      withUnitModule(unit) { (module, unitFileName) =>
        val unitDef = context.getUnit

        /** Generates a virtualized version of original Scala AST, wraps types by Rep[] and etc. */
        val virtUnitDef = virtPipeline(unitDef)

        /** Scala AST of virtualized module */
        implicit val ctx = GenCtx(scalanizer.context, isVirtualized = false, toRep = true)
        val virtAst = genPackageDef(virtUnitDef)
        
        saveCodeToResources(module, virtUnitDef.packageName, virtUnitDef.name, showCode(virtAst))
      }
    }
  )
}
