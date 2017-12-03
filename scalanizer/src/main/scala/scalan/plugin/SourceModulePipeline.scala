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
      val module = s.getSourceModule
      // add virtualized units from dependencies
      for (inModule <- module.collectInputModules()) {
        for (unitConf <- inModule.units.values) {
          val unit = parseEntityModule(unitConf.getResourceFile)(new ParseCtx(isVirtualized = true)(context))
          scalanizer.inform(s"Step(${step.name}): Adding dependency ${unit.fullName} parsed from ${unitConf.getResourceFile}")
          snState.addUnit(unit)
        }
      }
      // add not yet virtualized units from the current module
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
      snState.forEachWrapper { case (_, WrapperDescr(m, _, config)) =>
        val module = m.copy(imports = m.imports :+ SImportStat("scala.wrappers.WrappersModule"))(scalanizer.context)
        val moduleConf = getSourceModule

        /** Build source code of the wrapper module and store it in a file */
        val wrapperModuleWithoutImpl = module.copy(classes = Nil)(context)
        val optimizedImplicits = optimizeModuleImplicits(wrapperModuleWithoutImpl)
        val wrapperPackage = genWrapperPackage(optimizedImplicits)
        saveWrapperCode(moduleConf,
          optimizedImplicits.packageName,
          optimizedImplicits.name,
          showCode(wrapperPackage))
      }
    },
    ForEachUnitStep("virtfrontend") { context => import context._;
      withUnitModule(unit) { (module, unitFileName) =>
        val packageName = getUnitPackage(unit)
        val unitName = Path(unitFileName).stripExtension

        // this unit has been added in 'dependencies' step
        // now it can be replaced with the body which has passed namer and typer
        val existingUnit = snState.getUnit(packageName, unitName)

        implicit val ctx = new ParseCtx(false)(scalanizer.context)
        // here unit.body already passed namer and typer phases
        val unitDef = unitDefFromTree(unitFileName, unit.body)
        scalanizer.inform(s"Step(virtfrontend): Updating source unit ${existingUnit.fullName} with version from CompilationUnit(${unit.source.file})")
        snState.addUnit(unitDef)
      }
    },
    ForEachUnitStep("virtfinal") { context => import context._
      withUnitModule(unit) { (module, unitFileName) =>
        val packageName = getUnitPackage(unit)
        val unitName = Path(unitFileName).stripExtension
        val unitDef = snState.getUnit(packageName, unitName)

        /** Generates a virtualized version of original Scala AST, wraps types by Rep[] and etc. */
        val enrichedModuleDef = virtPipeline(unitDef)

        /** Scala AST of virtualized module */
        val optimizedImplicits = optimizeModuleImplicits(enrichedModuleDef)

        val virtAst = genUDModuleFile(optimizedImplicits, unit.body)

        val nameOnly = unitName

        saveCode(module, optimizedImplicits.packageName, nameOnly, showCode(virtAst))

        /** produce boilerplate code using ModuleFileGenerator
          * Note: we need enriched module with all implicits in there place for correct boilerplate generation */
        val boilerplateText = genUDModuleBoilerplateText(unitFileName, enrichedModuleDef)

        saveCode(module, enrichedModuleDef.packageName + ".impl", nameOnly + "Impl", boilerplateText)

        /** Checking of user's extensions like SegmentDsl, SegmentDslStd and SegmentDslExp */
        //        val extensions: List[Tree] = getExtensions(moduleDef)
        //        for ((e,i) <- extensions.zipWithIndex)
        //          showTree(s"extensions$i", unitName, e)
        /** Serialize Virtualized AST for passing to run-time. */
        //        val serializedModuleDef = serializeModuleDef(moduleDef)
        //        showTree("serializedAst", unitName, serializedModuleDef)
        /** Replace of hot spots by optimized kernels in the original Scala AST of current compilation unit. */
        //        val accelAst = transformHotSpots(moduleDef, unit)
        //        showTree("accelAst", unitName, accelAst)
        /** Staged Ast is package which contains virtualized Tree + boilerplate */
        //        val objectHotSpotKernels = getHotSpotKernels(moduleDef)
        //        val objectHotSpotManager = getHotSpotManager(moduleDef)
        //        val stagedAst = getStagedAst(
        //              moduleDef, virtAst, boilerplate, extensions, serializedModuleDef,
        //              objectHotSpotKernels,
        //              objectHotSpotManager)
        //        showTree("stagedAst", unitName, stagedAst)
      }
    }
  )
}
