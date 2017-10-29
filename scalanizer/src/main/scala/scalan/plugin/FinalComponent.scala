package scalan.plugin

import scala.reflect.io.Path
import scala.tools.nsc._
import scalan.meta.ScalanAst.SUnitDef
import scalan.meta.ScalanAst._

object FinalComponent {
  val name = "scalanizer-final"
}

class FinalComponent(override val plugin: ScalanizerPlugin) extends ScalanizerComponent(plugin) {
  import scalanizer._
  import scalanizer.global._

  val phaseName: String = FinalComponent.name
  override def description: String = "Code virtualization and specialization"
  val runsAfter = List(VirtFrontend.name)

  def showTree(prefix: String, name: String, tree: Tree) =
    saveDebugCode(prefix + "_" + name, showCode(tree))

  val virtPipeline = new ModuleVirtualizationPipeline()(context)

  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit): Unit = {
      val unitName = unit.source.file.name
      if (isModuleUnit(unitName)) try {
        val packageName = getModulePackage(unit)
        val moduleDef = snState.getModule(packageName, Path(unitName).stripExtension)

        /** Generates a virtualized version of original Scala AST, wraps types by Rep[] and etc. */
        val enrichedModuleDef = virtPipeline(moduleDef)

        /** Scala AST of virtualized module */
        val optimizedImplicits = optimizeModuleImplicits(enrichedModuleDef)
        val virtAst = genUDModuleFile(optimizedImplicits, unit.body)
        val nameOnly = Path(unitName).stripExtension
        saveWrapperCode(optimizedImplicits.packageName, nameOnly, showCode(virtAst), copyResource = true)

        /** produce boilerplate code using ModuleFileGenerator
          * Note: we need enriched module with all implicits in there place for correct boilerplate generation */
        val boilerplateText = genUDModuleBoilerplateText(unitName, enrichedModuleDef)
        saveWrapperCode(enrichedModuleDef.packageName + ".impl", nameOnly + "Impl", boilerplateText)

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

        if (snConfig.save) {
//          saveImplCode(unit.source.file.file, showCode(stagedAst))
        }

      } catch {
        case e: Exception => print(s"Error: failed to parse ${unitName} due to " + e.printStackTrace())
      }
    }
  }

  /** Puts all staff in one place. Here is an example for the Cols module:
    *  package scalanizer.collections {
    *  package implOfCols {
    *    object StagedEvaluation {
    *      // Virtualized code (virtAst)
    *      trait Cols extends Base {...}
    *      // Boilerplate generated by scalan-meta (boilerplate)
    *      trait ColsAbs extends ScalanDsl with Cols {...}
    *      trait ColsExp extends ScalanExp with ColsDsl {...}
    *      // Auto-generated extensions (extensions)
    *      trait ColsDsl extends ColsAbs {...}
    *      trait ColsDslExp extends ColsExp {...}
    *      // Module AST converted to string (serial)
    *      val serializedMetaAst = "..."
    *    }
    *    // Virtualized hot spots of the module and related staff (hotSpotKernels, hotSpotManager)
    *    object HotSpotKernels {...}
    *    object HotSpotManager {...}
    *  }}
    * */
  def getStagedAst(origModuleDef: SUnitDef,
                   virtAst: Tree,
                   boilerplate: Tree,
                   extensions: List[Tree],
                   serial: Tree,
                   hotSpotKernels: Tree, hotSpotManager: Tree): Tree =
  {
    val (imports, implStats) = boilerplate match {
      case PackageDef(_, topstats) =>
        val imports = topstats.collect { case i @ Import(_,_) => i }
        val implStatsOpt = topstats.collectFirst { case q"package impl { ..$stats }" => stats }
        (imports, implStatsOpt match {
          case Some(stats) => stats
          case None => !!!(s"Tree doesn't have correct structure: ${showCode(boilerplate)}")
        })
    }
    virtAst match {
      case PackageDef(pkgName, declStats) =>
        val body = declStats ++ implStats ++ extensions ++ List(serial)
        val stagedObj = q"object StagedEvaluation {..$body}"

        PackageDef(pkgName,
          List(PackageDef(
            Ident(TermName("implOf"+origModuleDef.name)),
            imports ++ List(stagedObj, hotSpotKernels, hotSpotManager)
          ))
        )
    }
  }

  /** Puts original and virtualized entity code together. For example:
    * package scalanizer.linalgebra {
    *   trait Matrs {self: LinearAlgebra => }
    * }
    * and the result of the getStagedAst method are combined into:
    * package scalanizer.linalgebra {
    *   trait Matrs {self: LinearAlgebra => ...}
    *   package implOfMatrs {...}
    * }
    * */
  def combineAst(orig: Tree, staged: Tree): Tree = {
    val stagedStats = staged match {
      case q"package $_ { ..$stats }" => stats
    }
    val newTree = orig match {
      case q"package $pkgname { ..$stats }" =>
        q"package $pkgname { ..${stats.toList ++ stagedStats} }"
      case _ => orig
    }
    newTree
  }

//  /** Generates extensions like:
//    *   trait MatrsDsl extends MatrsAbs { self: LinearAlgebraDsl => };
//    *   trait MatrsDslExp extends MatrsExp { self: LinearAlgebraDslExp => };
//    * for the module (Matrs).
//    */
//  def getExtensions(module: SModuleDef): List[Tree] = {
//    genModuleExtensions(module).map(extTrait => genTrait(extTrait)(GenCtx(module, false)))
//  }

  /** Converts Meta AST of a module to base64 string, assings the string to a variable and
    * returns Scala Tree of the variable. */
//  def serializeModuleDef(module: SModuleDef): Tree = {
//    val str = if (snConfig.saveMetaAst) {
//      val erasedModule = eraseModule(module)
//      Serialization.save(erasedModule)
//    } else ""
//    val serialized = Literal(Constant(str))
//
//    q"val serializedMetaAst = $serialized"
//  }
}
