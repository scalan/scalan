package scalan.plugin

import java.io.File

import scalan.util.FileUtil
import scala.tools.nsc._
import scalan.meta.ScalanAst._
import scalan.meta.ScalanAstExtensions._
import scalan.meta.{ScalanCodegen, SourceModuleConf}

object WrapBackend {
  val name = "scalanizer-backend"
}

/** Generating of Scala AST for wrappers. */
//class WrapBackend(override val plugin: ScalanizerPlugin) extends ScalanizerComponent(plugin) {
//  import plugin.scalanizer._
//  import plugin.scalanizer.global._
//
//  val phaseName: String = WrapBackend.name
//
//  override def description: String = "Generating of Scala AST for wrappers."
//
//  override val runsAfter = List(WrapEnricher.name)
//
//  def newPhase(prev: Phase) = new StdPhase(prev) {
//    override def run(): Unit = {
////      var wrapperSlices = initWrapperSlices
////      snState.forEachWrapper { case (_, WrapperDescr(m, _, config)) =>
////        val module = m.copy(imports = m.imports :+ SImportStat("scala.wrappers.WrappersModule"))(scalanizer.context)
////        val moduleConf = getSourceModule
////
////        /** Build source code of the wrapper module and store it in a file */
////        val wrapperModuleWithoutImpl = module.copy(classes = Nil)(context)
////        val optimizedImplicits = optimizeModuleImplicits(wrapperModuleWithoutImpl)
////        val wrapperPackage = genWrapperPackage(optimizedImplicits)
////        saveCode(moduleConf,
////          optimizedImplicits.packageName,
////          optimizedImplicits.name,
////          showCode(wrapperPackage))
////        /** Invoking of Scalan META to produce boilerplate code for the wrapper. */
////        val boilerplateText = genWrapperBoilerplateText(moduleConf, module)
////        saveCode(moduleConf, module.packageName + ".impl", module.name + "Impl", boilerplateText)
////        wrapperSlices = updateWrapperSlices(wrapperSlices, wrapperModuleWithoutImpl)
////      }
////      saveWrapperSlices(wrapperSlices)
//    }
//
//    def apply(unit: CompilationUnit): Unit = ()
//  }
//
//
//
////  /** Puts all wrappers to the cake WrappersDsl.
////    * Stores them into the file: <home>/scala/wrappers/WrappersModule.scala */
////  def saveWrapperSlices(module: SourceModuleConf, cake: WrapperSlices): Unit = {
////    implicit val genCtx = GenCtx(module = null, toRep = false)
////    val absCake = genTrait(cake.abs)
////    val cakePackage =
////      q"""
////        package scala.wrappers {
////          import scalan._
////
////          $absCake
////        }
////     """
////    val code = showCode(cakePackage)
////    saveWrapperCode("scala.wrappers", cake.abs.name, code)
////  }
//}
