package scalan.plugin

import java.io.File

import scalan.util.FileUtil
import scala.tools.nsc._
import scalan.meta.ScalanAst._
import scalan.meta.ScalanCodegen

object WrapBackend {
  val name = "scalanizer-backend"
}

/** Generating of Scala AST for wrappers. */
class WrapBackend(override val plugin: ScalanizerPlugin) extends ScalanizerComponent(plugin) {
  import plugin.scalanizer._
  import plugin.scalanizer.global._

  val phaseName: String = WrapBackend.name

  override def description: String = "Generating of Scala AST for wrappers."

  override val runsAfter = List(WrapEnricher.name)

  case class WrapperSlices(abs: STraitDef)

  def newPhase(prev: Phase) = new StdPhase(prev) {
    override def run(): Unit = {
      var wrapperSlices = initWrapperSlices
      snState.forEachWrapper { case (_, WrapperDescr(m, _, config)) =>
        val module = m.copy(imports = m.imports :+ SImportStat("scala.wrappers.WrappersModule"))(scalanizer.context)

        /** Form source code of the wrapper module and store it in a file */
        val wrapperModuleWithoutImpl = module.copy(concreteSClasses = Nil)(context)
        val optimizedImplicits = optimizeModuleImplicits(wrapperModuleWithoutImpl)
        val wrapperPackage = genWrapperPackage(optimizedImplicits)
        saveWrapperCode(
          optimizedImplicits.packageName,
          optimizedImplicits.name,
          showCode(wrapperPackage),
          copyResource = true)

        /** Invoking of Scalan META to produce boilerplate code for the wrapper. */
        val boilerplateText = genWrapperBoilerplateText(module)
        saveWrapperCode(module.packageName + ".impl", module.name + "Impl", boilerplateText)

        wrapperSlices = updateWrapperSlices(wrapperSlices, wrapperModuleWithoutImpl)
      }
      saveWrapperSlices(wrapperSlices)
    }

    def apply(unit: CompilationUnit): Unit = ()
  }

  /** Calls Scalan Meta to generate boilerplate code for the wrapper. */
  def genWrapperBoilerplateText(module: SModuleDef): String = {
    val gen = new scalan.meta.ModuleFileGenerator(
      ScalanCodegen, module, snConfig.wrappersCodegenConfig)
    val implCode = gen.emitImplFile
    implCode
  }

  /** Generates Scala AST for the given wrapper (without implementation). */
  def genWrapperPackage(module: SModuleDef): Tree = {
    implicit val genCtx = GenCtx(module = module, toRep = true)
    val scalaAst = genModuleTrait(module)
    val imports = module.imports.map(genImport(_))
    val selfType = Some(SSelfTypeDef("self", List(STraitCall("Wrappers", Nil))))
//    val extensions = genExtensions(module.name, selfType, Nil).map(
//      extTrait => genTrait(extTrait)(GenCtx(module, false))
//    )
    val pkgStats = imports :+ scalaAst
    val wrappersPackage = PackageDef(Ident(TermName(module.packageName)), pkgStats)
    wrappersPackage
  }

  def initWrapperSlices: WrapperSlices = {
    val wmodule = STraitDef("WrappersModule",
          tpeArgs = Nil,
          ancestors = List(),
          body = Nil, selfType = None, companion = None)
    WrapperSlices(wmodule)
  }

  def updateWrapperSlices(slices: WrapperSlices, module: SModuleDef): WrapperSlices = {
    val absAncestors = slices.abs.ancestors :+ STraitCall(module.name + "Module", Nil).toTypeApply
    WrapperSlices(
      abs = slices.abs.copy(ancestors = absAncestors)
    )
  }

  /** Puts all wrappers to the cake WrappersDsl.
    * Stores them into the file: <home>/scala/wrappers/WrappersModule.scala */
  def saveWrapperSlices(cake: WrapperSlices): Unit = {
    implicit val genCtx = GenCtx(module = null, toRep = false)
    val absCake = genTrait(cake.abs)
    val cakePackage =
      q"""
        package scala.wrappers {
          import scalan._

          $absCake
        }
     """
    val code = showCode(cakePackage)
    saveWrapperCode("scala.wrappers", cake.abs.name, code)
  }

}
