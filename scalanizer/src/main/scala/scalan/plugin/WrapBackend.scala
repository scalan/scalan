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
      snState.wrappers foreach { case (_, WrapperDescr(m, _, config)) =>
        val module = m.copy(imports = m.imports :+ SImportStat("scala.wrappers.WrappersModule"))
        /** Invoking of Scalan META to produce boilerplate code for the wrapper. */
        val boilerplate = genWrapperBoilerplate(module)
        saveWrapperBoilerplate(module.packageName, module.name, boilerplate)

        /** Form source code of the wrapper and store it. */
        val wrapperModuleWithoutImpl = module.copy(concreteSClasses = Nil)
        val wrapperPackage = genWrapperPackage(wrapperModuleWithoutImpl)
        saveWrapperCode(module.packageName, wrapperModuleWithoutImpl.name, showCode(wrapperPackage))

        wrapperSlices = updateWrapperSlices(wrapperSlices, wrapperModuleWithoutImpl)
      }
      saveWrapperSlices(wrapperSlices)
    }

    def apply(unit: CompilationUnit): Unit = ()
  }

  /** Calls Scalan Meta to generate boilerplate code for the wrapper. */
  def genWrapperBoilerplate(module: SModuleDef): String = {
    val gen = new scalan.meta.ModuleFileGenerator(
      ScalanCodegen, module, snConfig.wrappersCodegenConfig)
    val implCode = gen.emitImplFile
    implCode
  }

  /** Generates Scala AST for the given wrapper (without implementation). */
  def genWrapperPackage(module: SModuleDef): Tree = {
    implicit val genCtx = GenCtx(module = module, toRep = true)
    val scalaAst = genModule(module)
    val imports = module.imports.map(genImport(_))
    val selfType = Some(SSelfTypeDef("self", List(STraitCall("Wrappers", Nil))))
//    val extensions = genExtensions(module.name, selfType, Nil).map(
//      extTrait => genTrait(extTrait)(GenCtx(module, false))
//    )
    val pkgStats = imports :+ scalaAst
    val wrappersPackage = PackageDef(Ident(TermName(module.packageName)), pkgStats)

    wrappersPackage
  }
  def getWrappersHome = snConfig.home + "/src/main/scala"
  def saveWrapperCode(packageName: String, fileName: String, wrapperCode: String) = {
    val wrapperFile = FileUtil.file(getWrappersHome, packageName, fileName + ".scala")
    wrapperFile.mkdirs()
    FileUtil.write(wrapperFile, wrapperCode)
  }
  def saveWrapperBoilerplate(packageName: String, fileName: String, boilerplate: String): Unit = {
    val boilerplateFile = FileUtil.file(getWrappersHome, packageName, "impl", fileName + "Impl.scala")
    boilerplateFile.mkdirs()
    FileUtil.write(boilerplateFile, boilerplate)
  }

  def initWrapperSlices: WrapperSlices = {
    val dsl = STraitDef("WrappersModule",
          tpeArgs = Nil,
          ancestors = List(STraitCall("ScalanDsl", Nil).toTypeApply),
          body = Nil, selfType = None, companion = None)

    WrapperSlices(dsl)
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

    val wrapperFile = FileUtil.file(getWrappersHome, "scala/wrappers", cake.abs.name + ".scala")
    saveWrapperSlices(wrapperFile, showCode(cakePackage))
  }

  def saveWrapperSlices(wrapperFile: File, cakes: String): Unit = {
    wrapperFile.mkdirs()
    FileUtil.write(wrapperFile, cakes)
  }
}
