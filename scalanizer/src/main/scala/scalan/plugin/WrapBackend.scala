package scalan.plugin

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

  case class WrapperSlices(abs: STraitDef, seq: STraitDef, exp: STraitDef)

  def newPhase(prev: Phase) = new StdPhase(prev) {
    override def run(): Unit = {
      var wrapperSlices = initWrapperSlices
      snState.wrappers foreach { case (_, WrapperDescr(module, _)) =>
        /** Invoking of Scalan META to produce boilerplate code for the wrapper. */
        val boilerplate = genWrapperBoilerplate(module)
        saveWrapperBoilerplate(module.name, boilerplate)

        /** Form source code of the wrapper and store it. */
        val wrapperModuleWithoutImpl = module.copy(concreteSClasses = Nil)
        val wrapperPackage = genWrapperPackage(wrapperModuleWithoutImpl)
        saveWrapperCode(wrapperModuleWithoutImpl.name, showCode(wrapperPackage))

        wrapperSlices = updateWrapperSlices(wrapperSlices, wrapperModuleWithoutImpl)
      }
      saveWrapperSlices(wrapperSlices)
    }

    def apply(unit: CompilationUnit): Unit = ()
  }

  /** Calls Scalan Meta to generate boilerplate code for the wrapper. */
  def genWrapperBoilerplate(module: SModuleDef): String = {
    val gen = new scalan.meta.ModuleFileGenerator(
      ScalanCodegen, module, snConfig.codegenConfig)
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
    val wrappersPackage = PackageDef(Ident(TermName("wrappers")), pkgStats)

    wrappersPackage
  }
  def getWrappersHome = snConfig.home + "/src/main/scala/wrappers"
  def saveWrapperCode(fileName: String, wrapperCode: String) = {
    val wrapperFile = FileUtil.file(getWrappersHome, fileName + ".scala")
    wrapperFile.mkdirs()
    FileUtil.write(wrapperFile, wrapperCode)
  }
  def saveWrapperBoilerplate(fileName: String, boilerplate: String): Unit = {
    val boilerplateFile = FileUtil.file(getWrappersHome, "impl", fileName + "Impl.scala")
    boilerplateFile.mkdirs()
    FileUtil.write(boilerplateFile, boilerplate)
  }

  def initWrapperSlices: WrapperSlices = {
    val abs = STraitDef("WrappersDsl",
          tpeArgs = Nil,
          ancestors = List(STraitCall("ScalanDsl", Nil)),
          body = Nil, selfType = None, companion = None)
    val seq = STraitDef("WrappersDslStd", Nil,
      List(STraitCall("ScalanDslStd", Nil), STraitCall("WrappersDsl", Nil)),
      Nil, None, None)
    val exp = STraitDef("WrappersDslExp", Nil,
      List(STraitCall("ScalanDslExp", Nil), STraitCall("WrappersDsl", Nil)),
      Nil, None, None)

    WrapperSlices(abs, seq, exp)
  }

  def updateWrapperSlices(slices: WrapperSlices, module: SModuleDef): WrapperSlices = {
    val absAncestors = slices.abs.ancestors :+ STraitCall(module.name + "Dsl", Nil)
    val seqAncestors = if (snConfig.codegenConfig.isStdEnabled)
                         slices.seq.ancestors :+ STraitCall(module.name + "DslStd", Nil)
                       else
                         slices.seq.ancestors
    val expAncestors = slices.exp.ancestors :+ STraitCall(module.name + "DslExp", Nil)

    WrapperSlices(
      abs = slices.abs.copy(ancestors = absAncestors),
      seq = slices.seq.copy(ancestors = seqAncestors),
      exp = slices.exp.copy(ancestors = expAncestors)
    )
  }

  /** Puts all wrappers to the cakes WrappersDsl, WrappersDslStd and WrappersDslExp.
    * Stores them into the file: <home>/wrappers/Wrappers.scala */
  def saveWrapperSlices(cake: WrapperSlices): Unit = {
    implicit val genCtx = GenCtx(module = null, toRep = false)
    val absCake = genTrait(cake.abs)
    val seqCake = genTrait(cake.seq)
    val expCake = genTrait(cake.exp)

    val cakePackage =
      q"""
        package wrappers {
          import scalan._

          $absCake

          $seqCake

          $expCake
        }
     """

    saveWrapperSlices(showCode(cakePackage))
  }
  def saveWrapperSlices(cakes: String): Unit = {
    val wrapperFile = FileUtil.file(getWrappersHome, "Wrappers.scala")
    wrapperFile.mkdirs()
    FileUtil.write(wrapperFile, cakes)
  }
}
