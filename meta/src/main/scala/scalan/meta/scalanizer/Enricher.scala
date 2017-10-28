package scalan.meta.scalanizer

import java.io.File

import scala.tools.nsc.Global
import scalan.meta.{ScalanAst, SModuleBuilder}
import scalan.meta.ScalanAst._
import scalan.meta.ScalanAstUtils._
import scalan.util.FileUtil
import scalan.util.CollectionUtil._

trait Enricher[+G <: Global] extends ScalanizerBase[G] {

  def saveDebugCode(fileName: String, code: String) = {
    val folder = new File("library")  // this is the root of 'library' module
    val file = FileUtil.file(folder, "debug", fileName)
    file.mkdirs()
    FileUtil.write(file, code)
  }

  def saveImplCode(file: File, implCode: String) = {
    val fileName = file.getName.split('.')(0)
    val folder = file.getParentFile
    val implFile = FileUtil.file(folder, "impl", s"${fileName}Impl.scala")
    implFile.mkdirs()
    FileUtil.write(implFile, implCode)
  }

  class ModuleVirtualizationPipeline(implicit val context: AstContext) extends (SModuleDef => SModuleDef) {
    val moduleBuilder = new SModuleBuilder()
    import moduleBuilder._

    def externalTypeToWrapper(module: SModuleDef) = {
      val wrappedModule = snState.externalTypes.foldLeft(module){(acc, externalTypeName) =>
        new External2WrapperTypeTransformer(externalTypeName).moduleTransform(acc)
      }
      wrappedModule
    }

    /** Module parent is replaced by the parent with its extension. */
//    def composeParentWithExt(module: SModuleDef) = {
//      val parentsWithExts = module.ancestors.map { anc =>
//        anc.copy(tpe = anc.tpe.copy(name = anc.tpe.name + "Dsl"))
//      }
//      module.copy(ancestors = parentsWithExts)
//    }

    /** Imports scalan._ and other packages needed by Scalan and further transformations. */
    def addImports(module: SModuleDef) = {
      val usersImport = module.imports.collect{
        case imp @ SImportStat("scalan.compilation.KernelTypes._") => imp
      }
      module.copy(imports = SImportStat("scalan._") :: (usersImport))
    }


    def addModuleTrait(module: SModuleDef) = {
      if (module.origModuleTrait.isEmpty) {
        val mainName = module.name
        val mt = STraitDef(
          name = SModuleDef.moduleTraitName(mainName),
          tpeArgs = Nil,
          ancestors = List(STraitCall(s"impl.${mainName}Defs"), STraitCall("scala.wrappers.WrappersModule")).map(STypeApply(_)),
          body = Nil, selfType = None, companion = None)
        module.copy(origModuleTrait = Some(mt))
      }
      else module
    }

    private val chain = scala.Function.chain(Seq(
      fixExistentialType _,
      externalTypeToWrapper _,
//      composeParentWithExt _,
      addBaseToAncestors _,
      addEntityAncestors _,
      updateSelf _,
//      addEntityRepSynonym _,
      addImports _,
      checkEntityCompanion _,
      checkClassCompanion _,
      cleanUpClassTags _,
      replaceClassTagByElem _,
      eliminateClassTagApply _,
      genEntityImpicits _, genClassesImplicits _, genMethodsImplicits _,
      fixEntityCompanionName _,
      fixEvidences _
    ))
    override def apply(module: Module): Module = chain(module)
  }


}
