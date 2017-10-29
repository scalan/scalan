package scalan.meta.scalanizer

import scala.tools.nsc.Global
import scalan.meta.ScalanAst._
import scalan.meta.ScalanAstTransformers.{AstReplacer, TypeReplacer}
import scalan.meta.{ScalanParsers, UnitConfig, EntityManagement}
import scalan.util.FileUtil

trait ScalanizerBase[+G <: Global] extends ScalanParsers[G] {
  import global._
  import context._

  def snState : ScalanizerState[G]
  def snConfig: ScalanizerConfig
  def entityManagment: EntityManagement[G]


  def isNonWrapper(name: String): Boolean = {
    snConfig.nonWrappers.contains(name)
  }
  def isWrapper(name: String): Boolean = {
    val ok = !Set(
      isPrimitive _, isStandardType _,
      isEntity _, isEntityCompanion _,
      isClass _, isClassCompanion _,
      isModule _, isNonWrapper _
    ).exists(_(name))
    ok
  }

  def getParents(externalType: Type) = {
    externalType.typeSymbol.typeSignature match {
      case PolyType(_, ClassInfoType(parents, _, _)) => parents
      case ClassInfoType(parents, _, _) => parents
      case _ => Nil
    }
  }

  def getWrappersHome = snConfig.targetModuleFolder + "/src/main/scala"
  def getWrappersResourceHome = snConfig.targetModuleFolder + "/src/main/resources"

  def saveWrapperCode(packageName: String, fileName: String, wrapperCode: String, copyResource: Boolean = false) = {
    val packagePath = packageName.split('.').mkString("/")
    val wrapperFile = FileUtil.file(getWrappersHome, packagePath, fileName + ".scala")
    wrapperFile.mkdirs()
    FileUtil.write(wrapperFile, wrapperCode)
    if (copyResource) {
      val wrapperResourceFile = FileUtil.file(getWrappersResourceHome, packagePath, fileName + ".scala")
      FileUtil.copy(wrapperFile, wrapperResourceFile)
    }
  }

  class External2WrapperTypeTransformer(name: String)(implicit context: AstContext) extends AstReplacer(name, wrap)

  class ExtType2WrapperTypeTransformer(name: String) extends TypeReplacer(name, wrap)

}
