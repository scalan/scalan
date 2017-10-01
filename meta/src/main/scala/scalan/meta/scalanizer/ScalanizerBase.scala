package scalan.meta.scalanizer

import scala.tools.nsc.Global
import scalan.meta.ScalanAst._
import scalan.meta.ScalanAstTransformers.{MetaAstReplacer, TypeReplacer}
import scalan.meta.{ScalanParsers, CodegenConfig, EntityManagement}
import scalan.util.FileUtil

trait ScalanizerBase[G <: Global] extends ScalanParsers[G] {
  import global._
  import context._

  def snState : ScalanizerState[G]
  def snConfig: ScalanizerConfig
  def entityManagment: EntityManagement[G]

  /** Gets module name by its entity. TODO: Should be a general solution. */
  def mod(name: String) = name + "s"
  /** Converts the name of external type to the name of its wrapper. */
  def wrap(name: String) = "W" + name
  /** Converts the name of external type to the name of the module which
    * contains a wrapper for the type. */
  def wmod(name: String) = "W" + mod(name)
  /** Gets name of companion by entity name */
  def comp(name: String) = name + "Companion"
  /** Gets name of the target package to put wrapper based on original package name */
  def wrapPackage(packageName: String) = packageName

  /** Classification of external types by their names. */
  def isPrimitive(name: String): Boolean = {
    STpePrimitives.keySet.contains(name)
  }
  def isStandardType(name: String): Boolean = {
    Set("Tuple", "Function").exists(name.startsWith(_)) ||
    Set("ClassTag").contains(name)
  }

  def isNonWrapper(name: String): Boolean = {
    snConfig.nonWrappers.keys.toSet.contains(name)
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

  def getWrappersHome = snConfig.home + "/src/main/scala"

  def saveWrapperCode(packageName: String, fileName: String, wrapperCode: String) = {
    val packagePath = packageName.split('.').mkString("/")
    val wrapperFile = FileUtil.file(getWrappersHome, packagePath, fileName + ".scala")
    wrapperFile.mkdirs()
    FileUtil.write(wrapperFile, wrapperCode)
  }

  class External2WrapperTypeTransformer(name: String)(implicit context: AstContext) extends MetaAstReplacer(name, wrap)

  class ExtType2WrapperTypeTransformer(name: String) extends TypeReplacer(name, wrap)

}
