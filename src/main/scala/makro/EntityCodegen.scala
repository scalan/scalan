/**
 * User: Alexander Slesarenko
 * Date: 11/16/13
 */
package scalan.makro

import scalan.codegen.emit.Formatter
import scalan.codegen.emit.ast._
import scalan.codegen.emit.ScalaCodeEmitter
import makro.{ScalanParsers, ScalanAst}

trait ScalanCodegen extends ScalanAst with ScalanParsers {

  case class Context(name: String)
  case class EntityModuleDef(packageName: String, name: String, entityDef: TraitDef, selfType: Option[String] = None) {
  }

  def parseEntity(entityTemplate: String): TraitDef = parseTrait(entityTemplate)

  def entity(packageName: String, entityTemplate: String) = {
    val entity = parseEntity(entityTemplate)
    EntityModuleDef(packageName, s"${entity.name}s", entity)
  }
  def entity(packageName: String, entityTemplate: String, moduleName: String) = {
    val entity = parseEntity(entityTemplate)
    EntityModuleDef(packageName, moduleName, entity)
  }

  class EntityFileGenerator(module: EntityModuleDef) {

//    private def genModuleHeader(implicit f: Formatter): Formatter =
//      f << "trait " << module.name << " extends ScalanDsl { " <<
//            module.selfType.map { c => "self: %s =>".format(c) }
//
//    private def genEntityInterface(implicit f: Formatter): Formatter = {
//      f << "trait " << e.entName
//      e.tpeArgs.isEmpty match {
//        case false =>
//          f << "[" << e.tpeArgs << "]"
//        case _ =>
//      }
//      f << "{\n}"
//    }
  
    def getBaseTrait = {
      implicit val f = new Formatter { tabSize = 2 }
      val  emitter = new ScalaCodeEmitter
  
      f.toString
    }
  
    def getSeqTrait: String = ""
    def getStagedTrait: String = ""
  
    def getFileHeader = {
      val p = "package %s\n\n".format(module.packageName)
      val imports =
        """import scalan.lms.common.ProxyExp
          |import scalan.ScalanDsl
          |import scalan.common.Common
          |import Common._
          |import scalan.sequential.ScalanSeq
          |import scalan.staged.{StagedViews, ScalanStaged}
          |import reflect.SourceContext
          |
          |""".stripMargin
      p + imports
    }
  
    def generateCode: String = {
      val topLevel = List(
        getFileHeader,
        getBaseTrait,
        getSeqTrait,
        getStagedTrait
      )
      topLevel.mkString("\n")
    }
  }
}

object ScalanCodegen extends ScalanCodegen

