/**
 * User: Alexander Slesarenko
 * Date: 12/15/13
 */
package scalan.meta

import java.io.File

import com.typesafe.scalalogging.LazyLogging

import scalan.util.FileUtil
import ScalanAst._
import scala.tools.nsc.Global

class EntityManagement(val config: CodegenConfig) extends ScalanParsersEx[Global] with LazyLogging {
  def getGlobal: Global = new Global(settings, reporter)
  implicit val context = new AstContext

  initCompiler()

  case class EntityManager(name: String, file: File, module: SModuleDef, config: CodegenConfig)

  protected val entities = config.entityFiles.flatMap { f =>
    val file = FileUtil.file(config.srcPath, f)
    try {
      val module = parseEntityModule(file, config.isVirtualized)
      val unitName = file.getName
      context.addModule(unitName, module)
      Some(new EntityManager(module.name, file, module, config))
    } catch {
      case e: Exception =>
        logger.error(s"Failed to parse file at $file (relative to ${FileUtil.currentWorkingDir})", e)
        None
    }
  }

  def getCodegen: MetaCodegen = ScalanCodegen

  def createFileGenerator(codegen: MetaCodegen, module: SModuleDef, config: CodegenConfig) =
    new ModuleFileGenerator(codegen, module, config)

  def generateAll() = {
    entities.foreach { man =>
      println(s"  generating ${man.file}")
      val g = createFileGenerator(getCodegen, man.module, man.config)
      val implCode = g.emitImplFile
      saveEntity(man.file, implCode)
    }
  }

  def saveEntity(file: File, implCode: String) = {
    val fileName = file.getName.split('.')(0)
    val folder = file.getParentFile
    val implFile = FileUtil.file(folder, "impl", s"${fileName}Impl.scala")

    implFile.mkdirs()

    FileUtil.write(implFile, implCode)
  }

}
