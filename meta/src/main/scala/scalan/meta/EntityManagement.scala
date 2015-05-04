/**
 * User: Alexander Slesarenko
 * Date: 12/15/13
 */
package scalan.meta

import java.io.File
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.StoreReporter
import com.typesafe.scalalogging.slf4j.LazyLogging
import scalan.util.FileUtil
import ScalanAst._

class EntityManagement(val config: CodegenConfig) extends ScalanParsers with LazyLogging {
  type Compiler = scala.tools.nsc.interactive.Global
  val settings = new Settings
  settings.embeddedDefaults(getClass.getClassLoader)
  settings.usejavacp.value = true
  val reporter = new StoreReporter
  val compiler: Compiler = new Global(settings, reporter)

  case class EntityManager(name: String, file: File, entityDef: SEntityModuleDef, config: CodegenConfig)

  def parseEntityModule(file: File) = {
    val source = compiler.getSourceFile(file.getPath)
    val tree = compiler.parseTree(source)
    tree match {
      case pd: compiler.PackageDef =>
        entityModule(pd)
      case tree =>
        throw new Exception(s"Unexpected tree in file $file:\n\n$tree")
    }
  }

  private val entities = config.entityFiles.flatMap { f =>
    val file = FileUtil.file(config.srcPath, f)
    try {
      val d = parseEntityModule(file)
      Some(new EntityManager(d.name, file, d, config))
    } catch {
      case e: Exception =>
        logger.error(s"Failed to parse file at $file (relative to ${FileUtil.currentWorkingDir})", e)
        None
    }
  }

  def generateAll() = {
    entities.foreach { m =>
      println(s"  generating ${m.file}")
      val g = new ScalanCodegen.EntityFileGenerator(m.entityDef, m.config)
      val implCode = g.getImplFile
      saveEntity(m.file, implCode)
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
