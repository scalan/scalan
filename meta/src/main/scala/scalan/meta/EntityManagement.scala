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

class Parsers(val configs: List[CodegenConfig]) extends ScalanParsersEx[Global] {
  def getGlobal: Global = new Global(settings, reporter)
  implicit val context = new AstContext(configs)

  initCompiler()
}

class EnrichPipeline(implicit val context: AstContext) extends (SModuleDef => SModuleDef) {
  val moduleBuilder = new SModuleBuilder()
  import moduleBuilder._
  private val chain = scala.Function.chain(Seq(
    genClassesImplicits _
  ))
  override def apply(module: Module): Module = chain(module)
}

class EntityManagement[G <: Global](val parsers: ScalanParsers[G]) extends LazyLogging {
  import parsers._
  def configs = parsers.context.configs
  implicit def context = parsers.context
   
  case class EntityManager(name: String, file: File, module: SModuleDef, config: CodegenConfig)

  protected val entities = (for(c <- configs) yield {
    val file = FileUtil.file(c.srcPath, c.entityFile)
    try {
      inform(s"  parsing ${file} (relative to ${FileUtil.currentWorkingDir })")
      val module = parseEntityModule(file)(new ParseCtx(c.isVirtualized))
      val unitName = file.getName
      context.addModule(unitName, module)
      Some((c.name, new EntityManager(module.name, file, module, c)))
    } catch {
      case e: Exception =>
        val msg = s"Failed to parse file at $file (relative to ${FileUtil.currentWorkingDir })"
        inform(msg)
        logger.error(msg, e)
        None
    }
  }).flatten.toMap

  def createFileGenerator(codegen: MetaCodegen, module: SModuleDef, config: CodegenConfig) =
    new ModuleFileGenerator(codegen, module, config)

  val enrichPipeline = new EnrichPipeline()

  def generate(configName: String) = {
    entities.get(configName) match {
      case Some(man) =>
        println(s"  generating ${man.file}")
        val enrichedModule = enrichPipeline(man.module)
        val g = createFileGenerator(ScalanCodegen, enrichedModule, man.config)
        val implCode = g.emitImplFile
        saveEntity(man.file, implCode)
      case None =>
        logger.error(s"Cannot generate code for config '$configName' because it is not found.")
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
