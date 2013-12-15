/**
 * User: Alexander Slesarenko
 * Date: 12/15/13
 */
package makro

import java.io.File

case class CodegenConfig(
                          srcPath: String,
                          proxyTrait: String,
                          entityFiles: List[String],
                          stagedViewsTrait: String,
                          emitSourceContext: Boolean
                        )

object CodegenConfig {
  val default = CodegenConfig(
    srcPath = "",
    proxyTrait = "",
    entityFiles = Nil,
    stagedViewsTrait = "StagedViews",
    emitSourceContext = false
  )
}

class EntityManagement(val config: CodegenConfig = CodegenConfig.default) extends ScalanCodegen { ctx =>

  case class EntityManager(name: String, filePath: String, entityDef: EntityModuleDef)

  private val entities = (config.entityFiles map {
    f =>
      val path = config.srcPath + "/" + f
      val d = parseEntity(path)
      (d.name, new EntityManager(d.name, path, d))
  }).toMap

  def generateAll() = {
    entities.foreach { case (name, m) =>
      val g = new EntityFileGenerator(m.entityDef)
      val implCode = g.getImplFile
      saveEntity(m.filePath, implCode)
    }
  }

  def parseEntity(filePath: String) = {
    val entityTemplate = readFile(filePath)
    parseEntityModule(entityTemplate)
  }

  def saveEntity(filePath: String, implCode: String) = {
    val file = new File(filePath)
    val fileName = file.getName.split('.')(0)
    val folder = file.getParentFile.getPath
    val implFolder = folder + "/impl"

    new File(implFolder).mkdirs()
    val implFile = s"$implFolder/${fileName}Impl.scala"

    writeFile(implFile, implCode)
  }

}
