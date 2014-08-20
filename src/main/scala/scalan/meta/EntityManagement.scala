/**
 * User: Alexander Slesarenko
 * Date: 12/15/13
 */
package scalan.meta

import java.io.File

case class CodegenConfig(
                          srcPath: String,
                          entityFiles: List[String],
                          seqContextTrait: String,
                          stagedContextTrait: String,
                          extraImports: List[String]
                        )

class EntityManagement(val config: CodegenConfig) extends ScalanCodegen { ctx =>

  case class EntityManager(name: String, filePath: String, entityDef: SEntityModuleDef)

  private val entities = (config.entityFiles map {
    f =>
      val path = config.srcPath + "/" + f
      val d = parseEntityModule(path)
      (d.name, new EntityManager(d.name, path, d))
  }).toMap

  def generateAll() = {
    entities.foreach { case (name, m) =>
      val g = new EntityFileGenerator(m.entityDef)
      val implCode = g.getImplFile
      saveEntity(m.filePath, implCode)
    }
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
