package scalan.meta

import java.io.File

case class CodegenConfig(srcPath: String,
                         proxyTrait: String,
                         entityFiles: List[String],
                         stagedViewsTrait: String,
                         isLite: Boolean,
                         isoNames: (String, String),
                         extraImports: List[String]
                        )

class EntityManagement(val config: CodegenConfig) extends ScalanCodegen { ctx =>

  case class EntityManager(name: String, filePath: String, entityDef: EntityModuleDef)

  private def entities(srcPath : String) = (config.entityFiles map {
    f =>
      val path = srcPath + "/" + f
      val d = parseEntity(path)
      (d.name, new EntityManager(d.name, path, d))
  }).toMap

  def generateAll(srcPath : String = config.srcPath) = {
    entities(srcPath).foreach { case (name, m) =>
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
