package makro

import scala.io.Source
import java.io.{PrintWriter, File}

/**
 * User: Alexander Slesarenko   
 * Date: 12/1/13
 */
object BoilerplateTool extends App with ScalanCodegen {

  val sourcePrefix = "src/main/scala"
  val entityFiles = List(
    "scalan/rx/Reactive.scala"
    //, "scalan/rx/Trees.scala"
  )

  def readFile(name: String): String =
    Source.fromFile(name).getLines().toIterator.mkString("\n")

  def writeFile(name: String, text: String) = {
    val p = new PrintWriter(name)
    p.print(text)
    p.close()
  }


  def genEntity(filePath: String) = {
    val entityTemplate = readFile(filePath)
    val d = parseEntityModule(entityTemplate)
    val gen = new EntityFileGenerator(d)

    val file = new File(filePath)
    val fileName = file.getName.split('.')(0)
    val folder = file.getParentFile.getPath
    val implFolder = folder + "/impl"
    new File(implFolder).mkdirs()

    val dslFile = s"$folder/${fileName}Dsl.scala"
    val implFile = s"$implFolder/${fileName}Impl.scala"

    val dslCode = gen.getDslFile
    writeFile(dslFile, dslCode)

    val implCode = gen.getImplFile
    writeFile(implFile, implCode)
  }

  override def main(args: Array[String]) = {
    super.main(args)

    entityFiles foreach {f =>
      genEntity(sourcePrefix + "/" + f)
    }

  }
}
