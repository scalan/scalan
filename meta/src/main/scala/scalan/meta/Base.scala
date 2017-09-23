package scalan.meta

import java.util.Properties
import java.io.FileReader

case class CodegenConfig(name: String,
                         srcPath: String,
                         entityFiles: List[String],
                         entityTypeSynonyms: Map[String, String] = Map.empty,
                         baseContextTrait: String = "scalan.Scalan",
                         seqContextTrait: String = "scalan.ScalanDslStd",
                         stagedContextTrait: String = "scalan.ScalanDslExp",
                         extraImports: List[String] = List("scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}", "scalan.meta.ScalanAst._"),
                         isVirtualized: Boolean = true,
                         isStdEnabled: Boolean = true) {
  def isEntityFile(fileName: String) = entityFiles.contains(fileName)
}

object Base {
  lazy val config = {
    val prop = new Properties
    try {
      val reader = new FileReader("scalan.meta.properties")
      try {
        prop.load(reader)
      } finally {
        reader.close()
      }
    } catch {
      case _: Throwable => {}
    }
    prop.putAll(System.getProperties)
    prop
  }

  def !!!(msg: String) = {
    throw new IllegalStateException(msg)
  }
}