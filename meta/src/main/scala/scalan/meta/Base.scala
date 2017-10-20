package scalan.meta

import java.util.Properties
import java.io.{FileReader, File}

import scalan.util.FileUtil

case class MetaConfig(name: String,
                      srcPath: String, // the base path to where root package is located
                      resourcePath: String,
                      entityFile: String, // the package path and file name
                      baseContextTrait: String = "scalan.Scalan",
                      extraImports: List[String] = List("scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}", "scalan.meta.ScalanAst._"),
                      isVirtualized: Boolean = true,
                      isStdEnabled: Boolean = true) {
  def getFile: File = FileUtil.file(srcPath, entityFile)
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