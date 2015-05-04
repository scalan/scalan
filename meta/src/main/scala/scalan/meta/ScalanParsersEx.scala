package scalan.meta

import java.io.File
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.StoreReporter

trait ScalanParsersEx extends ScalanParsers {
  type Compiler = scala.tools.nsc.interactive.Global
  val settings = new Settings
  settings.embeddedDefaults(getClass.getClassLoader)
  settings.usejavacp.value = true
  val reporter = new StoreReporter
  val compiler: Compiler = new Global(settings, reporter)

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
}
