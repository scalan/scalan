package scalan.meta

import java.io.File
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.StoreReporter
import scala.reflect.internal.util.SourceFile

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

    parse(file.getPath, tree)
  }

  def parseFile(source: SourceFile): compiler.Tree = {
    compiler.newUnitParser(new compiler.CompilationUnit(source)).parse()
  }
}
