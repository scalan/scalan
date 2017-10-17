package scalan

import scala.reflect.internal.util.BatchSourceFile
import scalan.meta.{Parsers, BoilerplateToolRun}
import scalan.meta.ScalanAst.SModuleDef
import scalan.util.FileUtil

abstract class ModuleInfo(sourceFileName: String) {
  def moduleDef: SModuleDef = {
    val sourceCode = FileUtil.readAndCloseStream(getClass.getClassLoader.getResourceAsStream(sourceFileName))
    val parsers = new Parsers(BoilerplateToolRun.allConfigs)
    import parsers._
    val sourceFile = new BatchSourceFile(sourceFileName, sourceCode)
    val tree = parseFile(sourceFile)
    val module = moduleDefFromTree(sourceFileName, tree)(new ParseCtx(true))
    module
  }
}
