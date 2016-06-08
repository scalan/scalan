package scalan.compilation.lms.cxx

import java.io._

import scalan.ScalanDslExp
import scalan.compilation.GraphVizConfig
import scalan.compilation.lms.LmsCompiler
import scalan.compilation.lms.cxx.sharedptr.{CoreLmsBridgeCxx, CoreCxxShptrLmsBackend}
import scalan.compilation.lms.source2bin.Gcc
import scalan.util.{ProcessUtil, FileUtil}

class LmsCompilerCxx[+ScalanCake <: ScalanDslExp](_scalan: ScalanCake) extends LmsCompiler(_scalan) with CoreLmsBridgeCxx {
  import scalan._
  val lms = new CoreCxxShptrLmsBackend

  type CustomCompilerOutput = Unit

  type CompilerConfig = Unit
  implicit val defaultCompilerConfig = ()

  override protected def doBuildExecutable[A,B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, graphVizConfig: GraphVizConfig)
                                      (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]) = {
    val sourceFile = emitSource(sourcesDir, functionName, graph, eInput, eOutput, graphVizConfig)

    val makefileContents = this.makefileContents(sourceFile, executableDir, functionName, compilerConfig)
    val makefile = FileUtil.file(sourcesDir, "Makefile")
    FileUtil.write(makefile, makefileContents)

    val command = Seq("make")
    ProcessUtil.launch(sourcesDir, command: _*)
  }

  // Any other arguments?
  protected def makefileContents(sourceFile: File, executableDir: File, functionName: String, compilerConfig: CompilerConfig): String = {
    val libFileName = System.mapLibraryName(functionName)
    val sourceFileName = sourceFile.getName
    val compileCommand = Gcc.compileCommand(executableDir, sourceFile.getParentFile, sourceFile, functionName).mkString(" ")
    s"""all: $libFileName
        |
        |$libFileName: $sourceFileName
        |""".stripMargin + "\t" + compileCommand // TODO how to better put tabs here?
  }

  // TODO currently not executed
  override protected def doExecute[A, B](compilerOutput: CompilerOutput[A, B], input: A): B = {
//    val url = new File(jarPath(functionName, executableDir)).toURI.toURL
//    // ensure Scala library is available
//    val classLoader = new URLClassLoader(scala.Array(url), classOf[_ => _].getClassLoader)
//    val cls = classLoader.loadClass(functionName)
//    val argumentClass = eInput.runtimeClass
//    val method = cls.getMethod("apply", argumentClass)
//    val result = method.invoke(cls.newInstance(), input.asInstanceOf[AnyRef])
//    result.asInstanceOf[B]
    null.asInstanceOf[B]
  }
}
