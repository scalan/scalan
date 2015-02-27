package scalan.compilation.lms.scalac

import java.io.File

import scalan.ScalanCommunityDslExp
import scalan.compilation.GraphVizConfig
import scalan.compilation.lms.CommunityBridgeScala
import scalan.util.{ExtensionFilter, FileUtil}

trait CommunityLmsCompilerScala extends LmsCompilerScala with CommunityBridgeScala { self: ScalanCommunityDslExp =>

  override protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, graphVizConfig: GraphVizConfig)
                                                (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]) = {

    val libsDir = FileUtil.file(FileUtil.currentWorkingDir, lib)
    val executableLibsDir = FileUtil.file(executableDir, lib)
    // unused
    var mainJars = methodReplaceConf.libPaths.map {
      j => FileUtil.file(libsDir, j).getAbsolutePath
    }
    val dir = FileUtil.listFiles(libsDir, ExtensionFilter("jar"))
    dir.foreach(f => {
      mainJars = mainJars + f.getAbsolutePath
      FileUtil.copyToDir(f, executableLibsDir)
    })

    super.doBuildExecutable[A, B](sourcesDir, executableDir, functionName, graph, graphVizConfig)(compilerConfig, eInput, eOutput)
  }

  override def newObj[A: Manifest](symMirr: SymMirror, aClass: Class[_], args: Seq[Rep[_]]): lms.Exp[A] = {
    lms.newObj[A](aClass.getCanonicalName, args.map(v => symMirr(v.asInstanceOf[Exp[_]])))
  }

}
