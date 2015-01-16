package scalan.compilation.lms

import java.io._
import java.net.URLClassLoader

import scalan.JNIExtractorOpsExp
import scalan.compilation.Compiler
import scalan.compilation.GraphVizExport
import scalan.linalgebra.VectorsDslExp
import scalan.community.ScalanCommunityExp
import scalan.util.{FileUtil, ProcessUtil}

trait LmsCompilerCXX extends LmsCompiler with JNIExtractorOpsExp { self: ScalanCommunityExp with GraphVizExport with VectorsDslExp =>

  override def createManifest[T](eA: Elem[T]): Manifest[_] = eA match {
    case el: JNITypeElem[_] =>
      Manifest.classType(classOf[scalan.compilation.lms.JNILmsOps#JNIType[_]], createManifest(el.tElem))
    case _ =>
      super.createManifest(eA)
  }

  protected def doBuildExecutable[A,B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, emitGraphs: Boolean)
                                      (config: Config, eInput: Elem[A], eOutput: Elem[B]) = {
    /* LMS stuff */

    val outputSource = new File(sourcesDir, functionName + ".cxx")

    (createManifest(eInput), createManifest(eOutput)) match {
      case (mA: Manifest[a], mB: Manifest[b]) =>
        val bridge = makeBridge[a, b]
        val facade = bridge.getFacade(graph.asInstanceOf[bridge.scalan.PGraph])
        val codegen = bridge.lms.codegenCXX

        FileUtil.withFile(outputSource) { writer =>
          codegen.emitSource[a, b](facade.apply, functionName, writer)(mA, mB)
//          val s = bridge.lms.fresh[a](mA)
//          val body = codegen.reifyBlock(facade.apply(s))(mB)
//          codegen.emitSource(List(s), body, functionName, writer)(mB)
//          val bridge.lms.TP(sym,_) = bridge.lms.globalDefs.last
//          codegen.emitDepGraph( sym, new File( sourcesDir, functionName + "-LMS.dot" ).getAbsolutePath )
          codegen.emitDataStructures(writer)
        }
    }

//    val command = Seq("scalac", "-d", jarPath(functionName, executableDir)) ++ config.extraCompilerOptions :+
//      outputSource.getAbsolutePath
//
    val command = Seq("make")
    ProcessUtil.launch(new File(sourcesDir,"release"), command: _*)
  }

  protected def doExecute[A, B](executableDir: File, functionName: String, input: A)
                               (config: Config, eInput: Elem[A], eOutput: Elem[B]): B = {
//    val url = new File(jarPath(functionName, executableDir)).toURI.toURL
//    // ensure Scala library is available
//    val classLoader = new URLClassLoader(scala.Array(url), classOf[_ => _].getClassLoader)
//    val cls = classLoader.loadClass(functionName)
//    val argumentClass = eInput.classTag.runtimeClass
//    val method = cls.getMethod("apply", argumentClass)
//    val result = method.invoke(cls.newInstance(), input.asInstanceOf[AnyRef])
//    result.asInstanceOf[B]
    null.asInstanceOf[B]
  }

  private def jarPath(functionName: String, executableDir: File) =
    s"${executableDir.getAbsolutePath}/$functionName.jar"
}
