package scalan.compilation.lms.uni

import java.io.File
import java.net.{URLClassLoader, URL}

import scalan.common.Lazy
import scalan.compilation.lms.common.JNILmsOpsExp
import scalan.compilation.lms.cxx.sharedptr.CxxCodegen
import scalan.util.FileUtil
import scalan.{JNIExtractorOpsExp, ScalanCtxExp}
import scalan.compilation.GraphVizConfig
import scalan.compilation.language.MethodMappingDSL
import scalan.compilation.lms.scalac.{LmsCompilerScala, CommunityLmsCompilerScala}
import scalan.compilation.lms.source2bin.{Gcc, SbtConfig, Nsc, Sbt}
import scalan.compilation.lms._
import scalan.util.FileUtil._

/**
 * Created by adel on 5/12/15.
 */
trait LmsCompilerUni
  extends LmsCompilerScala
  with JNIExtractorOpsExp
  //extends CommunityLmsCompilerScala
  with CoreBridge with MethodMappingDSL with JNIBridge
{ self: ScalanCtxExp =>

//  case class CustomCompilerOutput(jar: URL)
  class LmsBackendUni extends CommunityLmsBackend with JNILmsOpsExp { self =>
    val nativeCodegen: CxxCodegen[self.type] = new CxxCodegen(self)
    val jniCallCodegen: JniCallCodegen[self.type] = new JniCallCodegen(self, nativeCodegen, "")
  }

  override val lms = new LmsBackendUni

  //var graph2: PGraph = new PGraph

  override def buildInitialGraph[A, B](func: Exp[A => B])(compilerConfig: CompilerConfig): PGraph = {
    val func2 = {
      val newLam = func match {
        case Def(lam: Lambda[a, b]) => {

          implicit val eA = lam.eA.asElem[a]
          val eJNIA =  JNITypeElem()(lam.eA).asElem[JNIType[a]]

          fun[JNIType[a], JNIType[b]] { arg: Rep[JNIType[a]] =>
            JNI_Pack(lam.self(JNI_Extract(arg)))(lam.eB) } (Lazy(eJNIA))
        }

        //case Def(lam: Lambda[a, b]) => fun[a, JNIType[b]] { arg: Rep[a] => JNI_Pack(lam.self(arg))(lam.eB) } (Lazy(lam.eA))
        case _ => !!!
      }

      //implicit val xB = oldRes.elem
      //val newGraph = new ProgramGraph(List(newLam), graph.mapping)
      //val newGraph = graph.transformOne(graph.roots.last, newLam)
      //val jLmsFunc = apply[ja, jb](newGraph)

      //val finalMirror = LmsMirror.empty.mirrorDefs(newGraph, newGraph.schedule)
      //val lmsFunc = finalMirror.funcMirror[a, jb](newLam)
      newLam
    }

    new PGraph(List(func, func2))

  }


  override protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, graphVizConfig: GraphVizConfig)
                                       (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]) = {
    Sbt.prepareDir(executableDir) //todo - check: is it sbt-specific function?

    implicit val eA = eInput
    val jInput = new JNITypeElem[A]
    implicit val eB = eOutput
    val jOutput = new JNITypeElem[B]

    /* LMS stuff */
    val jniCallCodegen = lms.jniCallCodegen
    val codegen = lms.nativeCodegen

    val (scalaFile, cxxFile) = {
      (createManifest(eInput), createManifest(eOutput), createManifest(jInput), createManifest(jOutput)) match {
        case (mA: Manifest[a], mB: Manifest[b], mjA: Manifest[ja], mjB: Manifest[jb]) =>

          //val jniCallCodegen =  new JniCallCodegen(self, lms.nativeCodegen, "")
          val lmsFunc0 = apply[a, b](graph)
          val finalMirror = LmsMirror.empty.mirrorDefs(graph, graph.schedule)
          val lmsFunc = finalMirror.funcMirror[a, b](graph.roots.head)


          val scalaFile = jniCallCodegen.createFile(lmsFunc, functionName, sourcesDir)(mA, mB)

          /*val lmsFunc2 = {
            val newLam = graph.roots.last match {
              case Def(lam: Lambda[a, b]) => {

                implicit val eA = lam.eA.asElem[a]
                implicit val eB = lam.eB.asElem[b]

                fun[JNIType[a], JNIType[b]] { arg: Rep[JNIType[a]] =>
                  JNI_Pack(lam.self(JNI_Extract(arg)))(eB) }
              }

              //case Def(lam: Lambda[a, b]) => fun[a, JNIType[b]] { arg: Rep[a] => JNI_Pack(lam.self(arg))(lam.eB) } (Lazy(lam.eA))
              case _ => !!!
            }
            //implicit val xB = oldRes.elem
            //val newGraph = new ProgramGraph(List(newLam), graph.mapping)
            val newGraph = graph.transformOne(graph.roots.last, newLam)
            val jLmsFunc = apply[ja, jb](newGraph)

            //val finalMirror = LmsMirror.empty.mirrorDefs(newGraph, newGraph.schedule)
            //val lmsFunc = finalMirror.funcMirror[a, jb](newLam)
            jLmsFunc
          }*/

          //val lmsFunc2 = JNI_Extract(lmsFunc)

          //val jInput2 = new Elem[JNIType[A]]
          val lmsFuncC = finalMirror.funcMirror[ja, jb](graph.roots.last)

          val cxxFile = codegen.createFile(lmsFuncC, jniCallCodegen.cppFunctionName(functionName), sourcesDir)(mjA, mjB)
          (scalaFile, cxxFile)
      }

    }

    val jarFile = file(executableDir.getAbsoluteFile, s"$functionName.jar")
    FileUtil.deleteIfExist(jarFile)
    val jarPath = jarFile.getAbsolutePath
    val mainClass : Option[String] = compilerConfig.sbt.mainPack match {
      case Some(mainPack) => Some(mainPack + "." +  functionName)
      case _ =>  None
    }
    val output: Option[Array[String]] = compilerConfig.scalaVersion match {
      case Some(scalaVersion) =>
        val dependencies:Array[String] = methodReplaceConf.flatMap(conf => conf.dependencies).toArray
        Some(Sbt.compile(sourcesDir, executableDir, functionName, compilerConfig, dependencies, scalaFile, jarPath))
      case None =>
        Nsc.compile(executableDir, functionName, compilerConfig.extraCompilerOptions.toList, scalaFile, jarPath)
        None
    }
    Gcc.compile(scalan.Base.config.getProperty("runtime.target"), executableDir, cxxFile, jniCallCodegen.cppLibraryName(functionName))
    CustomCompilerOutput(jarFile.toURI.toURL, mainClass, output)
  }


}
