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

  val isNativeKey = MetaKey[Boolean]("isNative")

  override def buildInitialGraph[A, B](func: Exp[A => B])(compilerConfig: CompilerConfig): PGraph = {

    val conf = compilerConfig.nativeMethods
    conf.rootIsNative match {
      case true =>
        val func2 = func match {
            case Def(lam: Lambda[a, b]) => {

              implicit val eA = lam.eA.asElem[a]
              //val eJNIA =  JNITypeElem(eA).asElem[JNIType[a]]
              //val newFunc = mkLambda(func, true)()
              val res = fun[JNIType[a], JNIType[b]] { arg: Rep[JNIType[a]] =>
                val data = JNI_Extract(arg)
                val res = lam.self(data)
                JNI_Pack(res) (lam.eB)
              } //(Lazy(eJNIA))
              res
            }
            case _ => !!!
          }

        func2.setMetadata(isNativeKey)(true)

        new PGraph(List(func, func2))

      case _ => //todo check methodCalls
        new PGraph(func)
    }
  }


  override protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, graphVizConfig: GraphVizConfig)
                                       (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]) = {
    Sbt.prepareDir(executableDir) //todo - check: is it sbt-specific function?

    implicit val eA = eInput
    implicit val eB = eOutput

    val jniCallCodegen = lms.jniCallCodegen
    val codegen = lms.nativeCodegen

    val scalaFile = {
      (createManifest(eInput), createManifest(eOutput)) match {
        case (mA: Manifest[a], mB: Manifest[b]) =>

          val scalaFile = graph.roots.size match {
            case 0 => !!!("program graph is empty")
            case 1 =>  // just generate one scala file
              // call LmsMirror
              val lmsFunc = apply[a, b](graph)
              lms.codegen.createFile(lmsFunc, functionName, sourcesDir)(mA, mB)

            case _ => //generate some c++ files, and then generate scal file with native calls
              // such as in LmsMirror.apply
              val finalMirror = LmsMirror.empty.mirrorDefs(graph, graph.schedule)

              val scalanFuncC = graph.roots.filter(_.getMetadata(isNativeKey).getOrElse(false))
              for( f <- scalanFuncC) {
                val t = findDefinition(f).getOrElse(!!!("can not find definition for root element") )
                val (jInput, jOutput) = (t.rhs match {
                  case Lambda(_, _, x, y) =>
                    (x.elem, y.elem)
                }) //.asInstanceOf[(Elem[A], Elem[B])]
                (createManifest(jInput), createManifest(jOutput)) match {
                  case (mA: Manifest[a], mB: Manifest[b]) =>

                    val lmsFuncC = finalMirror.funcMirror[a, b](f)
                    val cxxFile = codegen.createFile(lmsFuncC, jniCallCodegen.cppFunctionName(functionName), sourcesDir)(mA, mB)
                    Gcc.compile(scalan.Base.config.getProperty("runtime.target"), executableDir, cxxFile, jniCallCodegen.cppLibraryName(functionName))
                  //todo make one library for all functions
                }
              }

              val scalanFuncList = graph.roots.filter( ! _.getMetadata(isNativeKey).getOrElse(false))
              val scalanFunc = scalanFuncList.size match {
                case 0 => !!!("package not contains main jvm-function")
                case 1 => scalanFuncList.last
                case _ => !!!("package should contains only one jvm-function")
              }
              val lmsFunc = finalMirror.funcMirror[a, b](scalanFunc)
              jniCallCodegen.createFile(lmsFunc, functionName, sourcesDir)(mA, mB)
          }

          scalaFile
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
    CustomCompilerOutput(List(scalaFile.getAbsolutePath), jarFile.toURI.toURL, mainClass, output)
  }


}
