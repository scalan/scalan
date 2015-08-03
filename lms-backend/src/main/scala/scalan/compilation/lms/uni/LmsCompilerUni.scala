package scalan.compilation.lms.uni

import java.io.File

import scalan.compilation.GraphVizConfig
import scalan.compilation.lms._
import scalan.compilation.lms.common.JNILmsOpsExp
import scalan.compilation.lms.cxx.sharedptr.CxxCodegen
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.compilation.lms.source2bin.{Gcc, Nsc, Sbt}
import scalan.util.FileUtil
import scalan.util.FileUtil._
import scalan.{Base, JNIExtractorOpsExp, ScalanCommunityDslExp}

//  case class CustomCompilerOutput(jar: URL)
class LmsBackendUni extends CommunityLmsBackend with JNILmsOpsExp { self =>
  val nativeCodegen: CxxCodegen[self.type] = new CxxCodegen(self)
  val jniCallCodegen: JniCallCodegen[self.type] = new JniCallCodegen(self, nativeCodegen, "")
  // todo cppFunctionName and cppLibraryName should be moved from jniCallCodegen for use base codegen below (in doBuildExecutable), without information about subtype of codegen (native or jniCall)
}

/**
 * Created by adel on 5/12/15.
 */
trait LmsCompilerUni extends CommunityLmsCompilerScala with JNIBridge {
  override val scalan: ScalanCommunityDslExp with JNIExtractorOpsExp
  import scalan._

  val lms: LmsBackendUni

  lazy val marker = new SymbolsMarkerForSelectCodegen[scalan.type](scalan)

  override def buildInitialGraph[A, B](func: Exp[A => B])(compilerConfig: CompilerConfig): PGraph = {

    val conf = compilerConfig.nativeMethods
    val needForAdapt = marker.mark(func, marker.defaultCompilationPipelineContext, conf)

    def adapt(func: Exp[_]):List[Exp[_]] = {
      func.getMetadata(marker.codegenChangeKey) match {
        case Some(key) => {
          val adapter = KnownCodegens.getAdapterByString[scalan.type](scalan, key /*"Scala2Cxx"*/)
          func match {
            case Def(lam: Lambda[a, b]) =>
              val (master, slave) = KnownCodegens.pairFromString(key)//BackendCake
              val lam2 = adapter.adapt[a, b](lam)
              func.setMetadata(marker.codegenKey)(KnownCodegens.toString(master))
              lam2.setMetadata(marker.codegenKey)(KnownCodegens.toString(slave))
              List(lam, lam2)
            case _ => !!!("I can adapt only functions")
          }
        }
        case None => List(func)
      }
    }

    needForAdapt.length match {
      case 0 =>
        new PGraph(func)
      case _ => {
        val rootList = (needForAdapt flatMap ( f => adapt(f)))
        new PGraph(rootList)
      }
    }
  }


  override protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, graphVizConfig: GraphVizConfig)
                                       (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]) = {
    Sbt.prepareDir(executableDir) //todo - check: is it sbt-specific function?

    implicit val eA = eInput
    implicit val eB = eOutput

    val scalaFile = {
      (createManifest(eInput), createManifest(eOutput)) match {
        case (mA: Manifest[a], mB: Manifest[b]) =>

          val scalaFile = graph.roots.size match {
            case 0 => !!!("program graph is empty")
            case 1 =>  // just generate one scala file
              // call LmsMirror
              val lmsFunc = apply[a, b](graph)
              lms.codegen.createFile(lmsFunc, functionName, sourcesDir)(mA, mB)

            case _ => //generate some c++ files, and then generate scala file with native calls
              // such as in LmsMirror.apply
              val finalMirror = LmsMirror.empty.mirrorDefs(graph, graph.schedule)

              val jniCallCodegen = lms.jniCallCodegen
              val codegen = lms.nativeCodegen

              val scalanFuncC = graph.roots.filter( _.getMetadata(marker.codegenKey).getOrElse("") == KnownCodegens.Cxx.toString)
              for( f <- scalanFuncC) {
                val t = findDefinition(f).getOrElse(!!!("can not find definition for root element") )
                val (jInput, jOutput) = t.rhs match {
                  case Lambda(_, _, x, y) =>
                    (x.elem, y.elem)
                } //.asInstanceOf[(Elem[A], Elem[B])]
                (createManifest(jInput), createManifest(jOutput)) match {
                  case (mA: Manifest[a], mB: Manifest[b]) =>

                    val lmsFuncC = finalMirror.funcMirror[a, b](f)
                    val cxxFile = codegen.createFile(lmsFuncC, jniCallCodegen.cppFunctionName(functionName), sourcesDir)(mA, mB)
                    Gcc.compile(Base.config.getProperty("runtime.target"), executableDir, cxxFile, jniCallCodegen.cppLibraryName(functionName))
                  //todo make one library for all functions
                }
              }

              val scalanFuncList = graph.roots.filter( _.getMetadata(marker.codegenKey).getOrElse("") == KnownCodegens.Scala.toString)
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
