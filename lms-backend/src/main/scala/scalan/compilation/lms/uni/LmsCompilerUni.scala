package scalan.compilation.lms.uni

import java.io.File
import java.net.URL

import scalan.compilation.lms._
import scalan.compilation.lms.common.JNILmsOpsExp
import scalan.compilation.lms.cxx.sharedptr.CxxCodegen
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.compilation.lms.source2bin.Gcc
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
class LmsCompilerUni[+ScalanCake <: ScalanCommunityDslExp with JNIExtractorOpsExp](_scalan: ScalanCake) extends CommunityLmsCompilerScala[ScalanCake](_scalan) with JNIBridge {
  import scalan._

  override val lms = new LmsBackendUni

  lazy val marker = new SymbolsMarkerForSelectCodegen[scalan.type](scalan)

  override def buildInitialGraph[A, B](func: Exp[A => B])(compilerConfig: CompilerConfig): PGraph = {

    val conf = compilerConfig.nativeMethods
    val needForAdapt = marker.mark(func, marker.defaultCompilationPipelineContext, conf)

    def adapt(func: Exp[_]):List[Exp[_]] = {
      func.getMetadata(marker.codegenChangeKey) match {
        case Some(key) => {
          val adapter = KnownCodegens.getAdapterByString[scalan.type](scalan, key /*"Scala2Cxx"*/)
          func match {
            case func: scalan.Exp[Function1[a, b] @unchecked] =>
              val (master, slave) = KnownCodegens.pairFromString(key)//BackendCake
              val adapted = adapter.adapt(func)
              func.setMetadata(marker.codegenKey)(KnownCodegens.toString(master))
              adapted.setMetadata(marker.codegenKey)(KnownCodegens.toString(slave))
              List(func, adapted)
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

  val runtimeTargetDir = Base.config.getProperty("runtime.target")

  override def getClassLoader(jarUrls: Array[URL]): ClassLoader =
    new NativeCopyLoader(Seq.empty, Seq(new File(runtimeTargetDir)), jarUrls, getClass.getClassLoader)

  override def emitSource[A, B](sourcesDir: File, functionName: String, graph: PGraph, eInput: Elem[A], eOutput: Elem[B]) = {
    (elemToManifest(eInput), elemToManifest(eOutput)) match {
      case (mA: Manifest[a], mB: Manifest[b]) =>
        graph.roots.size match {
          case 0 => !!!("program graph is empty")
          case 1 => // just generate one scala file
            // call LmsMirror
            val lmsFunc = apply[a, b](graph)
            lms.codegen.createFile(lmsFunc, functionName, sourcesDir)(mA, mB)

          case _ => //generate some c++ files, and then generate scala file with native calls
            // such as in LmsMirror.apply
            val finalMirror = LmsMirror.empty.mirrorDefs(graph, graph.schedule)

            val cxxFunctions = findRootsForCodegen(graph, KnownCodegens.Cxx)
            for (f <- cxxFunctions) {
              emitCSource(sourcesDir, functionName, finalMirror, f)
            }

            val scalaFunctions = findRootsForCodegen(graph, KnownCodegens.Scala)
            scalaFunctions match {
              case Seq(scalaFunction) =>
                val lmsFunc = finalMirror.funcMirror[a, b](scalaFunction)
                lms.jniCallCodegen.createFile(lmsFunc, functionName, sourcesDir)(mA, mB)
              case Seq() => !!!("There are no function marked for JVM code generation")
              case _ => !!!("There must be exactly one function marked for JVM code generation")
            }
        }
    }
  }

  private def findRootsForCodegen(graph: PGraph, codegen: KnownCodegens.CodegenType) =
    graph.roots.filter(_.getMetadata(marker.codegenKey).getOrElse("") == codegen.toString)

  private def emitCSource(sourcesDir: File, functionName: String, finalMirror: LmsMirror, f: scalan.Exp[_]) = {
    val (jInput, jOutput) = f match {
      case Def(lam: Lambda[_, _]) => (lam.x.elem, lam.y.elem)
    }
    (elemToManifest(jInput), elemToManifest(jOutput)) match {
      case (mA: Manifest[a], mB: Manifest[b]) =>

        val lmsFuncC = finalMirror.funcMirror[a, b](f)
        val cxxFile = lms.nativeCodegen.createFile(lmsFuncC, lms.jniCallCodegen.cppFunctionName(functionName), sourcesDir)(mA, mB)
        Gcc.compile(runtimeTargetDir, sourcesDir, cxxFile, lms.jniCallCodegen.cppLibraryName(functionName))
      //todo make one library for all functions
    }
  }
}
