package scalan.compilation.lms

import java.io._
import java.net.URLClassLoader

import scalan.compilation.Backend
import scalan.compilation.GraphVizExport
import scalan.linalgebra.VectorsDslExp
import scalan.community.ScalanCommunityExp
import scalan.util.{FileUtil, ProcessUtil}

trait LmsBackend extends Backend { self: ScalanCommunityExp with GraphVizExport with VectorsDslExp =>

  case class Config(extraCompilerOptions: Seq[String])

  implicit val defaultConfig = Config(Seq.empty)

  def makeBridge[A, B]: LmsBridge[A, B] = new LmsBridge[A, B] {
    val scalan = self
  }

  protected def doBuildExecutable[A,B](sourcesDir: File, executableDir: File, functionName: String, func: Exp[A => B], emitGraphs: Boolean)
                                      (config: Config, eInput: Elem[A], eOutput: Elem[B]) = {
    if (emitGraphs) {
      val dotFile = new File(sourcesDir, functionName + ".dot")
      this.emitDepGraph(func, dotFile, false)
    }

    val g0 = new PGraph(List(func))

    /* LMS stuff */

    val outputSource = new File(sourcesDir, functionName + ".scala")

    func.elem match {
      case el:FuncElem[_,_] =>
        (createManifest(el.eDom), createManifest(el.eRange)) match {
          case (mA:Manifest[a], mB:Manifest[b]) =>
            val bridge = makeBridge[a, b]
            val facade = bridge.getFacade(g0.asInstanceOf[bridge.scalan.PGraph])
            val codegen = facade.codegen

            FileUtil.withFile(outputSource) { writer =>
              codegen.emitSource[a, b](facade.apply, functionName, writer)(mA, mB)
              codegen.emitDataStructures(writer)
            }
        }
    }

    val command = Seq("scalac", "-d", jarPath(functionName, executableDir)) ++ config.extraCompilerOptions :+
      outputSource.getAbsolutePath

    ProcessUtil.launch(sourcesDir, command: _*)
  }

  protected def doExecute[A, B](executableDir: File, functionName: String, input: A)
                               (config: Config, eInput: Elem[A], eOutput: Elem[B]): B = {
    val url = new File(jarPath(functionName, executableDir)).toURI.toURL
    // ensure Scala library is available
    val classLoader = new URLClassLoader(scala.Array(url), classOf[_ => _].getClassLoader)
    val cls = classLoader.loadClass(functionName)
    val argumentClass = eInput.classTag.runtimeClass
    val method = cls.getMethod("apply", argumentClass)
    val result = method.invoke(cls.newInstance(), input.asInstanceOf[AnyRef])
    result.asInstanceOf[B]
  }

  private def jarPath(functionName: String, executableDir: File) =
    s"${executableDir.getAbsolutePath}/$functionName.jar"

  def createManifest[T](eA: Elem[T]): Manifest[_] = {
    // Doesn't work for some reason, produces int instead of Int
    //    implicit val typeTag = eA.tag
    //    implicit val classTag = eA.classTag
    //    manifest[T]
    val m = eA match {
      case UnitElement => Manifest.Unit
      case ByteElement => Manifest.Byte
      case IntElement => Manifest.Int
      case FloatElement => Manifest.Float
      case DoubleElement => Manifest.Double
      case StringElement => manifest[String]
      case PairElem(eFst, eSnd) =>
        Manifest.classType(classOf[(_, _)], createManifest(eFst), createManifest(eSnd))
      case SumElem(eLeft, eRight) =>
        Manifest.classType(classOf[Either[_, _]], createManifest(eLeft), createManifest(eRight))
      case el: FuncElem[_,_] =>
        Manifest.classType(classOf[_ => _], createManifest(el.eDom), createManifest(el.eRange))
      case el: ArrayElem[_] =>
        Manifest.arrayType(createManifest(el.eItem))
      case el => ???(s"Don't know how to create manifest for $el")
    }
    m
  }
}
