package scalan
package compilation
package lms

import java.io._
import java.net.URLClassLoader

import scalan.util.{FileUtil, ProcessUtil}

trait LmsCompiler extends Compiler { self: ScalanCtxExp =>

  case class Config(extraCompilerOptions: Seq[String])

  implicit val defaultConfig = Config(Seq.empty)

  def makeBridge[A, B]: LmsBridge[A, B]

  def graphPasses(config: Config) = Seq(AllUnpackEnabler, AllInvokeEnabler)

  protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, emitGraphs: Boolean)
                                       (config: Config, eInput: Elem[A], eOutput: Elem[B]) = {
    /* LMS stuff */

    val outputSource = new File(sourcesDir, functionName + ".scala")

    (createManifest(eInput), createManifest(eOutput)) match {
      case (mA: Manifest[a], mB: Manifest[b]) =>
        val bridge = makeBridge[a, b]
        val facade = bridge.getFacade(graph.asInstanceOf[bridge.scalan.PGraph])
        val codegen = bridge.lms.codegen

        FileUtil.withFile(outputSource) { writer =>
          codegen.emitSource[a, b](facade.apply, functionName, writer)(mA, mB)
          codegen.emitDataStructures(writer)
        }
    }

    val command = Seq("scalac", "-d", jarFile(functionName, executableDir).getAbsolutePath) ++ config.extraCompilerOptions :+
      outputSource.getAbsolutePath

    ProcessUtil.launch(sourcesDir, command: _*)
  }

  protected def doExecute[A, B](executableDir: File, functionName: String, input: A)
                               (config: Config, eInput: Elem[A], eOutput: Elem[B]): B = {
    val url = jarFile(functionName, executableDir).toURI.toURL
    // ensure Scala library is available
    val classLoader = new URLClassLoader(scala.Array(url), classOf[_ => _].getClassLoader)
    val cls = classLoader.loadClass(functionName)
    val argumentClass = eInput.classTag.runtimeClass
    val method = cls.getMethod("apply", argumentClass)
    val result = method.invoke(cls.newInstance(), input.asInstanceOf[AnyRef])
    result.asInstanceOf[B]
  }

  private def jarFile(functionName: String, executableDir: File) =
    FileUtil.file(executableDir.getAbsoluteFile, s"$functionName.jar")

  def createManifest[T]: PartialFunction[Elem[T], Manifest[_]] = {
    // Doesn't work for some reason, produces int instead of Int
    //    implicit val typeTag = eA.tag
    //    implicit val classTag = eA.classTag
    //    manifest[T]
    case UnitElement => Manifest.Unit
    case BoolElement => Manifest.Boolean
    case ByteElement => Manifest.Byte
    case ShortElement => Manifest.Short
    case IntElement => Manifest.Int
    case LongElement => Manifest.Long
    case FloatElement => Manifest.Float
    case DoubleElement => Manifest.Double
    case StringElement => manifest[String]
    case PairElem(eFst, eSnd) =>
      Manifest.classType(classOf[(_, _)], createManifest(eFst), createManifest(eSnd))
    case SumElem(eLeft, eRight) =>
      Manifest.classType(classOf[Either[_, _]], createManifest(eLeft), createManifest(eRight))
    case el: FuncElem[_, _] =>
      Manifest.classType(classOf[_ => _], createManifest(el.eDom), createManifest(el.eRange))
    case el: ArrayElem[_] =>
      Manifest.arrayType(createManifest(el.eItem))
    case el: ListElem[_] ⇒
      Manifest.classType(classOf[List[_]], createManifest(el.eItem))
    case el => ???(s"Don't know how to create manifest for $el")
  }

}
