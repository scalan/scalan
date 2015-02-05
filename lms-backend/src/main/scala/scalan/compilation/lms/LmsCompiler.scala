package scalan
package compilation
package lms

import java.io._
import java.net.URLClassLoader
import scalan.collections._

import scalan.util.{StringUtil, FileUtil, ProcessUtil}
import scala.tools.nsc.Global
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.StoreReporter

trait LmsCompiler extends Compiler { self: ScalanCtxExp =>

  /**
   * If scalaVersion is None, uses scala-compiler.jar
   *
   * Otherwise uses SBT to compile with the desired version
   */
  case class Config(scalaVersion: Option[String], extraCompilerOptions: Seq[String])

  implicit val defaultConfig = Config(None, Seq.empty)

  def makeBridge[A, B]: LmsBridge[A, B]

  def graphPasses(config: Config) = Seq(AllUnpackEnabler, AllInvokeEnabler)

  protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, emitGraphs: Boolean)
                                       (config: Config, eInput: Elem[A], eOutput: Elem[B]) = {
    /* LMS stuff */

    val outputSource = new File(sourcesDir, functionName + ".scala")
    val buildSbtFile = new File(sourcesDir, "build.sbt")

    (createManifest(eInput), createManifest(eOutput)) match {
      case (mA: Manifest[a], mB: Manifest[b]) =>
        val bridge = makeBridge[a, b]
        val facade = bridge.getFacade(graph.asInstanceOf[bridge.scalan.PGraph])
        val codegen = bridge.lms.codegen

        FileUtil.withFile(outputSource) { writer =>
          codegen.emitSource[a, b](facade.apply, functionName, writer)(mA, mB)
          codegen.emitDataStructures(writer)
        }

        val jarPath = jarFile(functionName, executableDir).getAbsolutePath

        config.scalaVersion match {
          case Some(scalaVersion) =>
            val buildSbtText =
              s"""name := "$functionName"
                 |
                 |scalaVersion := "$scalaVersion"
                 |
                 |artifactPath in Compile in packageBin := "$jarPath"
                 |
                 |scalacOptions ++= Seq(${config.extraCompilerOptions.map(StringUtil.quote).mkString(", ")})
                 |""".stripMargin

            FileUtil.write(buildSbtFile, buildSbtText)

            val command = Seq("sbt", "package")

            ProcessUtil.launch(sourcesDir, command: _*)
          case None =>
            val settings = new Settings
            settings.usejavacp.value = true
            // necessary to lauch compiler
            // see http://stackoverflow.com/questions/27934282/object-scala-in-compiler-mirror-not-found-running-scala-compiler-programatical
            settings.embeddedDefaults[LmsCompiler]
            val compilerOptions = "-d" :: jarPath :: config.extraCompilerOptions.toList
            settings.processArguments(compilerOptions, false)
            val reporter = new StoreReporter
            val compiler: Global = new Global(settings, reporter)
            val run = new compiler.Run
            run.compile(List(outputSource.getAbsolutePath))
        }

    }
  }

  protected def doExecute[A, B](executableDir: File, functionName: String, input: A)
                               (config: Config, eInput: Elem[A], eOutput: Elem[B]): B = {
    val url = jarFile(functionName, executableDir).toURI.toURL
    // ensure Scala library is available
    val classLoader = new URLClassLoader(Array(url), classOf[_ => _].getClassLoader)
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
    case CharElement => Manifest.Char
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
    case el: ArrayBufferElem[_] =>
      Manifest.classType(classOf[scala.collection.mutable.ArrayBuilder[_]], createManifest(el.eItem))
    case el: ListElem[_] ⇒
      Manifest.classType(classOf[List[_]], createManifest(el.eItem))
    case el: PMapElem[_,_] =>
      Manifest.classType(classOf[java.util.HashMap[_,_]], createManifest(el.eKey), createManifest(el.eValue))
    case el => ???(s"Don't know how to create manifest for $el")
  }

}
