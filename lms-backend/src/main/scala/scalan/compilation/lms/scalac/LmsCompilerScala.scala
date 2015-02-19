package scalan
package compilation
package lms
package scalac

import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.StoreReporter
import scalan.compilation.language.MethodMapping
import scalan.util.{ExtensionFilter, FileUtil, ProcessUtil, StringUtil}
import java.io._
import java.net.{URL, URLClassLoader}
import scalan.util.FileUtil.{file, copyToDir}

trait LmsCompilerScala extends LmsCompiler with CoreBridge with MethodMapping { self: ScalanCtxExp =>
  /**
   * If scalaVersion is None, uses scala-compiler.jar
   *
   * Otherwise uses SBT to compile with the desired version
   */
  case class CompilerConfig(scalaVersion: Option[String], extraCompilerOptions: Seq[String], mainPack: String = null, extraClasses : Array[String] = Array.empty[String])

  implicit val defaultCompilerConfig = CompilerConfig(None, Seq.empty)

  case class CustomCompilerOutput(jars: Array[URL], mainPack: String = null)

  def graphPasses(compilerConfig: CompilerConfig) = Seq(AllUnpackEnabler, AllInvokeEnabler)

  val lib = "lib"

  protected def doBuildExecutable[A, B](sourcesDir: File, executableDir: File, functionName: String, graph: PGraph, graphVizConfig: GraphVizConfig)
                                       (compilerConfig: CompilerConfig, eInput: Elem[A], eOutput: Elem[B]) = {
    /* LMS stuff */
    val buildSbtFile = new File(sourcesDir, "build.sbt")

    val sourceFile = emitSource(sourcesDir, "scala", functionName, graph, eInput, eOutput)

    val libsDir = file(FileUtil.currentWorkingDir, lib)
    val executableLibsDir = file(executableDir, lib)

    var jars = methodReplaceConf.libPaths.map {
      j => file(libsDir, j).getAbsolutePath
    }
    val dir = FileUtil.listFiles(libsDir, ExtensionFilter("jar"))
    dir.foreach(f => {
      jars = jars + f.getAbsolutePath
      copyToDir(f, executableLibsDir)
    })

    val jarFile = file(executableDir.getAbsoluteFile, s"$functionName.jar")
    val jarPath = jarFile.getAbsolutePath
    FileUtil.deleteIfExist(jarFile)

    val logFile = file(executableDir.getAbsoluteFile, s"$functionName.log")
    FileUtil.deleteIfExist(logFile)

    compilerConfig.scalaVersion match {
      case Some(scalaVersion) =>
        compilerConfig.mainPack match {
          case mainPack: String =>
            val mainClass: String = mainPack + ".run"
            val jar = s"$functionName.jar"
            val src = file(executableDir, "src", "main", "scala")
            val f = file(src, mainPack.replaceAll("\\.", File.separator), s"$functionName.scala")
            FileUtil.move(sourceFile, f)
            FileUtil.addPrefix(f, s"package $mainPack\n")
            val mainClassFile = mainClass.replaceAll("\\.", File.separator) + ".scala"
            FileUtil.copy(FileUtil.file(FileUtil.currentClassDir, mainClassFile), file(src, mainClassFile))

            for (c <- compilerConfig.extraClasses) {
              val scalaFile = c.replaceAll("\\.", File.separator) + ".scala"
              FileUtil.copy(file(FileUtil.currentClassDir, scalaFile), file(src, scalaFile))
            }

            FileUtil.write(file(sourcesDir, "build.sbt"),
              s"""name := "$functionName"
                 |scalaVersion := "$scalaVersion"
                 |${methodReplaceConf.dependencies.map(d => s"libraryDependencies += $d").mkString("\n")}
                 |assemblyJarName in assembly := "$jar"
                 |mainClass in assembly := Some("$mainClass")
                 |assemblyMergeStrategy in assembly := {
                 |  case PathList("javax", "servlet", xs @ _*)         => MergeStrategy.first
                 |  case PathList(ps @ _*) if ps.last endsWith ".html" => MergeStrategy.first
                 |  case "application.conf"                            => MergeStrategy.concat
                 |  case "META-INF/MANIFEST.MF"                        => MergeStrategy.discard
                 |  case "META-INF/manifest.mf"                        => MergeStrategy.discard
                 |  case x => MergeStrategy.first
                 |}
              """.stripMargin)

            FileUtil.write(file(sourcesDir, "project", "plugins.sbt"),
              """resolvers += Resolver.sonatypeRepo("public")
                |addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.12.0")
              """.stripMargin)

            ProcessUtil.launch(sourcesDir, "sbt", "assembly")
            FileUtil.move(file(executableDir, "target", s"scala-${scalaVersion.substring(0, scalaVersion.lastIndexOf("."))}", jar),
              file(executableDir, jar))

          case _ =>
            FileUtil.write(buildSbtFile,
              s"""name := "$functionName"
              |
              |scalaVersion := "$scalaVersion"
              |
              |artifactPath in Compile in packageBin := file("$jarPath")
              |
              |scalacOptions ++= Seq(${compilerConfig.extraCompilerOptions.map(StringUtil.quote).mkString(", ")})
              |""".stripMargin)
            val command = Seq("sbt", "package")
            ProcessUtil.launch(sourcesDir, command: _*)
        }
      case None =>
        val settings = new Settings
        settings.usejavacp.value = true
        // necessary to lauch compiler
        // see http://stackoverflow.com/questions/27934282/object-scala-in-compiler-mirror-not-found-running-scala-compiler-programatical
        settings.embeddedDefaults[LmsCompilerScala]
        val compilerOptions = "-d" :: jarPath :: compilerConfig.extraCompilerOptions.toList
        settings.processArguments(compilerOptions, false)
        val reporter = new StoreReporter
        val compiler: Global = new Global(settings, reporter)
        val run = new compiler.Run
        run.compile(List(sourceFile.getAbsolutePath))

        import java.io.PrintWriter
        val S = new PrintWriter(logFile)
        S.println(settings)
        S.println(s"${settings.classpath}\n")
        for(row <- reporter.infos) {
          S.println(s"${row.severity}: ${row.msg}")
          S.println(s"|${row.pos.source.path}:${row.pos.safeLine}")
          S.println(s"|${row.pos.lineContent}")
          S.println("|"+" "*(row.pos.column-1) + "^")
        }
        S.println(s"class $functionName compiled with ${reporter.ERROR.count} errors and ${reporter.WARNING.count} warnings")
        S.close()

        reporter.ERROR.count match {
          case 0 => {} //println(s"class $functionName compiled with ${reporter.WARNING.count} warnings")
          case _ => throw new Exception(s"class $functionName compiled with ${reporter.ERROR.count} errors and ${reporter.WARNING.count} warnings, see ${logFile.getAbsolutePath} for details")
        }
    }

    val ar = FileUtil.listFiles(executableLibsDir, ExtensionFilter("jar"))
    val urls = (jarFile +: ar).map(_.toURI.toURL)
    CustomCompilerOutput(urls, compilerConfig.mainPack)
  }

  def loadMethod(compilerOutput: CompilerOutput[_, _]) = {
    // ensure Scala library is available
    val classLoader = new URLClassLoader(compilerOutput.custom.jars, classOf[_ => _].getClassLoader)
    val pack = compilerOutput.custom.mainPack match {
      case p: String => p + "."
      case _ => ""
    }
    val cls = classLoader.loadClass(pack + compilerOutput.common.name)
    val argumentClass = compilerOutput.common.eInput.classTag.runtimeClass
    (cls, cls.getMethod("apply", argumentClass))
  }

  protected def doExecute[A, B](compilerOutput: CompilerOutput[A, B], input: A): B = {
    val (cls, method) = loadMethod(compilerOutput)
    val instance = cls.newInstance()
    val result = method.invoke(instance, input.asInstanceOf[AnyRef])
    result.asInstanceOf[B]
  }
}
