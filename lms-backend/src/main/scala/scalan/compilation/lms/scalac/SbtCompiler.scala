package scalan.compilation.lms.scalac

import java.io.File

import scalan.util.{StringUtil, ProcessUtil, ExtensionFilter, FileUtil}
import scalan.util.FileUtil._

trait SbtCompiler { self:LmsCompilerScala =>

  case class SbtConfig(mainPack: Option[String] = None, extraClasses : Seq[String] = Seq.empty[String], mainClassSimpleName: String = "run", commands: Seq[String] = Seq("clean", "compile"))
  val lib = "lib"

  def sbtCompile(sourcesDir: File, executableDir: File, functionName: String, compilerConfig: CompilerConfig, sourceFile : File, jarPath : String): Unit = {
    val scalaVersion = compilerConfig.scalaVersion.get
    val buildSbtFile = new File(sourcesDir, "build.sbt")
    val libsDir = file(currentWorkingDir, lib)
    val executableLibsDir = file(executableDir, lib)
    var jars = methodReplaceConf.libPaths.map { j => file(libsDir, j).getAbsolutePath }
    val dir = listFiles(libsDir, ExtensionFilter("jar"))
    dir.foreach(f => {
      jars = jars + f.getAbsolutePath
      copyToDir(f, executableLibsDir)
    })
    compilerConfig.sbt.mainPack match {
      case Some(mainPack) =>
        val mainClass = mainPack + "." + compilerConfig.sbt.mainClassSimpleName
        val jar = s"$functionName.jar"
        val src = file(executableDir, "src", "main", "scala")
        val f = file(src, mainPack.replaceAll("\\.", File.separator), s"$functionName.scala")
        move(sourceFile, f)
        addHeader(f, s"package $mainPack")
        val mainClassFile = mainClass.replaceAll("\\.", File.separator) + ".scala"
        copy(file(currentClassDir, mainClassFile), file(src, mainClassFile))

        for (c <- compilerConfig.sbt.extraClasses) {
          val scalaFile = c.replaceAll("\\.", File.separator) + ".scala"
          copy(file(currentClassDir, scalaFile), file(src, scalaFile))
        }

        write(file(sourcesDir, "build.sbt"),
          s"""name := "$functionName"
              |scalaVersion := "$scalaVersion"
              |${methodReplaceConf.dependencies.map(d => s"libraryDependencies += $d").mkString("\n")}
              |assemblyJarName in assembly := "$jar"
              |mainClass in assembly := Some("$mainClass")
              |version := "1"
              |assemblyMergeStrategy in assembly := {
              |  case PathList("javax", "servlet", xs @ _*)         => MergeStrategy.first
              |  case PathList(ps @ _*) if ps.last endsWith ".html" => MergeStrategy.first
              |  case "application.conf"                            => MergeStrategy.concat
              |  case PathList("META-INF", ps @ _*) if ps.nonEmpty && (ps.last.toLowerCase.endsWith(".mf") ||
              |    ps.last.toLowerCase.endsWith(".sf")
              |    || ps.last.toLowerCase.endsWith(".dsa"))         => MergeStrategy.discard
              |  case x => MergeStrategy.first
              |}""".stripMargin)

        write(file(sourcesDir, "project", "plugins.sbt"),
          """resolvers += Resolver.sonatypeRepo("public")
            |addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.12.0")
          """.stripMargin)

        compilerConfig.sbt.commands.foreach(com => ProcessUtil.launch(sourcesDir, "sbt", com))

        val jarFile = file(executableDir, "target", s"scala-${scalaVersion.substring(0, scalaVersion.lastIndexOf("."))}", jar)
        jarFile.exists() match {
          case true => move(jarFile, file(executableDir, jar))
          case false =>
        }

      case _ =>
        write(buildSbtFile,
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
  }
}
