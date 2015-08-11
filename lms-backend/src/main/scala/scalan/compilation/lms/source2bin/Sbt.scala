package scalan.compilation.lms.source2bin

import java.io.File
import java.io.File.separator

import scalan.ScalanCtxExp
import scalan.compilation.lms.scalac.LmsCompilerScalaConfig
import scalan.util.FileUtil._
import scalan.util.{FileUtil, ExtensionFilter, ProcessUtil, StringUtil}

object Sbt {

  val lib = "lib"

  def prepareDir(executableDir: File) = {
    val libDir = FileUtil.file(FileUtil.currentWorkingDir, lib)
    val executableLibsDir = FileUtil.file(executableDir, lib)
    val dir = FileUtil.listFiles(libDir, ExtensionFilter("jar"))
    dir.foreach(f => FileUtil.copyToDir(f, executableLibsDir))
  }

  def compile(sourcesDir: File, executableDir: File, functionName: String, compilerConfig: LmsCompilerScalaConfig, dependencies: Array[String], sourceFile: File, jarPath: String): Array[String] = {
    val scalaVersion = compilerConfig.scalaVersion.getOrElse {
      throw new Exception(s"You must define compilerConfig.scalaVersion for use Sbt.compile method")
    }
    val buildSbtFile = new File(sourcesDir, "build.sbt")
    val libsDir = file(currentWorkingDir, lib)
    val executableLibsDir = file(executableDir, lib)

    listFiles(libsDir, ExtensionFilter("jar")).foreach(f => copyToDir(f, executableLibsDir))
    compilerConfig.sbt.mainPack match {
      case Some(mainPack) =>
        val jar = s"$functionName.jar"
        val src = file(executableDir, "src", "main", "scala")
        val resources = file(executableDir, "src", "main", "resources")
        val f = file(src, mainPack.replaceAll("\\.", separator), s"$functionName.scala")
        move(sourceFile, f)
        addHeader(f, s"package $mainPack")
        def scalaSource(className: String) = className.replaceAll("\\.", separator)  + ".scala"

        val mainClassFullName = mainPack + "." + compilerConfig.sbt.mainClassSimpleName
        val mainClass = scalaSource(mainClassFullName)
        val mainDest = file(src, mainClass)
        try {
          copyFromClassPath(mainClass, mainDest)
        } catch {
          case _: NullPointerException =>
            write(mainDest,
              s"""package $mainPack
                  |object ${compilerConfig.sbt.mainClassSimpleName} {
                  | def main(args: Array[String]): Unit = {
                  |  println("Main function isn't implemented for program '$functionName'")
                  | }
                  |}
          """.stripMargin)
        }
        for (c <- compilerConfig.sbt.extraClasses) {
          val scalaFile = scalaSource(c)
          copyFromClassPath(scalaFile, file(src, scalaFile))
        }
        for (c <- compilerConfig.sbt.resources) copyFromClassPath(c, file(resources, c))

        write(file(sourcesDir, "build.sbt"),
          s"""name := "$functionName"
              |scalaVersion := "$scalaVersion"
              |${dependencies.map(d => s"libraryDependencies += $d").mkString("\n")}
              |assemblyJarName in assembly := "$jar"
              |mainClass in assembly := Some("$mainClassFullName")
              |version := "1"
              |artifactPath in Compile in packageBin := file("$jarPath")
              |scalacOptions ++= Seq(${compilerConfig.extraCompilerOptions.map(StringUtil.quote).mkString(", ")})
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

        write(file(sourcesDir, "project", "build.properties"), "sbt.version=0.13.7")

        compilerConfig.sbt.commands.dropRight(1).foreach(com => ProcessUtil.launch(sourcesDir, "sbt", com))
        val output: Array[String] = ProcessUtil.launch(sourcesDir, "sbt", compilerConfig.sbt.commands.last)

        val jarFile = file(executableDir, "target", s"scala-${scalaVersion.substring(0, scalaVersion.lastIndexOf("."))}", jar)
        jarFile.exists() match {
          case true => move(jarFile, file(executableDir, jar))
          case false =>
        }
        output

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
