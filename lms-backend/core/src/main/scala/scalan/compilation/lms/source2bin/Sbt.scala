package scalan.compilation.lms.source2bin

import java.io.File
import java.io.File.separator

import org.apache.commons.io.FileUtils

import scalan.ScalanDslExp
import scalan.compilation.lms.scalac.LmsCompilerScalaConfig
import scalan.util.FileUtil._
import scalan.util.{ExtensionFilter, FileUtil, ProcessUtil, StringUtil}

object Sbt {

  def libDir(base: File) = new File(base, "lib")

  def prepareDir(executableDir: File) = {
    val workingLibDir = libDir(FileUtil.currentWorkingDir)
    val executableLibsDir = libDir(executableDir)
    val dir = FileUtil.listFiles(workingLibDir, ExtensionFilter("jar"))
    dir.foreach(f => FileUtil.copyToDir(f, executableLibsDir))
  }

  def compile(sourcesDir: File, executableDir: File, functionName: String, extraCompilerOptions: Seq[String], sbtConfig: SbtConfig, dependencies: Array[String], sourceFile: File, jarPath: String): Array[String] = {
    val buildSbtFile = new File(sourcesDir, "build.sbt")
    val libsDir = libDir(currentWorkingDir)
    val executableLibsDir = libDir(executableDir)
    def writeSbtVersion(): Unit = {
      write(file(sourcesDir, "project", "build.properties"), "sbt.version=0.13.9")
    }

    listFiles(libsDir, ExtensionFilter("jar")).foreach(f => copyToDir(f, executableLibsDir))
    sbtConfig.mainPack match {
      case Some(mainPack) =>
        val jar = s"$functionName.jar"
        val src = file(executableDir, "src", "main", "scala")
        val resources = file(executableDir, "src", "main", "resources")
        val f = file(src, mainPack.replaceAll("\\.", separator), s"$functionName.scala")
        move(sourceFile, f)
        addHeader(f, s"package $mainPack")
        def scalaSourceFileName(className: String) = className.replaceAll("\\.", separator) + ".scala"

        val mainClassFullName = mainPack + "." + sbtConfig.mainClassSimpleName
        val mainClassFileName = scalaSourceFileName(mainClassFullName)
        val mainSourceFile = file(src, mainClassFileName)
        try {
          copyFromClassPath(mainClassFileName, mainSourceFile)
        } catch {
          case _: NullPointerException =>
            write(mainSourceFile,
              s"""package $mainPack
                  |object ${sbtConfig.mainClassSimpleName} {
                  | def main(args: Array[String]): Unit = {
                  |  println("Main function isn't implemented for program '$functionName'")
                  | }
                  |}
          """.stripMargin)
        }
        for (c <- sbtConfig.extraClasses) {
          val scalaFileName = scalaSourceFileName(c)
          copyFromClassPath(scalaFileName, file(src, scalaFileName))
        }
        for (c <- sbtConfig.resources) copyFromClassPath(c, file(resources, c))

        write(file(sourcesDir, "build.sbt"),
          s"""name := "$functionName"
              |scalaVersion := "${sbtConfig.scalaVersion}"
              |${dependencies.map(d => s"libraryDependencies += $d").mkString("\n")}
              |assemblyJarName in assembly := "$jar"
              |mainClass in assembly := Some("$mainClassFullName")
              |version := "1"
              |artifactPath in Compile in packageBin := file("$jarPath")
              |scalacOptions ++= Seq(${extraCompilerOptions.map(StringUtil.quote).mkString(", ")})
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

        writeSbtVersion()

        sbtConfig.commands.dropRight(1).foreach(com => ProcessUtil.launch(sourcesDir, "sbt", com))
        val output: Array[String] = ProcessUtil.launch(sourcesDir, "sbt", sbtConfig.commands.last)

        val jarFile = file(executableDir, "target", s"scala-${sbtConfig.scalaBinaryVersion}", jar)
        if (jarFile.exists()) {
          FileUtils.moveToDirectory(jarFile, executableDir, false)
        }
        output

      case _ =>
        write(buildSbtFile,
          s"""name := "$functionName"
              |
              |scalaVersion := "${sbtConfig.scalaVersion}"
              |
              |artifactPath in Compile in packageBin := file("$jarPath")
              |
              |scalacOptions ++= Seq(${extraCompilerOptions.map(StringUtil.quote).mkString(", ")})
              |""".stripMargin)
        writeSbtVersion()

        val command = Seq("sbt", "package")
        ProcessUtil.launch(sourcesDir, command: _*)
    }
  }
}
