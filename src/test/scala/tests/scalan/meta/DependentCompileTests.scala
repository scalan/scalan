package tests.scalan.meta

import org.scalatest.FunSuite
import scalan.meta.{Base, EntityManagement, CodegenConfig}
import java.io._
import java.nio.file.Files

class DependentCompileTests extends FunSuite {

  test("Scalan-lite") {
    val liteConfig = CodegenConfig(
      isLite = true,
      srcPath = "",
      entityFiles = List(
        "PArrays.scala",
        "Types.scala"
      ),
      proxyTrait = "scalan.ProxyExp",
      stagedViewsTrait = "scalan.ViewsExp",
      extraImports = List(
        "scala.reflect.runtime.universe._",
        "scalan.common.Default.defaultVal")
    )

    compile(Base.config.getProperty("scalan-lite.assembly"), liteConfig, "src/test/scala/tests/scalan/meta/files/scalan-lite", List("PArrays.scala", "PArraysOps.scala", "Types.scala", "TypesOps.scala"))
  }

  test("Scalan") {
    val scalanConfig = CodegenConfig(
      isLite = false,
      srcPath = "../scalan/src/main/scala",
      proxyTrait = "scalan.lms.common.ProxyExp",
      stagedViewsTrait = "scalan.staged.StagedViews",
      entityFiles = List(
        "Trees.scala",
        "Matrices.scala",
//        "Vectors.scala",
        "Sets.scala"
      ),
      extraImports = List(
        "scala.reflect.runtime.universe._",
        "scalan.common.Common",
        "scalan.staged.ScalanStaged",
        "scalan.sequential.ScalanSeq")
    )

    compile(Base.config.getProperty("scalan.assembly"), scalanConfig, "src/test/scala/tests/scalan/meta/files/scalan",
      List("Matrices.scala", "MatricesOps.scala", "Sets.scala", "SetsOps.scala", /*"Vectors.scala", "VectorsOps.scala", */"Trees.scala", "TreesOps.scala"))
  }

  def compile(jarPath : String, config : CodegenConfig, prefixPath : String, files : List[String]) = {
    val dir = Files.createTempDirectory("sbtTest")
    try {
      for (fileName <- files) {
        copy(s"$prefixPath/$fileName.tmplt", dir + s"/$fileName")
      }

      val ctx = new EntityManagement(config)
      ctx.generateAll()

      writeToFile(dir + "/Makefile", s"main:\n\t\tscalac -classpath $jarPath/* *.scala impl/*.scala")

      launchProcess(new File(dir.toString), "make")
    }
    finally {
      launchProcess(new File(dir.toString), "rm", "-rf", dir.toString)
    }
  }

  def copy(src : String, dest : String) = new FileOutputStream(dest) getChannel() transferFrom(new FileInputStream(src).getChannel, 0, Long.MaxValue)

  protected def launchProcess(launchDir: File, commandArgs: String*) {
    val builder = new ProcessBuilder(commandArgs: _*)
    val absoluteLaunchDir = launchDir.getAbsoluteFile
    builder.directory(absoluteLaunchDir)
    builder.redirectErrorStream(true)
    val proc = builder.start()
    val exitCode = proc.waitFor()
    if (exitCode != 0) {
      val stream = proc.getInputStream
      try {
        val sb = new StringBuilder()
        val reader = new BufferedReader(new InputStreamReader(stream))
        var line: String = reader.readLine()
        while (line != null) {
          sb.append(line).append("\n")
          line = reader.readLine()
        }
        throw new RuntimeException(s"Executing '${commandArgs.mkString(" ")}' in directory $absoluteLaunchDir returned exit code $exitCode with following output:\n$sb")
      } finally {
        stream.close()
      }
    }
  }

  def writeToFile(file: String, data: String): Unit = {
    val f = new File(file)
    f.getParentFile.mkdirs()
    val stream = new java.io.PrintWriter(new java.io.FileOutputStream(file))
    stream.println(data)
    stream.close()
  }
}
