package tests.scalan.meta

import org.scalatest.FunSuite
import scalan.meta.{Base, EntityManagement, CodegenConfig}
import java.io._
import java.nio.file.Files

class DependentCompileTests extends FunSuite {

  test("Scalan-lite") {
    compile(Base.config.getProperty("scalan-lite.assembly"))
  }

  def compile(jarPath : String) = {
    val dir = Files.createTempDirectory("sbtTest")
    try {
      copy("src/test/scala/tests/scalan/meta/files/PArrays.scala.tmplt", dir + "/PArrays.scala")
      copy("src/test/scala/tests/scalan/meta/files/PArraysOps.scala.tmplt", dir + "/PArraysOps.scala")
      copy("src/test/scala/tests/scalan/meta/files/Types.scala.tmplt", dir + "/Types.scala")
      copy("src/test/scala/tests/scalan/meta/files/TypesOps.scala.tmplt", dir + "/TypesOps.scala")

      val liteConfig = CodegenConfig(
        isLite = true,
        srcPath = dir.toString,
        entityFiles = List(
          "PArrays.scala",
          "Types.scala"
        ),
        proxyTrait = "scalan.ProxyExp",
        stagedViewsTrait = "scalan.ViewsExp",
        isoNames = ("From", "To"),
        extraImports = List(
          "scala.reflect.runtime.universe._",
          "scalan.common.Default.defaultVal")
      )

      val ctx = new EntityManagement(liteConfig)
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
