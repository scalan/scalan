package tests.scalan.meta

import org.scalatest.FunSuite
import scalan.meta.{ Base, EntityManagement, CodegenConfig }
import java.io._
import java.nio.file.Files
import tests.BaseTests
import scalan.meta.BoilerplateTool

class DependentCompileTests extends BaseTests {
  it("Scalan-lite") {
    compile(Base.config.getProperty("scalan-lite.assembly"), BoilerplateTool.liteConfig,
      new File("src/test/resources/scalan-lite"))
  }

  it("Scalan") {
    compile(Base.config.getProperty("scalan.assembly"), BoilerplateTool.scalanConfig,
      new File("src/test/resources/scalan"))
  }

  def compile(jarPath: String, config: CodegenConfig, dir: File) = {
    val testDir = Files.createTempDirectory("sbtTest")
    val fileNames = dir.list.map(_.stripSuffix(".tmplt"))
    try {
      for (fileName <- fileNames) {
        copy(s"$dir/$fileName.tmplt", s"$testDir/$fileName")
      }

      val ctx = new EntityManagement(config.copy(srcPath = testDir.toString, entityFiles = fileNames.toList.filter(!_.endsWith("Ops.scala"))))
      ctx.generateAll()

      writeToFile(testDir + "/Makefile", s"main:\n\t\tscalac -classpath $jarPath/* *.scala impl/*.scala")

      launchProcess(new File(testDir.toString), "make")
    } finally {
      launchProcess(new File(testDir.toString), "rm", "-rf", testDir.toString)
    }
  }

  def copy(src: String, dest: String) = 
    new FileOutputStream(dest) getChannel () transferFrom (new FileInputStream(src).getChannel, 0, Long.MaxValue)

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
