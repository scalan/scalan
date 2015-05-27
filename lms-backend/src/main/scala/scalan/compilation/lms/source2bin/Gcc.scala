package scalan.compilation.lms.source2bin

import java.io.File

import scalan.util.ProcessUtil

/**
 * Created by adel on 5/26/15.
 */
object Gcc {
  def compile(targetDir: String, sourceDir: File, sourceFile: File, libName: String): Array[String] = {  //extraCompilerOptions: List[String],
    val sourceName = sourceFile.getName
    val targetFile = targetDir+s"/lib${libName}.so" //todo make mname for non-linux platform
    val include = "-I/usr/include -I/usr/lib/jvm/java-7-oracle/include -I/usr/lib/jvm/java-7-oracle/include/linux -I"+includeRuntimeDir
    val command = s"g++ $sourceName $include -fPIC -shared -pthread -std=c++0x -o $targetFile".split(" ").toSeq
    println("command: " + command.mkString(" "))
    ProcessUtil.launch(sourceDir, command: _*)

  }

  def includeRuntimeDir: String = {
    System.getProperty("user.dir") + "/" + scalan.Base.config.getProperty("runtime.include")
  }
}
