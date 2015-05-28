package scalan.compilation.lms.source2bin

import java.io.File

import scalan.util.ProcessUtil

/**
 * Created by adel on 5/26/15.
 */
object Gcc {
  def compile(targetDir: String, sourceDir: File, sourceFile: File, libName: String): Array[String] = {  //extraCompilerOptions: List[String],
    val sourceName = sourceFile.getName
    val targetFile = targetDir+fileSeparator+libFileName(libName)
    val include = s"-I/usr/include $includeJavaFlag $includeRuntimeDirFlag"
    val command = s"g++ $sourceName $include -fPIC -shared -pthread -std=c++0x -o $targetFile".split(" ").toSeq
    println("command: " + command.mkString(" "))
    ProcessUtil.launch(sourceDir, command: _*)

  }

  def includeRuntimeDirFlag: String = {
    "-I"+System.getProperty("user.dir") + fileSeparator + scalan.Base.config.getProperty("runtime.include")
  }

  val javaHome = System.getProperty("java.home")+"/.."
  val includeJavaFlag = s"-I$javaHome/include -I$javaHome/include/linux"
  val fileSeparator = System.getProperty("file.separator")
  val osName = System.getProperty("os.name")
  def libFileName(libName:String) = {
    osName match {
      case x if x contains ("Linux") => s"lib${libName}.so"
      case x if x contains ("OS X") => s"lib${libName}.dylib"
      case x if x contains ("Windows") => s"${libName}.dll"
      case _ =>
        println("WARNING: OS not detected, try to generate library name for Linux")
        s"lib${libName}.so"
    }

  }
}
