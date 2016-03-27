package scalan.compilation.lms.source2bin

import java.io.File

import scalan.util.ProcessUtil

/**
 * Created by adel on 5/26/15.
 */
object Gcc {
  def compile(targetDir: String, sourceDir: File, sourceFile: File, libName: String): Array[String] = {
    val command = compileCommand(targetDir, sourceDir, sourceFile, libName)
    ProcessUtil.launch(sourceDir, command.split("\\s+"): _*)
  }

  def compileCommand(targetDir: String, sourceDir: File, sourceFile: File, libName: String) = {
    val sourceName = sourceFile.getAbsolutePath
    val targetFile = targetDir+fileSeparator+System.mapLibraryName(libName)
    val include = s"$includeJavaFlag $includeRuntimeDirFlag $includeFlags"
    s"$cxx $sourceName $include -fPIC -shared -pthread $commonFlags $optFlags -o $targetFile"
  }

  def includeRuntimeDirFlag: String = {
    "-I"+System.getProperty("user.dir") + fileSeparator + scalan.Base.config.getProperty("runtime.include")
  }

  val javaHome = scalan.Base.config.getProperty("java.home")+"/.."
  val includeJavaFlag = s"-I$javaHome/include -I$javaHome/include/linux -I$javaHome/include/darwin -I/usr/local/Cellar/boost/1.58.0/include"
  val fileSeparator = scalan.Base.config.getProperty("file.separator")
  val osName = scalan.Base.config.getProperty("os.name")
  val cxx = sys.env.getOrElse("CXX", "g++")
  val commonFlags = scalan.Base.config.getProperty("gcc.commonFlags", "-std=c++11 -Wall -pedantic")
  val includeFlags = scalan.Base.config.getProperty("gcc.includeFlags", "")
  val optFlags = scalan.Base.config.getProperty("gcc.optFlags", "-O3")
  val debugFlags = scalan.Base.config.getProperty("gcc.debugFlags", "-g -O0")
}
