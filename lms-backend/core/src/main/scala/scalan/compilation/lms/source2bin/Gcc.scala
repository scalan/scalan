package scalan.compilation.lms.source2bin

import java.io.File

import scalan.util.{FileUtil, ProcessUtil}
import com.github.kxbmap.configs.syntax._

import scalan.Plugins

object Gcc {
  def compile(targetDir: File, sourceDir: File, sourceFile: File, libName: String) = {
    val command = compileCommand(targetDir, sourceDir, sourceFile, libName)
    ProcessUtil.launch(sourceDir, command: _*)
  }

  def compileCommand(targetDir: File, sourceDir: File, sourceFile: File, libName: String) = {
    val sourcePath = sourceFile.getAbsolutePath
    val targetPath = new File(targetDir, System.mapLibraryName(libName)).getAbsolutePath
    val includeArgs = (List(new File(javaHome, "include"), new File(javaHome, "include/linux"), new File(javaHome, "include/darwin"), boostDir, runtimeIncludeDir) ++ otherIncludeDirs).map {
      file => s"-I${file.getAbsolutePath}"
    }
    List(cxx, sourcePath) ++ includeArgs ++ s"$commonFlags $optFlags -o $targetPath".split(" ")
  }

  val javaHome = new File(System.getProperty("java.home")).getAbsoluteFile.getParentFile
  val config = scalan.Base.config.getConfig("backend.cpp")
  val boostDir = config.get[File]("boost")
  val runtimeIncludeDir = config.get[File]("runtime.include")
  val otherIncludeDirs = config.get[List[File]]("otherIncludes")
  val gccConfig = config.getConfig("gcc")

  val cxx = gccConfig.getString("cxx")
  val commonFlags = gccConfig.getString("commonFlags")
  val optFlags = gccConfig.getString("optFlags")
  val debugFlags = gccConfig.getString("debugFlags")

  // TODO unpack relative to KernelStore?
  private val cppRuntimePath = "runtime/cpp/"
//  if (!runtimeIncludeDir.exists() || runtimeIncludeDir.lastModified() <= FileUtil.classPathLastModified(cppRuntimePath, Plugins.pluginClassLoader)) {
  FileUtil.copyFromClassPath(cppRuntimePath, runtimeIncludeDir, Plugins.pluginClassLoader)
//  }
}
