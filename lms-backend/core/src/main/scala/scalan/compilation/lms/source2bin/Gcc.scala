package scalan.compilation.lms.source2bin

import java.io.File

import scalan.util.ProcessUtil
import com.github.kxbmap.configs.syntax._

/**
 * Created by adel on 5/26/15.
 */
object Gcc {
  def compile(targetDir: File, sourceDir: File, sourceFile: File, libName: String): Array[String] = {
    val command = compileCommand(targetDir, sourceDir, sourceFile, libName)
    ProcessUtil.launch(sourceDir, command: _*)
  }

  def compileCommand(targetDir: File, sourceDir: File, sourceFile: File, libName: String) = {
    val sourcePath = sourceFile.getAbsolutePath
    val targetPath = new File(targetDir, System.mapLibraryName(libName)).getAbsolutePath
    val includeArgs = (List(new File(javaHome, "include"), new File(javaHome, "include/linux"), new File(javaHome, "include/darwin"), config.get[File]("boost"), config.get[File]("runtime.include")) ++ config.get[List[File]]("otherIncludes")).map {
      file => s"-I${file.getAbsolutePath}"
    }
    List(cxx, sourcePath) ++ includeArgs ++ s"$commonFlags $optFlags -o $targetPath".split(" ")
  }

  val javaHome = new File(System.getProperty("java.home")).getAbsoluteFile.getParentFile
  val config = scalan.Base.config.getConfig("backend.cpp")
  val gccConfig = config.getConfig("gcc")

  val cxx = gccConfig.getString("cxx")
  val commonFlags = gccConfig.getString("commonFlags")
  val optFlags = gccConfig.getString("optFlags")
  val debugFlags = gccConfig.getString("debugFlags")
}
