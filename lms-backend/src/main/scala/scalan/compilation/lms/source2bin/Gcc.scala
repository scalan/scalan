package scalan.compilation.lms.source2bin

import java.io.File

import scalan.util.ProcessUtil

/**
 * Created by adel on 5/26/15.
 */
object Gcc {
  def compile(executableDir: File, sourceFile: File, libName: String): Array[String] = {  //extraCompilerOptions: List[String],
    val sourceName = sourceFile.getName
    val runtimeDir1 = "../../../../../../../runtime/cpp/src/"  //todo make normal path
    val runtimeDir2 = "../../../../../../runtime/cpp/src/"
    val include = "-I/usr/include -I/usr/lib/jvm/java-7-oracle/include -I/usr/lib/jvm/java-7-oracle/include/linux -I"+runtimeDir2 //+" -I"+runtimeDir2
    val command = s"g++ $sourceName $include -fPIC -shared -pthread -std=c++0x -o lib${libName}.so".split(" ").toSeq
    println("command: " + command.mkString(" "))
    ProcessUtil.launch(executableDir, command: _*)

  }
}
