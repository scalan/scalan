package scalan.util

import java.io.{InputStreamReader, BufferedReader, File}

object ProcessUtil {
  def launch(workingDir: File, command: String*) {
    val builder = new ProcessBuilder(command: _*)
    val absoluteWorkingDir = workingDir.getAbsoluteFile
    builder.directory(absoluteWorkingDir)
    builder.inheritIO()
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
        throw new RuntimeException(s"Executing '${command.mkString(" ")}' in directory $absoluteWorkingDir returned exit code $exitCode with following output:\n$sb")
      } finally {
        stream.close()
      }
    } else {
      // program executed successfully
    }
  }

}
