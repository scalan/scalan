package scalan.util

import java.io.{InputStreamReader, BufferedReader, File}

object ProcessUtil {
  def launch(workingDir: File, command: String*) {
    val absoluteWorkingDir = workingDir.getAbsoluteFile
    val builder = new ProcessBuilder(command: _*).
      directory(absoluteWorkingDir).
      redirectErrorStream(true)
    val proc = builder.start()
    val exitCode = proc.waitFor()
    val output = readOutput(proc)
    if (exitCode != 0) {
      throw new RuntimeException(s"Executing '${command.mkString(" ")}' in directory $absoluteWorkingDir returned exit code $exitCode with following output:\n$output")
    } else {
      Console.print(output)
    }
  }

  def readOutput(process: Process) = {
    val stream = process.getInputStream
    try {
      val sb = new StringBuilder()
      val reader = new BufferedReader(new InputStreamReader(stream))
      var line: String = reader.readLine()
      while (line != null) {
        sb.append(line).append("\n")
        line = reader.readLine()
      }
      sb.result()
    } finally {
      stream.close()
    }
  }
}
