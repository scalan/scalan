package scalan.util

import java.io.{InputStreamReader, BufferedReader, File}

object ProcessUtil {
  def launch(workingDir: File, command: String*) {
    val absoluteWorkingDir = workingDir.getAbsoluteFile
    val builder = new ProcessBuilder(command: _*).
      directory(absoluteWorkingDir).
      redirectErrorStream(true)
    val proc = builder.start()
    val reader = new BufferedReader(new InputStreamReader(proc.getInputStream))
    var br: Option[BufferedReader] = None
    val exitCode = proc.waitFor()
    exitCode match {
      case 0 =>
        toSystemOut match {
          case true =>
            try {
              var line: String = reader.readLine()
              while (line != null) line = reader.readLine()
            } finally {
              reader.close()
            }
          case false => br = Some(reader)
        }
      case _ =>
        try {
          var line: String = reader.readLine()
          val sb = new StringBuilder
          while (line != null) {
            sb.append(line).append("\n")
            line = reader.readLine()
          }
          throw new RuntimeException(s"Executing '${command.mkString(" ")}' in directory $absoluteWorkingDir returned exit code $exitCode with following output:\n$sb")
        } finally {
          reader.close()
        }
    }
    br
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
