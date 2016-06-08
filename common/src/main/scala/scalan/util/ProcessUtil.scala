package scalan.util

import java.io.{InputStreamReader, BufferedReader, File}

import scala.collection.mutable

object ProcessUtil {
  def launch(workingDir: File, command: String*): Array[String] = {
    val absoluteWorkingDir = workingDir.getAbsoluteFile
    val builder = new ProcessBuilder(command: _*).
      directory(absoluteWorkingDir).
      redirectErrorStream(true)
    val proc = builder.start()
    val reader = new BufferedReader(new InputStreamReader(proc.getInputStream))
    val ar = mutable.ArrayBuffer[String]()
    var notDone = true
    while (notDone) {
      notDone = reader.readLine() match {
        case null => false
        case s2: String =>
          ar += s2
          true
      }
    }
    reader.close()
    val exitCode = proc.waitFor()
    exitCode match {
      case 0 => ar.toArray
      case _ =>
        val commandString = command.map(escapeCommandLineArg).mkString(" ")
        throw new RuntimeException(s"Executing `$commandString` in directory $absoluteWorkingDir returned exit code $exitCode with following output:\n${ar.mkString("\n")}")
    }
  }

  private def escapeCommandLineArg(arg: String) = {
    if (arg.contains(" ") && !arg.contains("'"))
      "'" + arg + "'"
    else {
      val escaped = arg.replace("""\""", """\\""").replace("$", """\$""").
        replace("`", """\`""").replace("\"", """\"""").replace("\n", "\\\n")
      if (escaped.contains(" ") || escaped != arg) StringUtil.quote(escaped) else arg
    }
  }
}
