package scalan.util

import java.io.{InputStreamReader, BufferedReader, File}

import scala.collection.mutable
import scala.collection.JavaConverters._

object ProcessUtil {
  /** Same as `launch(File, Map[String, String], String*)` with empty `extraEnv`. */
  def launch(workingDir: File, command: String*): Seq[String] =
    launch(workingDir, Map.empty[String, String], command: _*)

  /** Launches the process given by the command line `command` in directory `workingDir`. `extraEnv` is used
    * to modify the environment variables of the process. `null` values are allowed in `extraEnv`
    * and mean the variable should be deleted. */
  def launch(workingDir: File, extraEnv: Map[String, String], command: String*): Seq[String] = {
    // TODO use scala.sys.process.ProcessBuilder
    val absoluteWorkingDir = workingDir.getAbsoluteFile
    val builder = new ProcessBuilder(command: _*).
      directory(absoluteWorkingDir).
      redirectErrorStream(true)
    val env = builder.environment()
    val (toAdd, toRemove) = extraEnv.partition(_._2 != null)
    env.putAll(toAdd.asJava)
    for ((key, null) <- toRemove) {
      env.remove(key)
    }
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
      case 0 => ar
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
