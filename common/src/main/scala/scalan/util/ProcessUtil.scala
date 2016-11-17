package scalan.util

import java.io.{BufferedReader, File, InputStreamReader}

import scala.collection.mutable
import scala.collection.JavaConverters._
import scala.sys.process.ProcessLogger

case class ProcessOutput(stdOutLines: Seq[String], stdErrLines: Seq[String], interleavedLines: Seq[String]) {
  lazy val stdOutAll = stdOutLines.mkString("\n")
  lazy val stdErrAll = stdErrLines.mkString("\n")
  lazy val interleavedAll = interleavedLines.mkString("\n")
}

object ProcessUtil {
  /** Similar to scala.sys.process.ProcessBuilder.!!, but collects both error and output streams and includes
    * the complete output into the exception message on failure */
  def launch(command: Seq[String], workingDir: File = FileUtil.currentWorkingDir, extraEnv: Map[String, String] = Map.empty, printToConsole: Boolean = false): ProcessOutput = {
    val absoluteWorkingDir = workingDir.getAbsoluteFile
    val builder = scala.sys.process.Process(command, absoluteWorkingDir, extraEnv.toSeq: _*)
    val stdOutBuffer = mutable.ArrayBuffer.empty[String]
    val stdErrBuffer = mutable.ArrayBuffer.empty[String]
    val interleavedBuffer = mutable.ArrayBuffer.empty[String]
    val logger = ProcessLogger(
      outLine => {
        if (printToConsole) { Console.out.println(outLine) }
        stdOutBuffer += outLine
        interleavedBuffer += outLine
      },
      errLine => {
        if (printToConsole) { Console.err.println(errLine) }
        stdErrBuffer += errLine
        interleavedBuffer += errLine
      }
    )
    (builder ! logger) match {
      case 0 =>
        ProcessOutput(stdOutBuffer, stdErrBuffer, interleavedBuffer)
      case exitCode =>
        val envPrefix = extraEnv.map {
          case (name, value) => s"$name=${escapeCommandLineArg(value)} "
        }.mkString("")
        val commandString = command.map(escapeCommandLineArg).mkString(" ")
        throw new RuntimeException(s"Executing `$envPrefix$commandString` in directory $absoluteWorkingDir returned exit code $exitCode with following output:\n${interleavedBuffer.mkString("\n")}")
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
