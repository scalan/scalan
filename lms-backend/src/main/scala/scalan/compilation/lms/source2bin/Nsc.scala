package scalan.compilation.lms.source2bin

import java.io.File

import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.StoreReporter
import scalan.compilation.lms.scalac.LmsCompilerScala
import scalan.util.FileUtil
import scalan.util.FileUtil._

/**
 * make jar-file from scala-file via internal compiler scala.tools.nsc
 *  Created by adel on 4/8/15.
 */

object Nsc {

  def compile(executableDir: File, functionName: String, extraCompilerOptions: List[String], sourceFile: File, jarPath: String): Array[String] = {
    val logFile = file(executableDir.getAbsoluteFile, s"$functionName.log")
    FileUtil.deleteIfExist(logFile)

    val settings = new Settings
    settings.usejavacp.value = true
    // necessary to lauch compiler
    // see http://stackoverflow.com/questions/27934282/object-scala-in-compiler-mirror-not-found-running-scala-compiler-programatical
    settings.embeddedDefaults[LmsCompilerScala]
    val compilerOptions = "-d" :: jarPath :: extraCompilerOptions
    settings.processArguments(compilerOptions, processAll = false)
    val reporter = new StoreReporter
    val compiler = new Global(settings, reporter)
    val run = new compiler.Run
    run.compile(List(sourceFile.getAbsolutePath))

    import java.io.PrintWriter
    val S = new PrintWriter(logFile)
    S.println(settings)
    S.println(s"${settings.classpath}\n")
    for(row <- reporter.infos) {
      S.println(s"${row.severity}: ${row.msg}")
      S.println(s"|${row.pos.source.path}:${row.pos.safeLine}")
      S.println(s"|${row.pos.lineContent}")
      S.println("|"+" "*(row.pos.column-1) + "^")
    }
    S.println(s"class $functionName compiled with ${reporter.ERROR.count} errors and ${reporter.WARNING.count} warnings")
    S.close()

    reporter.ERROR.count match {
      case 0 => //println(s"class $functionName compiled with ${reporter.WARNING.count} warnings")
      case _ => throw new Exception(s"class $functionName compiled with ${reporter.ERROR.count} errors and ${reporter.WARNING.count} warnings, see ${logFile.getAbsolutePath} for details")
    }
    Array("")

  }

}
