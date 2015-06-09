package scalan.it.lms

import java.io.File

import org.scalatest.{Matchers, Suite}

import scalan.compilation.GraphVizConfig
import scalan.compilation.lms.cxx.LmsCompilerCxx
import scalan.it.ItTestsUtil

trait ItTestsUtilLmsCxx { self: Suite with Matchers with ItTestsUtil =>
  def generate[A, B](back: LmsCompilerCxx)(f: back.Exp[A => B], functionName: String)
                    (implicit config: back.CompilerConfig): Unit = {
    val dir = new File(prefix, functionName)
    back.buildExecutable(dir, dir, functionName, f, GraphVizConfig.default)
  }
}
