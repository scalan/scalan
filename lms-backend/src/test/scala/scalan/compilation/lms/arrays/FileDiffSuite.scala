package scalan.compilation.lms.arrays

import java.io.{PrintStream,File,FileOutputStream,ByteArrayOutputStream}
import org.scalatest._

import scalan.util.FileUtil

trait FileDiffSuite extends FunSuite {
  val dir = new File("test-out")

  def withOutFile(name: String)(func: => Unit): Unit = {
    val file = new File(dir, name)
    file.getParentFile.mkdirs()
    withOutput(new PrintStream(new FileOutputStream(file)))(func)
  }
  def captureOutput(func: => Unit): String = {
    val bstream = new ByteArrayOutputStream
    withOutput(new PrintStream(bstream))(func)
    bstream.toString
  }
  def withOutput(out: PrintStream)(func: => Unit): Unit = {
    val oldStdOut = System.out
    val oldStdErr = System.err
    try {
      System.setOut(out)
      System.setErr(out)
      Console.withOut(out)(Console.withErr(out)(func))
    } finally {
      out.flush()
      out.close()
      System.setOut(oldStdOut)
      System.setErr(oldStdErr)
    }
  }

  def assertFileEqualsCheck(name: String): Unit = {
    val file = new File(dir, name)
    assert(FileUtil.read(file) == FileUtil.readInputStream(FileUtil.getResource(name+".check")), name) // TODO: diff output
  }
  def withOutFileChecked(name: String)(func: => Unit): Unit = {
    withOutFile(name)(func)
    assertFileEqualsCheck(name)
  }
}
