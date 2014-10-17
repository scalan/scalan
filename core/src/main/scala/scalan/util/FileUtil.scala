package scalan.util

import java.io._
import scala.Console
import scala.io.{Source, Codec}

object FileUtil {
  def read(file: File, codec: Codec = Codec.UTF8): String = {
    val source = Source.fromFile(file)(codec)
    try {
      source.mkString
    } finally {
      source.close()
    }
  }

  // default arguments can't be used when another overload has them
  def read(path: String, codec: Codec): String = read(new File(path), codec)

  def read(path: String): String = read(new File(path), Codec.UTF8)

  def withFile(file: File)(f: PrintWriter => Unit): Unit = {
    if (file.isDirectory && !file.delete()) {
      throw new RuntimeException(s"File $file is a non-empty directory")
    } else {
      file.getParentFile.mkdirs()
      val stream = new PrintWriter(new FileOutputStream(file))
      try {
        f(stream)
      } finally {
        stream.close()
      }
    }
  }

  def write(file: File, text: String) = withFile(file) { _.print(text) }

  def captureStdOutAndErr(func: => Unit): String = {
    val out = new ByteArrayOutputStream
    withStdOutAndErr(new PrintStream(out))(func)
    out.toString
  }

  def withStdOutAndErr(out: PrintStream)(func: => Unit): Unit = {
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
}
