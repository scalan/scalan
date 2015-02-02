package scalan.util

import java.io._
import java.nio.file.Paths
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

  def write(file: File, text: String):Unit = withFile(file) { _.print(text) }

  def write(path: String, text: String):Unit = write(new File(path), text)

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

  def copy(source: File, target: File): Unit = {
    target.getParentFile.mkdirs()
    new FileOutputStream(target).getChannel.transferFrom(new FileInputStream(source).getChannel, 0, Long.MaxValue)
  }

  /**
   * Copy file source to targetDir, keeping the original file name
   */
  def copyToDir(source: File, targetDir: File): Unit =
    copy(source, new File(targetDir, source.getName))

  /**
   * Like fileOrDirectory.delete() but works for non-empty directories
   * and throws exceptions instead of returning false on failure
   */
  def delete(fileOrDirectory: File): Unit = {
    if (fileOrDirectory.isDirectory) {
      fileOrDirectory.listFiles.foreach(delete)
    }

    if (fileOrDirectory.exists()) {
      if (!fileOrDirectory.delete()) throw new IOException(s"Failed to delete file $fileOrDirectory")
    } else {
      throw new FileNotFoundException(s"$fileOrDirectory doesn't exist")
    }
  }

  def currentWorkingDir = Paths.get("").toAbsolutePath.toFile

  def file(first: String, rest: String*): File =
    file(new File(first), rest: _*)

  def file(first: File, rest: String*): File =
    rest.foldLeft(first) { (file, child) => new File(file, child) }
}
