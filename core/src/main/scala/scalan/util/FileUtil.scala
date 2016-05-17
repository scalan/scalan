package scalan.util

import java.io._
import java.nio.channels.Channels
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes

import scala.Console
import scala.io.{Codec, Source}

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

  def copyFromClassPath(source: String, target: File): Unit = {
    target.getParentFile.mkdirs()
    val stream = getClass.getClassLoader.getResourceAsStream(source)
    if (stream != null)
      new FileOutputStream(target).getChannel.transferFrom(Channels.newChannel(stream), 0, Long.MaxValue)
    else
      throw new IllegalArgumentException(s"Resource $source not found on classpath")
  }

  /**
   * Copy file source to targetDir, keeping the original file name
   */
  def copyToDir(source: File, targetDir: File): Unit =
    copy(source, new File(targetDir, source.getName))

  def move(source: File, target: File): Unit = {
    copy(source, target)
    delete(source)
  }

  /**
   * Add header into the file
   */
  def addHeader(file: File, header: String): Unit = write(file, header + "\n" + read(file))

  /**
   * Like fileOrDirectory.delete() but works for non-empty directories
   * and throws exceptions instead of returning false on failure
   */
  def delete(fileOrDirectory: File): Unit = {
    removeRecursive(fileOrDirectory.toPath)
  }

  def deleteIfExist(fileOrDirectory: File): Unit = {
    if (fileOrDirectory.exists()) delete(fileOrDirectory)
  }

  def removeRecursive(path: Path) {
    Files.walkFileTree(path, new SimpleFileVisitor[Path]() {
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }
      override def visitFileFailed(file: Path, exc: IOException): FileVisitResult = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }
      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
        if (exc == null) {
          Files.delete(dir)
          FileVisitResult.CONTINUE
        }
        else {
          throw exc
        }
      }
    })
  }

  def currentWorkingDir = Paths.get("").toAbsolutePath.toFile

  def currentClassDir = getClass.getClassLoader.getResource("").getFile

  def file(first: String, rest: String*): File =
    file(new File(first), rest: _*)

  def file(first: File, rest: String*): File =
    rest.foldLeft(first) { (file, child) => new File(file, child) }

  /**
   * Same as dir.listFiles(filter), except it returns empty array instead of null
   * if dir doesn't exist or is not a directory
   */
  def listFiles(dir: File, filter: FilenameFilter): Array[File] = dir.listFiles(filter) match {
    case null => Array.empty
    case array => array
  }

  /**
    * Read resource from classpath
    */
  def getResource(file: String): InputStream =
    getResource(file, getClass.getClassLoader)

  def getResource(file: String, cl: ClassLoader): InputStream =
    cl.getResourceAsStream(file)

  /**
    * Read multiple resources from classpath
    */
  def getResources(file: String): Seq[InputStream] =
    getResources(file, getClass.getClassLoader)

  def getResources(file: String, cl: ClassLoader): Seq[InputStream] = {
    import collection.JavaConverters._
    cl.getResources(file).asScala.map(_.openStream()).toSeq.reverse
  }

  def readInputStream(stream: InputStream) = {
    val source = Source.fromInputStream(stream)
    try {
      source.mkString
    } finally {
      source.close()
    }
  }
}

case class ExtensionFilter(extension: String) extends FilenameFilter {
  override def accept(dir: File, name: String): Boolean = name.toLowerCase.endsWith(s".$extension")
}
