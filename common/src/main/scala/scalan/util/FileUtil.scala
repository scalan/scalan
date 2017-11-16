package scalan.util

import java.io._
import java.net.{JarURLConnection, URL}
import java.nio.charset.Charset
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util.jar.JarFile
import org.apache.commons.io.{FileUtils, IOUtils}
import scala.Console
import scala.collection.JavaConverters._
import scalan.util.StringUtil.StringUtilExtensions
import scalan.util.CollectionUtil.AnyOps

object FileUtil {
  def read(file: File): String = FileUtils.readFileToString(file, Charset.defaultCharset())

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

  def write(file: File, text: String): Unit = withFile(file) { _.print(text) }

  def withStdOutAndErr(out: PrintStream)(func: => Unit): Unit = {
    val oldStdOut = System.out
    val oldStdErr = System.err
    try {
      System.setOut(out)
      System.setErr(out)
      Console.withOut(out)(Console.withErr(out)(func))
    } finally {
      out.flush()
      System.setOut(oldStdOut)
      System.setErr(oldStdErr)
    }
  }

  def captureStdOutAndErr(func: => Unit): String = {
    val out = new ByteArrayOutputStream
    val ps = new PrintStream(out)
    try {withStdOutAndErr(ps)(func)}
    finally {ps.close() }
    out.toString
  }

  def copy(source: File, target: File): Unit =
    if (source.isFile)
      FileUtils.copyFile(source, target, false)
    else
      FileUtils.copyDirectory(source, target, false)

  def copyFromClassPath(source: String, target: File, classLoader: ClassLoader = getClass.getClassLoader): Unit = {
    target.getParentFile.mkdirs()
    val urls = classLoader.getResources(source)
    if (urls.hasMoreElements) {
      if (source.endsWith("/")) {
        urls.asScala.foreach { url =>
          url.getProtocol match {
            case "file" =>
              FileUtils.copyDirectory(urlToFile(url), target, false)
            case "jar" =>
              val jarFile = new JarFile(jarUrlToJarFile(url))
              jarFile.entries().asScala.foreach { entry =>
                val entryPath = entry.getName
                if (entryPath.startsWith(source)) {
                  val entryTarget = new File(target, entryPath.stripPrefix(source))
                  if (entry.isDirectory)
                    entryTarget.mkdirs()
                  else {
                    // copyInputStreamToFile closes stream
                    FileUtils.copyInputStreamToFile(jarFile.getInputStream(entry), entryTarget)
                  }
                }
              }
          }
        }
      } else {
        val url = urls.nextElement()
        if (urls.hasMoreElements) {
          throw new IllegalArgumentException(s"Multiple $source resources found on classpath")
        } else {
          // copyInputStreamToFile closes stream
          FileUtils.copyInputStreamToFile(url.openStream(), target)
        }
      }
    } else
      throw new IllegalArgumentException(s"Resource $source not found on classpath")
  }

  def classPathLastModified(source: String, classLoader: ClassLoader = getClass.getClassLoader) = {
    def urlLastModified(url: URL): Long = {
      url.getProtocol match {
        case "file" =>
          val file = urlToFile(url)
          if (file.isDirectory) {
            var result = file.lastModified()
            Files.walkFileTree(file.toPath, new SimpleFileVisitor[Path]() {
              override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
                result = math.max(result, file.toFile.lastModified())
                FileVisitResult.CONTINUE
              }

              override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
                if (exc == null) {
                  result = math.max(result, dir.toFile.lastModified())
                  FileVisitResult.CONTINUE
                } else {
                  throw exc
                }
              }
            })
            result
          } else
            file.lastModified()
        case "jar" =>
          jarUrlToJarFile(url).lastModified()
      }
    }

    val urls = classLoader.getResources(source)
    if (urls.hasMoreElements) {
      urls.asScala.map(urlLastModified).max
    } else
      throw new IllegalArgumentException(s"Resource $source not found on classpath")
  }

  def jarUrlToJarFile(url: URL) = {
    val jarFileUrl = url.openConnection().asInstanceOf[JarURLConnection].getJarFileURL
    urlToFile(jarFileUrl)
  }

  /**
    * Copy file source to targetDir, keeping the original file name
    */
  def copyToDir(source: File, targetDir: File): Unit =
    copy(source, new File(targetDir, source.getName))

  def move(source: File, target: File): Unit =
    if (source.isFile)
      FileUtils.moveFile(source, target)
    else
      FileUtils.moveDirectory(source, target)

  /**
    * Add header into the file
    */
  def addHeader(file: File, header: String): Unit = write(file, header + "\n" + read(file))

  /**
    * Like fileOrDirectory.delete() but works for non-empty directories
    * and throws exceptions instead of returning false on failure
    */
  def delete(fileOrDirectory: File): Unit = {
    deleteRecursive(fileOrDirectory.toPath)
  }

  def deleteIfExist(fileOrDirectory: File): Unit = {
    if (fileOrDirectory.exists()) delete(fileOrDirectory)
  }

  def deleteRecursive(path: Path) {
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

  def file(first: String, rest: String*): File =
    file(new File(first), rest: _*)

  def file(first: File, rest: String*): File =
    rest.foldLeft(first) { (file, child) => new File(file, child) }

  final val AcceptAllFiles = new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = !file(dir, name).isDirectory
  }
  final val AcceptAllDirectories = new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = file(dir, name).isDirectory
  }

  /**
    * Same as dir.listFiles(filter), except it returns empty array instead of null
    * if dir doesn't exist or is not a directory
    */
  def listFiles(dir: File, filter: FilenameFilter = AcceptAllFiles): Array[File] = dir.listFiles(filter) match {
    case null => Array.empty
    case array => array
  }

  def listDirectories(dir: File): Array[File] = listFiles(dir, AcceptAllDirectories)

  /** Starts from <code>dir</code> and builds an array of sub-directories including <code>dir</code> */
  def listDirectoriesRecursive(dir: File): Array[File] = {
    dir.traverseDepthFirst(f => listDirectories(f).toList).toArray
  }

  def listFilesRecursive(dir: File): Array[File] = {
    val dirs = listDirectoriesRecursive(dir)
    for {d <- dirs; f <- listFiles(d)} yield f
  }

  def readAndCloseStream(stream: InputStream) = {
    try {
      IOUtils.toString(stream, Charset.defaultCharset())
    } finally {
      IOUtils.closeQuietly(stream)
    }
  }

  def stripExtension(fileName: String) =
    fileName.lastIndexOf('.') match {
      case -1 =>
        fileName
      case n =>
        fileName.substring(0, n)
    }

  def replaceOrAppendExtension(fileName: String, extension: String): String =
    s"${stripExtension(fileName) }.$extension"

  def modifyName(file: File)(f: String => String): File = {
    val parent = file.getParentFile
    val name = file.getName
    val newName = f(name)
    new File(parent, newName)
  }

  def urlToFile(url: URL) = Paths.get(url.toURI).toFile

  /** Accepts an arbitrary (printable) string and returns a similar string
    * which can be used as a file name. For convenience, replaces spaces with hyphens.
    */
  def cleanFileName(string: String) = string.
      replaceAll("""[ /\\:;<>|?*^]""", "_").
      replaceAll("""['"]""", "")

  def isBadFileName(string: String) = cleanFileName(string) != string

  def extractModuleName(path: String, sourceDir: String = "src/main/scala"): String = {
    val moduleDir = path.prefixBefore("/" + sourceDir)
    if (moduleDir.length == path.length) return ""
    moduleDir.lastComponent('/')
  }

}

case class ExtensionFilter(extension: String) extends FilenameFilter {
  override def accept(dir: File, name: String): Boolean = name.toLowerCase.endsWith(s".$extension")
}
