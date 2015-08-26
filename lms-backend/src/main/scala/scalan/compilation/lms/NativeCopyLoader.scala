// adapted from sbt.classpath.NativeCopyLoader
package scalan.compilation.lms

import java.io.File
import java.net.{URLClassLoader, URL}
import java.nio.file.Files

import scalan.util.FileUtil

/**
 * Loads native libraries from a temporary location in order to work around the jvm native library uniqueness restriction.
 *
 * The loader will provide native libraries listed in `explicitLibraries` and on `searchPaths` by copying them to `tempDirectory`.
 */
class NativeCopyLoader(explicitLibraries: Seq[File], searchPaths: Seq[File], urls: Array[URL], parent: ClassLoader) extends URLClassLoader(urls, parent) {
  private[this] val mapped = new collection.mutable.HashMap[String, String]
  private[this] val tempDirectory = Files.createTempDirectory("native-library-copies").toFile
  tempDirectory.deleteOnExit()

  override protected def findLibrary(name: String): String =
    synchronized { mapped.getOrElseUpdate(name, findLibrary0(name)) }

  private[this] def findLibrary0(name: String): String = {
    val mappedName = System.mapLibraryName(name)
    val explicit = explicitLibraries.filter(_.getName == mappedName).toStream
    val search = searchPaths.toStream flatMap relativeLibrary(mappedName)
    (explicit ++ search).headOption.map(copy).orNull
  }
  private[this] def relativeLibrary(mappedName: String)(base: File): Seq[File] = {
    val f = new File(base, mappedName)
    if (f.isFile) f :: Nil else Nil
  }
  private[this] def copy(f: File): String = {
    val target = new File(tempDirectory, f.getName)
    FileUtil.copy(f, target)
    target.getAbsolutePath
  }
}