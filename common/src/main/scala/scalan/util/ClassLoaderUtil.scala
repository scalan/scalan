package scalan.util

import java.io.File
import java.net.URLClassLoader

object ClassLoaderUtil {
  def classPath(loader: ClassLoader): Seq[File] = loader match {
    case loader: URLClassLoader =>
      loader.getURLs.map(FileUtil.urlToFile) ++ classPath(loader.getParent)
    case _ =>
      Nil
  }

  def URLClassLoader(files: Array[File], parent: ClassLoader): URLClassLoader =
    new URLClassLoader(files.map(_.toURI.toURL), parent)
  def URLClassLoader(files: TraversableOnce[File], parent: ClassLoader): URLClassLoader =
    URLClassLoader(files.toArray, parent)
}
