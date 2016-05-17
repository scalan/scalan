package scalan.util

import java.io.{ObjectStreamClass, ObjectInputStream, InputStream}

// adapted from https://github.com/scala/scala/blob/2.11.x/src/actors/scala/actors/remote/JavaSerializer.scala#L20
// and https://github.com/NetLogo/NetLogo/blob/5.x/src/main/org/nlogo/util/ClassLoaderObjectInputStream.scala
/**
 * An [[ObjectInputStream]] which uses the current thread's context classloader to resolve classes. See
 * <a>www.scala-sbt.org/0.13/docs/Running-Project-Code.html</a>.
 */
class ThreadContextClassLoaderObjectInputStream(in: InputStream) extends ObjectInputStream(in) {
  val cl = Thread.currentThread().getContextClassLoader

  override def resolveClass(cd: ObjectStreamClass): Class[_] =
    try {
      cl.loadClass(cd.getName)
    } catch {
      case cnf: ClassNotFoundException =>
        super.resolveClass(cd)
    }
  override def resolveProxyClass(interfaces: Array[String]): Class[_] =
    try {
      val ifaces = interfaces.map(cl.loadClass)
      java.lang.reflect.Proxy.getProxyClass(cl, ifaces: _*)
    } catch {
      case e: ClassNotFoundException =>
        super.resolveProxyClass(interfaces)
    }
}
