package scalan

import java.lang.reflect.Field
import java.util.regex.Pattern

import scala.annotation.tailrec
import scala.util.control.NonFatal

/**
 * To collect debugging data, run with -Dscalan.debug=true or set scalan.debug to `true`
 * in `application.conf`. To print it, call `printDebugData()`. Debug data is everything
 * stored in fields whose names start with (or contain) "debug$".
 */
trait Debugging { self: Scalan =>
  var isDebug: Boolean = config.getBoolean("debug")

  private lazy val fields = {
    val buffer = scala.collection.mutable.ArrayBuffer.empty[Field]

    @tailrec
    def collectFields(clazz: Class[_]): Unit = if (clazz != null) {
      buffer ++= clazz.getDeclaredFields.filter(_.getName.contains("debug$"))
      collectFields(clazz.getSuperclass)
    }

    collectFields(self.getClass)

    buffer.toSeq
  }

  /**
   * Print all collected debug data.
   */
  def printDebugData(cond: String => Boolean): Unit = {
    if (isDebug) {
      println("Debug data:")
      fields.foreach { f =>
        val name = f.getName.substring(f.getName.indexOf("debug$") + "debug$".length)
        if (cond(name)) {
          try {
            f.setAccessible(true)
            println(s"$name: ${f.get(self)}")
          } catch {
            case NonFatal(e) => println(s"Failed to access field ${f.getName}")
          }
        }
      }
      println()
    }
  }

  def printDebugData(): Unit = printDebugData(_ => true)

  def printDebugData(nameRegexes: String*): Unit = printDebugData {
    name => nameRegexes.exists(Pattern.matches(_, name))
  }

  def counter[A] = collection.mutable.Map.empty[A, Int].withDefaultValue(0)
}
