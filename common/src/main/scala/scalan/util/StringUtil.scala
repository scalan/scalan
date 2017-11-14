package scalan.util

object StringUtil {
  def quote(x: Any) = "\"" + x + "\""

  def lowerCaseFirst(s: String) = if (s.isEmpty) {
    s
  } else {
    s.substring(0, 1).toLowerCase + s.substring(1)
  }

  implicit class StringUtilExtensions(val str: String) extends AnyVal {
    def isNullOrEmpty = str == null || str.isEmpty

    def stripAndTrim = str.stripMargin.stripPrefix("\n").stripPrefix("\r\n").stripLineEnd

    def lastComponent(sep: Char): String = {
      str.substring(str.lastIndexOf(sep) + 1)
    }

    def prefixBefore(substr: String): String = {
      val pos = str.indexOf(substr)
      val res = if (pos == -1) str else str.substring(0, pos)
      res
    }
  }

}
