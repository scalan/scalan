package scalan.util

object StringUtil {
  def quote(x: Any) = "\"" + x + "\""

  def lowerCaseFirst(s: String) = if (s.isEmpty) {
    s
  } else {
    s.substring(0, 1).toLowerCase + s.substring(1)
  }
}
