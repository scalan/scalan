package scalan.util

object ScalaNameUtil {
  val opChars = Set('+', '-', '*', '/', ':', '>', '<', '=', '!', '@', '#', '%',
    '&', '~', '?', '|', '\\', '^')
  
  def cleanNestedClassName(className: String): String =
    cleanScalaName(className.substring(className.lastIndexOf("$") + 1))

  def cleanScalaName(name: String): String = {
    name.replace("$plus", "+").replace("$minus", "-").
      replace("$times", "*").replace("$div", "/").
      replace("$colon", ":").replace("$greater", ">").
      replace("$less", "<").replace("$eq", "=").
      replace("$bang", "!").replace("$at", "@").
      replace("$hash", "#").replace("$percent", "%").
      replace("$amp", "&").replace("$tilde", "~").
      replace("$qmark", "?").replace("$bar", "|").
      replace("$bslash", "\\").replace("$up", "^")
  }
  
  def isOpName(name: String) = !name.isEmpty && opChars.contains(name.last)

  object PackageAndName {
    def unapply(name: String): Option[(List[String], String)] = {
      val parts = name.split('.')
      if (parts.length > 1)
        Some((parts.slice(0, parts.length - 1).toList, parts.last))
      else
        None
    }
  }

}
