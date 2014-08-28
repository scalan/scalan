package scalan.util

object ScalaNameUtil {
  def cleanNestedClassName(className: String): String =
    cleanScalaName(className.substring(className.lastIndexOf("$") + 1))

  def cleanScalaName(name: String): String = {
    name.replaceAll("\\$plus", "+").replaceAll("\\$minus", "-").
      replaceAll("\\$times", "*").replaceAll("\\$div", "/").
      replaceAll("\\$colon", ":").replaceAll("\\$greater", ">").
      replaceAll("\\$less", "<").replaceAll("\\$eq", "=").
      replaceAll("\\$bang", "!").replaceAll("\\$at", "@").
      replaceAll("\\$hash", "#").replaceAll("\\$percent", "%").
      replaceAll("\\$amp", "&").replaceAll("\\$tilde", "~").
      replaceAll("\\$qmark", "?").replaceAll("\\$bar", "|").
      replaceAll("\\$bslash", "\\").replaceAll("\\$up", "^")
  }
}
