package scalan.common

trait StringE extends PimpedType[String] {
  def replicate(n: Int): String = (1 to n).foldLeft("")((a: String, _: Int) => a + value)
}

object Strings {
  implicit class StringToOps(ss: String) extends StringE {
    val value = ss
  }
}
