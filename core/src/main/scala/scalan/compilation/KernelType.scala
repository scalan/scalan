package scalan.compilation

import com.typesafe.config.ConfigUtil

case class KernelType(name: String, confKey: String)

object KernelType {
  def apply(name: String): KernelType = {
    if (ConfigUtil.joinPath(name) == name)
      KernelType(name, name.toLowerCase)
    else
      throw new IllegalArgumentException(s"${name.toLowerCase} is not a legal unquoted configuration key, supply one explicitly")
  }

  val Scala = KernelType("Scala")
  val Cpp = KernelType("C++", "cpp")
  val Lua = KernelType("Lua")
}
