package scalan.compilation.language

import scala.Symbol
import scala.reflect.runtime.universe._
import scala.language.postfixOps

case class Library(packageName: String = null, isStatic: Boolean = true) {
  implicit val v = this
}

case class Link(path: String, header: String, isStatic: Boolean)
