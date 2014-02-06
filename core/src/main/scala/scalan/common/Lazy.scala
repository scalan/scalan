package scalan.common

/**
 * Author: Alexander Slesarenko
 * Date: 7/19/12
 */

class Lazy[A] private (expr: => A) {
  @volatile private[this] var _isSet = false
  
  lazy val value: A = {
    _isSet = true
    expr
  }
  
  def isSet = _isSet
  
  override def toString = {
    if (!_isSet)
      "<lazy>"
    else
      value.toString
  }
}

object Lazy {
  def apply[A](expr: => A): Lazy[A] = new Lazy(expr)
}
