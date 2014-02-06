package scalan.common

/**
 * Author: Alexander Slesarenko
 * Date: 7/19/12
 */

class Lazy[A] private (expr: => A) {
  private var _value: A = null.asInstanceOf[A]

  def value: A =
    if (_value != null) _value
    else {
      _value = expr
      _value
    }
}

object Lazy {
  def apply[A](expr: => A): Lazy[A] = new Lazy(expr)
}

