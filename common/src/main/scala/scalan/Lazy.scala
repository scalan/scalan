package scalan

class Lazy[A] private (block: => A) {
  @volatile private[this] var _isSet = false

  lazy val value: A = {
    _isSet = true
    block
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
  def apply[A](block: => A): Lazy[A] = new Lazy(block)
}
