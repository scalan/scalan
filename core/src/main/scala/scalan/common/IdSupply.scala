package scalan.common

object IdSupply {
  private var _nextId = 0

  def nextId = {
    _nextId += 1
    _nextId
  }
}
