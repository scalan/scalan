package scalan.primitives

import scalan.Scalan

trait LogicalOps { self: Scalan =>
  val And = new EndoBinOp[Boolean]("&&", _ && _)

  val Or = new EndoBinOp[Boolean]("||", _ || _)

  val Not = new EndoUnOp[Boolean]("!", !_)

  val BooleanToInt = new UnOp[Boolean, Int]("ToInt", if (_) 1 else 0)

  implicit class RepBooleanOps(value: Rep[Boolean]) {
    def &&(y: Rep[Boolean]): Rep[Boolean] = And(value, y)
    def ||(y: Rep[Boolean]): Rep[Boolean] = Or(value, y)
    def unary_!() : Rep[Boolean] = Not(value)
    def toInt: Rep[Int] = BooleanToInt(value)
  }
}
