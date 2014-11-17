package scalan.primitives

import scalan.Scalan

trait MathOps { self: Scalan =>
  object Math {
    def ceil(x: Rep[Double]) = MathCeil(x)
    def floor(x: Rep[Double]) = MathFloor(x)
    def exp(x: Rep[Double]) = MathExp(x)
    def log(x: Rep[Double]) = MathLog(x)
    def sqrt(x: Rep[Double]) = MathSqrt(x)
    def sin(x: Rep[Double]) = MathSin(x)
    def cos(x: Rep[Double]) = MathCos(x)
    def acos(x: Rep[Double]) = MathAcos(x)
    def atan(x: Rep[Double]) = MathAtan(x)
    def atan2(x: Rep[Double], y: Rep[Double]) = MathAtan2(x,y)
    def pow(x: Rep[Double], y: Rep[Double]) = MathPow(x,y)
    def tanh(x: Rep[Double]) = MathTanh(x)
    def abs[A](x: Rep[A])(implicit e: Elem[A], n:Numeric[A]) = MathAbs(n).apply(x)
    def max[A](x: Rep[A], y: Rep[A])(implicit e: Elem[A], n:Ordering[A]) = x.max(y)
    def min[A](x: Rep[A], y: Rep[A])(implicit e: Elem[A], n:Ordering[A]) = x.min(y)
    val Pi = toRep(math.Pi)
    val E = toRep(math.E)
  }

  type UnDoubleOp = UnOp[Double, Double]

  type BinDoubleOp = BinOp[Double, Double]

  val MathCeil = new UnDoubleOp("Ceil", scala.math.ceil)

  val MathFloor = new UnDoubleOp("Floor", scala.math.floor)

  val MathExp = new UnDoubleOp("Exp", scala.math.exp)

  val MathLog = new UnDoubleOp("Log", scala.math.log)

  val MathSqrt = new UnDoubleOp("Sqrt", scala.math.sqrt)

  val MathSin = new UnDoubleOp("Sin", scala.math.sin)

  val MathCos = new UnDoubleOp("Cos", scala.math.cos)

  val MathAcos = new UnDoubleOp("Acos", scala.math.acos)

  val MathAtan = new UnDoubleOp("Atan", scala.math.atan)

  val MathAtan2 = new BinDoubleOp("Atan2", scala.math.atan2)

  val MathPow = new BinDoubleOp("Pow", scala.math.pow)

  val MathTanh = new UnDoubleOp("Tanh", scala.math.tanh)

  case class MathAbs[T: Elem](n: Numeric[T]) extends UnOp[T, T]("Abs", n.abs)
}
