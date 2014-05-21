package scalan

import scalan._
import scalan.primitives._
import scalan.primitives.NumericOps

trait Monoids { self: Scalan =>
  class RepMonoid[A](val opName: String, val append: (Rep[A], => Rep[A]) => Rep[A], val zero: Rep[A], val isInfix: Boolean, val isCommutative: Boolean)(implicit eA: Elem[A]) {
    override def toString = s"Monoid[${eA.name}]($opName, $zero)"
  }
  
  object RepMonoid {
    def apply[A](opName: String, append: (Rep[A], => Rep[A]) => Rep[A], zero: A, isInfix: Boolean, isCommutative: Boolean)(implicit eA: Elem[A]): RepMonoid[A] =
      new RepMonoid(opName, append, toRep(zero), isInfix, isCommutative)
  }
  
  implicit lazy val IntRepPlusMonoid: RepMonoid[Int] = 
    RepMonoid("+", (a, b) => a + b, 0, isInfix = true, isCommutative = true)
  implicit lazy val FloatRepPlusMonoid: RepMonoid[Float] = 
    RepMonoid("+", (a, b) => a + b, 0f, isInfix = true, isCommutative = true)
  implicit lazy val DoubleRepPlusMonoid: RepMonoid[Double] = 
    RepMonoid("+", (a, b) => a + b, 0f, isInfix = true, isCommutative = true)
  implicit lazy val BooleanRepOrMonoid: RepMonoid[Boolean] = 
    RepMonoid("||", (a, b) => a || b, false, isInfix = true, isCommutative = true)

  lazy val IntRepMultMonoid = 
    RepMonoid[Int]("*", (a, b) => a * b, 1, isInfix = true, isCommutative = true)
  lazy val FloatRepMultMonoid = 
    RepMonoid[Float]("*", (a, b) => a * b, 1f, isInfix = true, isCommutative = true)
  lazy val BooleanRepAndMonoid = 
    RepMonoid[Boolean]("&&", (a, b) => a && b, true, isInfix = true, isCommutative = true)
  
  def isPredefined(m: RepMonoid[_]): Boolean = {
    Set[RepMonoid[_]](IntRepMultMonoid,
      IntRepPlusMonoid,
      FloatRepMultMonoid,
      FloatRepPlusMonoid,
      BooleanRepAndMonoid,
      BooleanRepOrMonoid).contains(m)}
}
