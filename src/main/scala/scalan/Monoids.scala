package scalan

import scalan._
import scalan.primitives._
import scalan.common.{Common, Zero, Semigroup, Monoid}
import Common._
import scalan.primitives.NumericOps

trait Monoids extends Base with NumericOps with TypeDescriptors { self: Scalan =>
  import Monoid._

  implicit lazy val IntRepZero: Zero[Rep[Int]] = zero[Rep[Int]](0)
  implicit lazy val FloatRepZero = zero[Rep[Float]](0f)
  implicit lazy val BooleanRepFalse: Zero[Rep[Boolean]] = zero[Rep[Boolean]](false)
  implicit lazy val StringRepZero = zero[Rep[String]]("")
  implicit def arrayRepZero[A:Manifest] = zero[Rep[Array[A]]](Array.empty[A])

  implicit lazy val IntRepOne: Zero[Rep[Int]] = zero[Rep[Int]](1)
  implicit lazy val FloatRepOne: Zero[Rep[Float]] = zero[Rep[Float]](1.0f)
  implicit lazy val BooleanRepTrue: Zero[Rep[Boolean]] = zero[Rep[Boolean]](true)

  implicit lazy val IntRepPlusSemigroup = semigroup[Rep[Int]]("+", (a, b) => numeric_plus(a, b), true)
  implicit lazy val FloatRepPlusSemigroup = semigroup[Rep[Float]]("+", (a, b) => numeric_plus(a, b), true)
  implicit lazy val BooleanRepOrSemigroup = semigroup[Rep[Boolean]]("||", (a, b) => a || b, true)

  lazy val IntRepMultSemigroup = semigroup[Rep[Int]]("*", (a, b) => a * b, true)
  lazy val FloatRepMultSemigroup = semigroup[Rep[Float]]("*", (a, b) => a * b, true)
  lazy val BooleanRepAndSemigroup = semigroup[Rep[Boolean]]("&&", (a, b) => a && b, true)

  implicit lazy val IntRepPlusMonoid: Monoid[Rep[Int]] = monoid[Rep[Int]](IntRepPlusSemigroup, IntRepZero)
  implicit lazy val FloatRepPlusMonoid: Monoid[Rep[Float]] = monoid[Rep[Float]](FloatRepPlusSemigroup, FloatRepZero)
  implicit lazy val BooleanRepOrMonoid: Monoid[Rep[Boolean]] = monoid[Rep[Boolean]](BooleanRepOrSemigroup, BooleanRepFalse)

  lazy val IntRepMultMonoid = monoid(IntRepMultSemigroup, IntRepOne)
  lazy val FloatRepMultMonoid = monoid(FloatRepMultSemigroup, FloatRepOne)
  lazy val BooleanRepAndMonoid = monoid(BooleanRepAndSemigroup, BooleanRepTrue)
  def isPredefined(m: Monoid[_]): Boolean = {
    Set[Any](IntRepMultMonoid,
      IntRepPlusMonoid,
      FloatRepMultMonoid,
      FloatRepPlusMonoid,
      BooleanRepAndMonoid,
      BooleanRepOrMonoid).contains( m)}
}

