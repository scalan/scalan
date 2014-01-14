package scalan

import scalan._
import scalan.primitives._
import scalan.common.{Common, Semigroup, Monoid}
import Common._
import scalan.primitives.NumericOps

trait Monoids extends Semigroups { self: Scalan =>
  import Semigroup._
  import Monoid._
  
//  implicit def repMonoid[T](implicit m: Monoid[T]): Monoid[Rep[T]] = {
//    val repSemigroups
//  }

  implicit lazy val IntRepPlusMonoid: Monoid[Rep[Int]] = monoid[Rep[Int]](IntRepPlusSemigroup, 0)
  implicit lazy val FloatRepPlusMonoid: Monoid[Rep[Float]] = monoid[Rep[Float]](FloatRepPlusSemigroup, 0f)
  implicit lazy val BooleanRepOrMonoid: Monoid[Rep[Boolean]] = monoid[Rep[Boolean]](BooleanRepOrSemigroup, false)

  lazy val IntRepMultMonoid = monoid[Rep[Int]](IntRepMultSemigroup, 1)
  lazy val FloatRepMultMonoid = monoid[Rep[Float]](FloatRepMultSemigroup, 1f)
  lazy val BooleanRepAndMonoid = monoid[Rep[Boolean]](BooleanRepAndSemigroup, true)
  
  def isPredefined(m: Monoid[_]): Boolean = {
    Set[Any](IntRepMultMonoid,
      IntRepPlusMonoid,
      FloatRepMultMonoid,
      FloatRepPlusMonoid,
      BooleanRepAndMonoid,
      BooleanRepOrMonoid).contains( m)}
}

