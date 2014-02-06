package scalan

import scalan.primitives.NumericOps
import scalan.common.Semigroup
import scalan.common.Semigroup._

trait Semigroups extends Base with NumericOps with Elems { self: Scalan =>
  def repSemigroup[T](repOp: (Rep[T], => Rep[T]) => Rep[T], s: Semigroup[T]): Semigroup[Rep[T]] = semigroup[Rep[T]](s.opName, repOp, s.isInfix)

  implicit lazy val IntRepPlusSemigroup = repSemigroup[Int]((a, b) => a + b, IntSemigroup)
  implicit lazy val FloatRepPlusSemigroup = repSemigroup[Float]((a, b) => numeric_plus(a, b), FloatSemigroup)
  implicit lazy val BooleanRepOrSemigroup = repSemigroup[Boolean]((a, b) => a || b, BooleanOrSemigroup)

  lazy val IntRepMultSemigroup = repSemigroup[Int]((a, b) => a * b, IntMultSemigroup)
  lazy val FloatRepMultSemigroup = repSemigroup[Float]((a, b) => a * b, FloatMultSemigroup)
  lazy val BooleanRepAndSemigroup = repSemigroup[Boolean]((a, b) => a && b, BooleanAndSemigroup)
}