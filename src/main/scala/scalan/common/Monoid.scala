package scalan.common

// definitions shamelessly taken from Scalaz library
/**
 * A categorical monoid.
 *
 * <p>
 * All monoid instances must satisfy the semigroup law and 2 additional laws:
 * <ol>
 * <li><strong>left identity</strong><br/><code>forall a. append(zero, a) == a</code></li>
 * <li><strong>right identity</strong><br/><code>forall a. append(a, zero) == a</code></li>
 * </p>
 */
trait Monoid[M] extends Semigroup[M] {
  val zero: M
}

abstract class MonoidLow {
  def monoid[M](s: Semigroup[M], z: M): Monoid[M] = new Monoid[M] {
    def append(s1: M, s2: => M) = s append (s1, s2)
    val zero = z
    def opName = s.opName
    def isInfix = s.isInfix
  }
}

object Monoid extends MonoidLow {
  import Semigroup._

  implicit val IntPlusMonoid = monoid(Semigroup.IntSemigroup, 0)
  val IntMinMonoid = monoid(Semigroup.IntMinSemigroup, Int.MaxValue)
  val IntMaxMonoid = monoid(Semigroup.IntMaxSemigroup, Int.MinValue)

  implicit val FloatPlusMonoid = monoid(Semigroup.FloatSemigroup, 0f)
  val FloatMultMonoid = monoid(Semigroup.FloatMultSemigroup, 1f)

  implicit val DoublePlusMonoid = monoid(Semigroup.DoubleSemigroup, 0d)
  val DoubleMultMonoid = monoid(Semigroup.DoubleMultSemigroup, 1d)

  //def EitherLeftMonoid[A, B](bz: B): Monoid[Either.LeftProjection[A, B]] = monoid[Either.LeftProjection[A, B]](EitherLeftSemigroup, Right(bz).left)
}
