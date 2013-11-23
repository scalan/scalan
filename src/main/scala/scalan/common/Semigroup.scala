package scalan.common

import collection.mutable.ArraySeq

// definitions taken from Scalaz library
/**
 * A Semigroup in type S must satisfy two laws:
 * <ol>
 * <li>
 * Closure: ? a, b in S, append(a, b) is also in S. This is enforced by the type system.
 * </li>
 * <li>
 * Associativity: ? a, b and c in S, the equation append(append(a, b), c) = append(a, append(b , c)) holds.
 * </li>
 * </ol>
 * @see scalaz.Identity#?
 */
trait Semigroup[S] {
  def append(s1: S, s2: => S): S

  def opName: String
  def isInfix: Boolean
}

trait Semigroups {
  def semigroup[S](name: String,  f: (S, => S) => S, infix: Boolean = true): Semigroup[S] = new Semigroup[S] {
    def append(s1: S, s2: => S) = f(s1, s2)
    def opName = name
    def isInfix = infix
  }
}

trait SemigroupLow {
}

object Semigroup extends SemigroupLow {
  import Common._

  implicit def UnitSemigroup: Semigroup[Unit] = semigroup("", (_, _) => ())

  implicit def StringSemigroup: Semigroup[String] = semigroup("+", _ + _)

  implicit def IntSemigroup: Semigroup[Int] = semigroup("+", _ + _)
  def IntMinSemigroup: Semigroup[Int] = {val o = implicitly[Ordering[Int]];  semigroup("min", o.min(_, _), false)}
  def IntMaxSemigroup: Semigroup[Int] = {val o = implicitly[Ordering[Int]];  semigroup("max", o.max(_, _), false)}

  implicit def BooleanSemigroup: Semigroup[Boolean] = semigroup("||", (a, b) => (a || b))

  implicit def CharSemigroup: Semigroup[Char] = semigroup("+", (a, b) => (a + b).toChar)
//
//  implicit def ByteSemigroup: Semigroup[Byte] = semigroup((a, b) => (a + b).toByte)
//
//  implicit def LongSemigroup: Semigroup[Long] = semigroup((a, b) => (a + b).toLong)
//
//  implicit def ShortSemigroup: Semigroup[Short] = semigroup((a, b) => (a + b).toShort)

  implicit def FloatSemigroup: Semigroup[Float] = semigroup("+", (a, b) => (a + b))
  implicit def DoubleSemigroup: Semigroup[Double] = semigroup("+", (a, b) => (a + b))
  def FloatMultSemigroup: Semigroup[Float] = semigroup("*", (a, b) => (a * b))
  def DoubleMultSemigroup: Semigroup[Double] = semigroup("*", (a, b) => (a * b))

  implicit def ArraySemigroup[A: Manifest]: Semigroup[Array[A]] = semigroup("concat", Array.concat(_, _), false)

}
