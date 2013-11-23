package scalan.common

// definitions taken from Scalaz library
/**
 * A Zero in type Z provides the identity element for the operation  { @link scalaz.Semigroup # append }
 * in the corresponding  { @link scalaz.Semigroup } in type Z.
 * <p/>
 * ? a in S, append(a, zero) = a
 */

trait Zero[Z] {
  val zero: Z
}

trait Zeros {
  def zero[Z](z: Z): Zero[Z] = new Zero[Z] {
    val zero = z
  }

  /**
   * Returns the Zero element of type Z.
   * <p/>
   * For example:
   * <pre>
   *   (?[Int], ?: List[Int]) == (0, List.empty[Int])
   * </pre>
   * @usecase def ?[Z]: Z
   */
  def ?[Z](implicit z: Zero[Z]): Z = z.zero

  /**
   * Alias for  { @link scalaz.Zeros # ? }
   */
  def mzero[Z](implicit z: Zero[Z]): Z = z.zero
}

object Zero {
  import Common._

  implicit def UnitZero: Zero[Unit] = zero(())

  implicit def StringZero: Zero[String] = zero("")

  implicit def IntZero: Zero[Int] = zero(0)
  def IntMinZero: Zero[Int] = zero(0)
  def IntMaxZero: Zero[Int] = zero(0)

  implicit def BooleanZero: Zero[Boolean] = zero(false)

  implicit def CharZero: Zero[Char] = zero(0.toChar)

  implicit def ByteZero: Zero[Byte] = zero(0.toByte)

  implicit def LongZero: Zero[Long] = zero(0L)

  implicit def ShortZero: Zero[Short] = zero(0.toShort)

  implicit def FloatZero: Zero[Float] = zero(0F)

  implicit def DoubleZero: Zero[Double] = zero(0D)

  implicit def BigIntegerZero = zero(java.math.BigInteger.valueOf(0))

  implicit def BigIntZero: Zero[BigInt] = zero(BigInt(0))

  implicit def OptionZero[A]: Zero[Option[A]] = zero(None)

  implicit def ArrayZero[A](implicit m: Manifest[A]): Zero[Array[A]] = zero(new Array[A](0))

  implicit def EitherRightZero[A: Zero, B]: Zero[Either.RightProjection[A, B]] = zero(Left(mzero[A]).right)

  implicit def EitherLeftZero[A, B](implicit bz: Zero[B]): Zero[Either.LeftProjection[A, B]] = zero(Right(mzero[B]).left)

  implicit def Tuple2Zero[A, B](implicit az: Zero[A], bz: Zero[B]): Zero[(A, B)] =
    zero((az.zero, bz.zero))

  implicit def Tuple3Zero[A, B, C](implicit az: Zero[A], bz: Zero[B], cz: Zero[C]): Zero[(A, B, C)] =
    zero((az.zero, bz.zero, cz.zero))

  implicit def Tuple4Zero[A, B, C, D](implicit az: Zero[A], bz: Zero[B], cz: Zero[C], dz: Zero[D]): Zero[(A, B, C, D)] =
    zero((az.zero, bz.zero, cz.zero, dz.zero))

  implicit def Function1ABZero[A, B: Zero]: Zero[A => B] = zero((_: A) => mzero[B])

}
