package scalan.common

import scala.reflect.ClassTag

case class Default[+A](value: A)

trait Defaults {
  def defaultVal[A](z: A): Default[A] = Default[A](z)

  /**
   * Returns the Default element of type A.
   * <p/>
   * For example:
   * <pre>
   *   (defaultOf[Int], defaultOf: List[Int]) == (0, List.empty[Int])
   * </pre>
   * @usecase def default[A]: A
   */
  def defaultOf[A](implicit d: Default[A]): A = d.value
}

object Default extends Defaults {
  val OfAnyRef: Default[AnyRef] = defaultVal(null)

  implicit val OfUnit: Default[Unit] = defaultVal(())

  implicit val OfString: Default[String] = defaultVal("")

  implicit val OfInt: Default[Int] = defaultVal(0)

  implicit val OfBoolean: Default[Boolean] = defaultVal(false)

  implicit val OfChar: Default[Char] = defaultVal(0.toChar)

  implicit val OfByte: Default[Byte] = defaultVal(0.toByte)

  implicit val OfLong: Default[Long] = defaultVal(0L)

  implicit val OfShort: Default[Short] = defaultVal(0.toShort)

  implicit val OfFloat: Default[Float] = defaultVal(0F)

  implicit val OfDouble: Default[Double] = defaultVal(0D)

  implicit val OfBigInteger = defaultVal(java.math.BigInteger.valueOf(0))

  implicit val OfBigInt: Default[BigInt] = defaultVal(BigInt(0))

  implicit def OfOption[A]: Default[Option[A]] = defaultVal(None)

  implicit def OfArray[A: ClassTag]: Default[Array[A]] = defaultVal(new Array[A](0))

  implicit def OfEitherRight[A: Default, B]: Default[Either.RightProjection[A, B]] = defaultVal(Left(defaultOf[A]).right)

  implicit def OfEitherLeft[A, B](implicit bz: Default[B]): Default[Either.LeftProjection[A, B]] = defaultVal(Right(defaultOf[B]).left)

  implicit def OfTuple2[A, B](implicit ad: Default[A], bd: Default[B]): Default[(A, B)] =
    defaultVal((ad.value, bd.value))

  implicit def OfTuple3[A, B, C](implicit ad: Default[A], bd: Default[B], cd: Default[C]): Default[(A, B, C)] =
    defaultVal((ad.value, bd.value, cd.value))

  implicit def OfTuple4[A, B, C, D](implicit ad: Default[A], bd: Default[B], cd: Default[C], dd: Default[D]): Default[(A, B, C, D)] =
    defaultVal((ad.value, bd.value, cd.value, dd.value))

  implicit def OfFunction1[A, B: Default]: Default[A => B] = defaultVal((_: A) => defaultOf[B])

}
