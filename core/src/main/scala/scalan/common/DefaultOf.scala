package scalan.common

import scala.reflect.ClassTag

trait DefaultOf[+A] {
  val value: A
}

trait Defaults {
  def defaultVal[A](z: A): DefaultOf[A] = new DefaultOf[A] {
    val value = z
  }

  /**
   * Returns the DefaultOf element of type A.
   * <p/>
   * For example:
   * <pre>
   *   (defaultOf[Int], defaultOf: List[Int]) == (0, List.empty[Int])
   * </pre>
   * @usecase def default[A]: A
   */
  def defaultOf[A](implicit d: DefaultOf[A]): A = d.value
}

object Defaults {
  import Common._

  implicit def UnitDefaultOf: DefaultOf[Unit] = defaultVal(())

  implicit def StringDefaultOf: DefaultOf[String] = defaultVal("")

  implicit def IntDefaultOf: DefaultOf[Int] = defaultVal(0)

  implicit def BooleanDefaultOf: DefaultOf[Boolean] = defaultVal(false)

  implicit def CharDefaultOf: DefaultOf[Char] = defaultVal(0.toChar)

  implicit def ByteDefaultOf: DefaultOf[Byte] = defaultVal(0.toByte)

  implicit def LongDefaultOf: DefaultOf[Long] = defaultVal(0L)

  implicit def ShortDefaultOf: DefaultOf[Short] = defaultVal(0.toShort)

  implicit def FloatDefaultOf: DefaultOf[Float] = defaultVal(0F)

  implicit def DoubleDefaultOf: DefaultOf[Double] = defaultVal(0D)

  implicit def BigIntegerDefaultOf = defaultVal(java.math.BigInteger.valueOf(0))

  implicit def BigIntDefaultOf: DefaultOf[BigInt] = defaultVal(BigInt(0))

  implicit def OptionDefaultOf[A]: DefaultOf[Option[A]] = defaultVal(None)

  implicit def ArrayDefaultOf[A: ClassTag]: DefaultOf[Array[A]] = defaultVal(new Array[A](0))

  implicit def EitherRightDefaultOf[A: DefaultOf, B]: DefaultOf[Either.RightProjection[A, B]] = defaultVal(Left(defaultOf[A]).right)

  implicit def EitherLeftDefaultOf[A, B](implicit bz: DefaultOf[B]): DefaultOf[Either.LeftProjection[A, B]] = defaultVal(Right(defaultOf[B]).left)

  implicit def Tuple2DefaultOf[A, B](implicit ad: DefaultOf[A], bd: DefaultOf[B]): DefaultOf[(A, B)] =
    defaultVal((ad.value, bd.value))

  implicit def Tuple3DefaultOf[A, B, C](implicit ad: DefaultOf[A], bd: DefaultOf[B], cd: DefaultOf[C]): DefaultOf[(A, B, C)] =
    defaultVal((ad.value, bd.value, cd.value))

  implicit def Tuple4DefaultOf[A, B, C, D](implicit ad: DefaultOf[A], bd: DefaultOf[B], cd: DefaultOf[C], dd: DefaultOf[D]): DefaultOf[(A, B, C, D)] =
    defaultVal((ad.value, bd.value, cd.value, dd.value))

  implicit def Function1ABDefaultOf[A, B: DefaultOf]: DefaultOf[A => B] = defaultVal((_: A) => defaultOf[B])

}
