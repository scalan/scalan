package scalan

import java.util.Properties
import java.io.FileReader

import com.typesafe.scalalogging.slf4j.LazyLogging

import scala.annotation.unchecked.uncheckedVariance

trait Base extends LazyLogging {
  type |[+A, +B] = Either[A, B]
  type L[A] = (A | Unit)
  type R[A] = (Unit | A)
  type Rep[+A]
  type Elem[A]
  type IntRep = Rep[Int]
  type BoolRep = Rep[Boolean]
  type UnitRep = Rep[Unit]
  type ByteRep = Rep[Byte]
  type ShortRep = Rep[Short]
  type CharRep = Rep[Char]
  type LongRep = Rep[Long]
  type FloatRep = Rep[Float]
  type DoubleRep = Rep[Double]
  type :=>[-A, +B] = PartialFunction[A, B]

  class StagingException[A](message: String, val syms: Seq[Rep[_]]) extends RuntimeException(message)

  class ElemException[A](message: String)(implicit val element: Elem[A]) extends StagingException(message, Seq())

  def ??? : Nothing = ???("not implemented")
  def ???(msg: String): Nothing = sys.error(msg)
  def ???(msg: String, syms: Rep[_]*): Nothing = throw new StagingException(msg + " " + syms.mkString, syms)

  def !!! : Nothing = !!!("should not be called")
  def !!!(msg: String): Nothing = sys.error(msg)
  def !!!(msg: String, syms: Rep[_]*): Nothing = throw new StagingException(msg + " " + syms.mkString, syms)

  var isDebug: Boolean = Base.config.getProperty("scalan.debug") != null

  implicit class RepForSomeExtension(x: Rep[_]) {
    def asRep[T]: Rep[T] = x.asInstanceOf[Rep[T]]
  }

  def toRep[A](x: A)(implicit eA: Elem[A]): Rep[A] = !!!(s"Don't know how to create Rep for $x with element $eA")
  implicit def liftToRep[A:Elem](x: A) = toRep(x)

  trait Reifiable[+T] {
    def selfType: Elem[T @uncheckedVariance]
    def self: Rep[T]
  }
}

object Base {
  lazy val config = {
    val prop = new Properties
    try {
      val reader = new FileReader("scalan.properties")
      try {
        prop.load(reader)
      } finally {
        reader.close()
      }
    } catch {
      case _: Throwable => {}
    }
    prop.putAll(System.getProperties)
    prop
  }
}